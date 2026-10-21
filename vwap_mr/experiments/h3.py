"""H3 runner: regime predictability, the conditional mean difference (U10; PLAN.md §6.4, §7 U10).

`run_h3(path)` mirrors `runner.run_spec`: it refuses an invalid, uncommitted or dirty spec, uncommitted code and a
spec already in the ledger. It records the look (label `U10 run <id>@<sha12>`, n = headlines + variants) before
computing, writes `results/cells/<hash>/` per gate and the run record, and appends the ledger rows last.

Streams and timing:
- r = the H1 headline stream (R₀ on SPY, the registered cost; row t = session t's net excess return). On a
  registered run it is checked against H1's ledger row before anything else is computed.
- States come from the spliced long series (`features.state.long_inputs`), so every window is full on 2016-01-04.
  Regime columns come from `strategies.gates.regime_frame`, and gates from the spec's `gates` map. Gate row t is
  as of session t's close decision.
- Δμ conditions return row t on gate row t − 1: the state was known at the decision before the return's first
  leg (the overnight gap) began. Sessions with no gate are dropped and counted.
- `cond_mean_test`: a circular block bootstrap of the session index (block 21), Newey–West t alongside.

Per gate it reports: Δμ, μ_in, μ_out, p, the one-sided 90 % lower bound, π realized vs registered, episodes, and
the MDE at the registered π (0.5 where π is only reported). The economic version is the backtested gated stream
(target × gate, flat off-gate, the registered costs): its paired Ledoit–Wolf Sharpe difference vs the ungated
stream, and its return scaled to the ungated volatility.

The verdict is Holm over the headlines (≤ 4, one-sided). A headline passes iff its Holm p < alpha, its in-gate mean
> the floor, and π lies within the tolerance (gates with a registered π only). A gate that cannot be tested (on or
off on < 2 sessions) keeps p = 1 in the Holm step and is flagged `untestable`, never dropped.

Also reported: the incremental Newey–West regression of r on the baselines' gates plus each listed gate's; and,
from the refit logs over the window, R1 / R2's convergence, degenerate-refit and swap-flag counts and R2's φ
separation (U9: R2 invents regimes on regime-free data)."""

import json
import time
from collections.abc import Callable
from dataclasses import replace
from datetime import UTC, datetime, timedelta
from pathlib import Path

import numpy as np
import pandas as pd

from vwap_mr.backtest.costs import CostModel, tbill_yield
from vwap_mr.backtest.engine import run
from vwap_mr.backtest.market import Market, load_market
from vwap_mr.backtest.metrics import ANN, summary
from vwap_mr.config import ROOT, SPECS_DIR, DecisionTime
from vwap_mr.experiments.ledger import Ledger, clean
from vwap_mr.experiments.looks import LOOKS_PATH, check_window, looks, looks_total, record_look
from vwap_mr.experiments.report import ReportRefused, _f, _p, _table
from vwap_mr.experiments.runner import CELLS_DIR, LEDGER_PATH, RunError, _hash, _rel
from vwap_mr.experiments.spec import Spec, SpecError, check_clean, code_hash, load_spec, registered_sha
from vwap_mr.features.state import REGISTERED, StateConfig, state_frame
from vwap_mr.stats.bootstrap import cond_mean_test, newey_west_ols, sharpe_test
from vwap_mr.stats.multiplicity import holm
from vwap_mr.stats.power import mde_cond_diff
from vwap_mr.strategies.gates import Gated, gate_frame, regime_frame
from vwap_mr.strategies.vwap_mr import r0

H1_SPEC = SPECS_DIR / "H1_vwap_mr.yaml"
Inputs = Callable[[Spec], tuple[Market, pd.DataFrame, pd.Series]]


def load_inputs(spec: Spec) -> tuple[Market, pd.DataFrame, pd.Series]:
    """The headline market on the spec's window; the spliced long frame and VIX for the states."""
    from vwap_mr.features.state import long_inputs

    w, sym = spec["window"], spec["instruments"]["headline"]
    m = load_market(sym, w["start"], w["end"] + timedelta(days=1), vintage=spec["vintage"])
    return (m, *long_inputs(sym, spec["vintage"]))


def synthetic_inputs(
    spec: Spec, seed: int = 7, start: str = "1993-01-29"
) -> tuple[Market, pd.DataFrame, pd.Series]:
    """Dry runs: one seeded synthetic series; the headline market is its tail on the spec's window."""
    from vwap_mr.data.synthetic import daily_market

    w = spec["window"]
    long = daily_market(spec["instruments"]["headline"], start, str(w["end"]), seed)
    keep = long.index >= pd.Timestamp(w["start"], tz=long.index.tz)
    m = Market(long.sessions[keep], "synthetic", exo=long.exo, symbol=long.symbol)
    assert long.exo is not None
    vix = pd.Series(long.exo.asof("VIX", long.decision_instants(DecisionTime.CLOSE)), index=long.index)
    return m, long.sessions, vix


def check_h1_stream(mean_bp_day: float, ledger_rows: list[dict]) -> None:
    rows = [r for r in ledger_rows if r.get("spec_id") == "H1_vwap_mr" and r.get("name") == "R0"]
    if len(rows) != 1 or abs(rows[0]["mean_bp_day"] - mean_bp_day) > 1e-9:
        raise RunError(f"rebuilt H1 stream {mean_bp_day} bp/day does not match the H1 ledger row {rows}")


def refit_summary(log: list[dict], index: pd.DatetimeIndex, start) -> dict:
    """The refits that produce the window's gates: every refit at or after the window's first session, and the last
    one before it (its block covers the first sessions)."""
    pos = int((index < pd.Timestamp(start, tz=index.tz)).sum())
    fits = [r for r in log if "skipped" not in r]
    before = [r for r in fits if r["session"] < pos]
    fits = before[-1:] + [r for r in fits if r["session"] >= pos]
    sep = [r["stat"][1] - r["stat"][0] for r in fits]
    return {"refits": len(fits), "not_converged": sum(not r["converged"] for r in fits),
            "degenerate": sum(r["degenerate"] for r in fits), "swap_flag": sum(r["swap_flag"] for r in fits),
            "skipped": sum("skipped" in r for r in log if r["session"] >= pos),
            "stat_lo_median": float(np.median([r["stat"][0] for r in fits])) if fits else None,
            "stat_hi_median": float(np.median([r["stat"][1] for r in fits])) if fits else None,
            "sep_median": float(np.median(sep)) if fits else None,
            "sep_q10": float(np.quantile(sep, 0.1)) if fits else None}  # fmt: skip


def _validate_h3(spec: Spec) -> None:
    gates, names = spec.data.get("gates"), [*spec["headlines"], *spec["variants"]]
    if not isinstance(gates, dict) or [n for n in names if n not in gates]:
        raise SpecError("an H3 spec needs a `gates` entry for every headline and variant")
    for k in ("in_gate_mean_min", "pi_tolerance"):
        if not isinstance(spec["floors"].get(k), int | float):
            raise SpecError(f"floors.{k} must be a number")
    for k in ("state_configuration", "regime_configuration", "incremental"):
        if k not in spec.data:
            raise SpecError(f"an H3 spec needs `{k}`")


def run_h3(
    path: Path | str, *, inputs: Inputs = load_inputs, ledger_path: Path = LEDGER_PATH,
    looks_path: Path = LOOKS_PATH, cells_dir: Path = CELLS_DIR, repo: Path = ROOT, require_registered: bool = True,
    state_cfg: StateConfig = REGISTERED, data_hash: Callable[[str], str] | None = None,
) -> dict:  # fmt: skip
    t0 = time.time()
    spec = load_spec(path)
    if spec["test"]["kind"] != "cond_mean_diff":
        raise SpecError(f"run_h3 runs cond_mean_diff specs, not {spec['test']['kind']}")
    _validate_h3(spec)
    if not require_registered and {Path(ledger_path), Path(looks_path), Path(cells_dir)} & {
        LEDGER_PATH, LOOKS_PATH, CELLS_DIR}:  # fmt: skip
        raise RunError(
            "an unregistered run must write to scratch paths, never results/ (it would consume the spec)"
        )
    if require_registered:
        spec_sha, git_sha = registered_sha(spec.path, repo), check_clean(repo)
    else:
        spec_sha = git_sha = "unregistered"
    ledger = Ledger(ledger_path)
    with ledger.run_lock():
        prior_rows = ledger.rows()
        if any(r.get("spec_id") == spec.id for r in prior_rows if "cell_hash" in r):
            raise RunError(
                f"{spec.id} has already run (ledger {ledger_path}); an amendment is a new spec file"
            )
        heads, variants = list(spec["headlines"]), list(spec["variants"])
        names = heads + variants
        if len(names) > spec["budget"]:
            raise SpecError("over budget")
        market, frame, vix = inputs(spec)
        check_window(market.index, "dev")
        if not require_registered and market.source != "synthetic":
            raise RunError("an unregistered run may read synthetic markets only")
        if data_hash is None:
            from vwap_mr.data.store import verify_vintage, vintage_dir

            dh = verify_vintage(vintage_dir(spec["vintage"])) if require_registered else "synthetic"
        else:
            dh = data_hash(spec["vintage"])
        look = record_look(f"U10 run {spec.id}@{spec_sha[:12]}", len(names),
                           f"{spec.id}: {len(heads)} headline gates (Holm), {len(variants)} variant gates",
                           looks_path, window="dev")  # fmt: skip

        c, t = spec["cost"], spec["test"]
        costs = CostModel(round_trip_bp=c["registered_bp"], borrow_rate=c["borrow_rate"], fees=c["fees"])
        y = tbill_yield(market)
        base = run(market, r0(), costs=costs, cash_yield=y)
        r = base.excess.iloc[1:]
        if require_registered:
            check_h1_stream(float(r.mean() * 1e4), prior_rows)
        rc, sc = spec["regime_configuration"], spec["state_configuration"]
        state = state_frame(frame, vix=vix, net=base.net, cfg=state_cfg)
        reg, logs = regime_frame(frame, rc)
        G = gate_frame(state.join(reg), {n: spec["gates"][n] for n in names}, sc["trailing_quantile_window"])
        G = G.reindex(market.index)
        g_prev = G.shift(1).iloc[1:]  # row t: the gate known at decision t − 1
        gm = replace(market, state=G)
        kw = {"block": t["block"], "n_boot": t["n_boot"], "seed": t["seed"], "sided": t["sided"]}
        sd_r, fl, pw = float(r.std(ddof=1)), spec["floors"], spec["power"]
        ch = code_hash()
        rows, out = [], {}
        for name in names:
            gs, is_head = spec["gates"][name], name in heads
            gi = g_prev[name]
            ok = gi.notna()
            rec: dict = {"name": name, "kind": "headline" if is_head else "variant", "gate": gs,
                         "n_no_gate": int((~ok).sum()), "n_obs": int(ok.sum()), "status": "ok"}  # fmt: skip
            try:
                ct = cond_mean_test(r[ok].to_numpy(), gi[ok].to_numpy() == 1.0, alpha=0.10, **kw)
                rec |= {k: ct[k] for k in ("delta", "mu_in", "mu_out", "pi", "episodes", "episode_median", "p",
                                           "t", "t_nw", "se", "n_invalid")}  # fmt: skip
                rec |= {"delta_bp_day": ct["delta"] * 1e4, "mu_in_bp_day": ct["mu_in"] * 1e4,
                        "mu_out_bp_day": ct["mu_out"] * 1e4, "lower90_bp_day": ct["ci"][0] * 1e4}  # fmt: skip
            except ValueError as e:
                rec |= {"status": "no_fit", "error": str(e), "p": None, "pi": float(gi[ok].mean()) if ok.any()
                        else None}  # fmt: skip
            pi_reg = gs.get("pi")
            mde = mde_cond_diff(max(int(ok.sum()), 2), sd_r if sd_r > 0 else pw["sigma_d_bp"] / 1e4,
                                pi_reg if isinstance(pi_reg, float) else 0.5, k=len(heads) if is_head else 1)  # fmt: skip
            rec["mde_bp_day"] = mde["mde_bp_day"]
            rec["floors"] = {
                "in_gate_mean": rec.get("mu_in_bp_day") is not None and rec["mu_in_bp_day"] > fl["in_gate_mean_min"],
                "pi_drift": None if not isinstance(pi_reg, float) or rec["pi"] is None
                else abs(rec["pi"] - pi_reg) <= fl["pi_tolerance"],
            }  # fmt: skip
            gated = run(gm, Gated(r0(), name), costs=costs, cash_yield=y)
            gx = gated.excess.iloc[1:]
            s = summary(gated)
            rec["gated"] = {k: s[k] for k in ("mean_bp_day", "ret_ann", "vol_ann", "sharpe", "max_dd",
                                              "round_trips_yr", "flat_share", "cost_bp_day")}  # fmt: skip
            if np.array_equal(gx.to_numpy(), r.to_numpy()):
                rec["gated"]["d_sharpe_note"] = "identical to the ungated stream (gate always on)"
            elif gx.std() > 0:
                st = sharpe_test(gx.to_numpy(), r.to_numpy(), **kw)
                rec["gated"] |= {"d_sharpe_ann": st["delta_ann"], "d_sharpe_p": st["p"],
                                 "ret_ann_volmatched": float(gx.mean() * ANN * sd_r / gx.std(ddof=1))}  # fmt: skip
            h = _hash(spec.id, spec_sha, name, market.symbol, costs.round_trip_bp, ch, dh)
            d = Path(cells_dir) / h
            d.mkdir(parents=True, exist_ok=True)
            pd.DataFrame({"r": r, "gate_prev": gi, "gated_excess": gx}).to_parquet(d / "daily.parquet")
            (d / "result.json").write_text(json.dumps(clean(rec), indent=1, sort_keys=True, allow_nan=False))
            rec["cell_hash"] = h
            out[name] = rec

        p_heads = [out[h]["p"] if out[h]["p"] is not None else 1.0 for h in heads]
        for h, ph in zip(heads, holm(p_heads), strict=True):
            o = out[h]
            o["p_holm"] = float(ph)
            o["untestable"] = o["status"] != "ok"
            o["passes"] = bool(not o["untestable"] and ph < t["alpha"] and o["floors"]["in_gate_mean"]
                               and o["floors"]["pi_drift"] is not False)  # fmt: skip
        for name, o in out.items():
            rows.append({"cell_hash": o["cell_hash"], "spec_id": spec.id, "spec_sha": spec_sha, "kind": o["kind"],
                         "name": name, "strategy": f"R0|{name}", "symbol": market.symbol, "git_sha": git_sha,
                         "code_hash": ch, "data_hash": dh, "vintage": spec["vintage"], "started_at": look["at"],
                         "runtime_s": round(time.time() - t0, 1), "n_obs": o["n_obs"],
                         "delta_bp_day": o.get("delta_bp_day"), "p_boot": o["p"], "p_holm": o.get("p_holm"),
                         "t_nw": o.get("t_nw"), "pi": o["pi"], "mean_bp_day": o["gated"]["mean_bp_day"],
                         "sharpe": o["gated"]["sharpe"], "cost_bp": costs.round_trip_bp, "status": o["status"],
                         "error_path": None})  # fmt: skip

        inc = []
        b_names = spec["incremental"]["baselines"]
        for x in spec["incremental"]["gates"]:
            Z = g_prev[[*b_names, x]]
            okz = Z.notna().all(axis=1)
            try:
                nw = newey_west_ols(r[okz].to_numpy(), Z[okz].to_numpy(float), 5)
                inc.append({"gate": x, "n": int(okz.sum()), "beta_bp_day": float(nw["beta"][-1] * 1e4),
                            "t_nw": float(nw["t"][-1])})  # fmt: skip
            except np.linalg.LinAlgError as e:
                inc.append({"gate": x, "n": int(okz.sum()), "error": f"singular design ({e})"})
        refits = {
            k: refit_summary(v, pd.DatetimeIndex(frame.index), spec["window"]["start"])
            for k, v in logs.items()
        }
        holds = heads and all(out[h]["passes"] for h in heads)
        record = clean({
            "spec_id": spec.id, "spec_path": _rel(spec.path, repo), "spec_sha": spec_sha, "git_sha": git_sha,
            "code_hash": ch, "data_hash": dh, "vintage": spec["vintage"],
            "spec": json.loads(json.dumps(spec.data, default=str)), "look": look, "k_at_look": looks_total(looks_path),
            "ungated": summary(base) | {"n_obs": len(r)}, "gates": out, "incremental": inc, "refits": refits,
            "n_pass": sum(out[h]["passes"] for h in heads), "all_pass": holds,
            "finished_at": datetime.now(UTC).isoformat(), "runtime_s": round(time.time() - t0, 1),
        })  # fmt: skip
        rp = Path(cells_dir) / f"run_{spec.id}_{spec_sha[:12]}.json"
        rp.parent.mkdir(parents=True, exist_ok=True)
        rp.write_text(json.dumps(record, indent=1, sort_keys=True, allow_nan=False))
        ledger.append(
            rows
        )  # last: a crash before this leaves the look but no trial rows (the spec can still run)
        record["record_path"] = rp.as_posix()
        return record


# ── report ──────────────────────────────────────────────────────────────────


def _gate_row(o: dict, k: int) -> list:
    pi_reg = o["gate"].get("pi")
    return [o["name"], o["n_obs"], o["n_no_gate"], _f(o.get("delta_bp_day"), sign=True),
            _f(o.get("lower90_bp_day"), sign=True), _f(o.get("mu_in_bp_day"), sign=True),
            _f(o.get("mu_out_bp_day"), sign=True), _p(o["p"], k) if o["p"] is not None else o.get("error", "—"),
            _f(o.get("t_nw")), f"{_f(o['pi'], 3)} / {pi_reg if isinstance(pi_reg, float) else 'rep.'}",
            o.get("episodes", "—"), _f(o.get("episode_median"), 0), _f(o["mde_bp_day"], 1)]  # fmt: skip


def _floors(f: dict) -> str:
    return f"{f['in_gate_mean']}; {'n/a' if f['pi_drift'] is None else f['pi_drift']}"


GATE_HEADER = ["gate", "n", "no gate", "Δμ bp/day", "90 % lower", "μ in", "μ out", "p one-sided (p·K)", "t NW",
               "π realized / registered", "episodes", "median length", "MDE"]  # fmt: skip


def render_h3(record: dict, looks_path: Path = LOOKS_PATH) -> str:
    look = record["look"]
    if not any(r.get("at") == look["at"] and r.get("label") == look["label"] and r.get("n") == look["n"]
               for r in looks(looks_path)):  # fmt: skip
        raise ReportRefused(
            f"no look row {look['label']!r} at {look['at']} in {looks_path}: record the look first"
        )
    k = looks_total(looks_path)
    spec, g, u = record["spec"], record["gates"], record["ungated"]
    heads = spec["headlines"]
    L = [f"# {record['spec_id']} — H3 regime predictability (PLAN.md §6.4)", ""]
    L += [(f"Spec `{record['spec_path']}` @ `{record['spec_sha'][:12]}`; code `{record['git_sha'][:12]}` (hash "
           f"`{record['code_hash'][:12]}`); data vintage `{record['vintage']}` (`{record['data_hash'][:12]}`). Window "
           f"{spec['window']['start']} → {spec['window']['end']}, {spec['instruments']['headline']}, R₀ at "
           f"{spec['cost']['registered_bp']} bp, {spec['execution']['decision']} → {spec['execution']['fill']}. Look "
           f"`{look['label']}` (n {look['n']}, {look['at']}). **K = {k}** (every window); p·K beside every p."), "",
          ("**Registered as a diagnostic** (§11, 2026-10-21): H1 was rejected on this window and its stream was "
           f"already examined (U6, U7); the expected effect, {spec['expected']['magnitude_bp_day']} bp/day, is below "
           "every headline's MDE. A rejection says little; a pass would need the holdout (U12) and the forward test "
           "(U13) to count."), ""]  # fmt: skip
    L += ["## 1. Headline gates (Holm over the headlines, one-sided H₁: Δμ > 0)", "",
          _table(GATE_HEADER + ["Holm p", "floors (μ in > 0; π drift)", "passes"],
                 [_gate_row(g[h], k) + [_f(g[h]["p_holm"], 4), _floors(g[h]["floors"]), g[h]["passes"]]
                  for h in heads]), ""]  # fmt: skip
    L += [(f"**Verdict: {record['n_pass']} of {len(heads)} headline gates pass** — diagnostic (pass: Holm p < "
           f"{spec['test']['alpha']}, in-gate mean > {spec['floors']['in_gate_mean_min']}, and π within ± "
           f"{spec['floors']['pi_tolerance']} of its registered value where one is registered). Ungated R₀: "
           f"{_f(u['mean_bp_day'], 2, True)} bp/day net excess over {u['n_obs']} sessions. Return row t is "
           "conditioned on the gate at decision t − 1; MDE at the registered π (0.5 where π is only reported; Holm "
           f"k = {len(heads)} for headlines), with the ungated stream's σ."), ""]  # fmt: skip
    untest = [h for h in heads if g[h]["untestable"]]
    if untest:
        L += [f"Untestable headlines (kept at p = 1 in the Holm step): {', '.join(untest)}.", ""]
    L += [
        "## 2. Variant gates (reported, not in the Holm step; E1–E4 under-powered, U8)",
        "",
        _table(GATE_HEADER, [_gate_row(g[v], k) for v in spec["variants"]]),
        "",
    ]
    L += ["## 3. Economic version: the gated stream (target × gate, flat off-gate, registered costs)", "",
          _table(["gate", "bp/day", "%/yr", "vol %/yr", "%/yr at ungated vol", "Sharpe", "ΔSharpe vs ungated (LW)",
                  "p one-sided (p·K)", "round trips/yr", "flat share"],
                 [[n, _f(o["gated"]["mean_bp_day"], sign=True), _f(o["gated"]["ret_ann"] * 100, 2, True),
                   _f(o["gated"]["vol_ann"] * 100, 1), _f((o["gated"].get("ret_ann_volmatched") or float("nan")) * 100,
                   2, True), _f(o["gated"]["sharpe"]), _f(o["gated"].get("d_sharpe_ann"), sign=True),
                   _p(o["gated"].get("d_sharpe_p"), k), _f(o["gated"]["round_trips_yr"], 1),
                   _f(o["gated"]["flat_share"], 3)] for n, o in g.items()]), "",
          (f"Ungated: Sharpe {_f(u['sharpe'])}, {_f(u['ret_ann'] * 100, 2, True)} %/yr, vol "
           f"{_f(u['vol_ann'] * 100, 1)} %/yr. A gate with no information on a share π of sessions has Sharpe ≈ √π × "
           "the ungated one (§6.4), so ΔSharpe < 0 is the null's expectation, not a finding."), ""]  # fmt: skip
    inc = spec["incremental"]
    L += [f"## 4. Incremental value given {' + '.join(inc['baselines'])} (Newey–West, 5 lags)", "",
          _table(["gate", "n", "β bp/day", "t NW"], [[i["gate"], i["n"], _f(i.get("beta_bp_day"), sign=True),
                 _f(i.get("t_nw")) if "error" not in i else i["error"]] for i in record["incremental"]]), ""]  # fmt: skip
    L += ["## 5. Regime refits over the window (R2: φ ordering; R1: σ ordering)", "",
          _table(["model", "refits", "not converged", "degenerate", "swap flag (|Δφ| < 0.05)", "skipped",
                  "median low / high", "median separation (10 % quantile)"],
                 [[m, s["refits"], s["not_converged"], s["degenerate"], s["swap_flag"] if m != "R1" else "—",
                   s["skipped"], f"{_f(s['stat_lo_median'], 3)} / {_f(s['stat_hi_median'], 3)}",
                   f"{_f(s['sep_median'], 3)} ({_f(s['sep_q10'], 3)})"] for m, s in record["refits"].items()]), "",
          ("U9's reading: on regime-free data R2 calls 'fast' 40 % of the time with 78 % degenerate refits, and its "
           "two φs sit near the random-walk 0.71 (gap often < 0.05). An R2 gate is meaningful only if the "
           "separation is wide and the degenerate count low."), ""]  # fmt: skip
    return "\n".join(L)
