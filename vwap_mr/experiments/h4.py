"""H4 runner: the regime-weighted MR / trend blend vs the static blend (U11; PLAN.md §6.5, §7 U11).

`run_h4(path)` has `run_h3`'s refusals (an invalid, uncommitted or dirty spec, uncommitted code, a spec already in the
ledger, a scratch breach, a non-synthetic unregistered market) and checks the rebuilt R₀ stream against H1's ledger
row. It records the look (label `U11 run <id>@<sha12>`, n = configurations + the new reference streams) before
computing, writes `results/cells/<hash>/` per configuration and the run record, and appends the ledger rows last.

Inputs and timing:
- The weight's states and the trend inputs come from the spliced long series (`features.state.long_inputs`), so
  every window is full on 2016-01-04. Row t is as of session t's close decision: B1_vol, B1's gate (H3's rule), its
  trailing-504 percentile, R2's one-step-ahead P(fast) (U9's configuration), TSMOM's trailing returns, Donchian.
- B6_diff = 1[Σ_63 (R₀ net − TSMOM net) > 0], both on the development window at the registered cost (row t's net
  is realized by t's close); its first 62 sessions are undefined and take the registered `undefined` weight.
- Each configuration's blend and its static blend (w = 0.5, the same TF and vol target) are backtested as one netted
  target at the registered costs; the statistic is the paired Ledoit–Wolf ΔSharpe of their daily excess returns.

Per configuration: both Sharpes, ΔSharpe (annualized), p, the one-sided 90 % lower bound, ρ(blend, static), the
blend's return at the static's volatility, the drawdown difference, the floor (on net returns), w's distribution, the
ΔSharpe vs a matched-mean static (w fixed at E[w]: the timing value without the static tilt toward TF), and the turnover of
the w-scaled MR leg and the (1 − w)-scaled TF leg with the netting between them; the blend's cost is attributed to
the legs pro rata to that turnover. The headline passes iff p < alpha and the floor holds (k = 1). Also reported: R₀,
TSMOM, DC55 and the vol-managed buy-and-hold alone, the MR–TF correlation by year, and R2's refits."""

import json
import time
from dataclasses import replace
from datetime import UTC, datetime
from pathlib import Path

import numpy as np
import pandas as pd

from vwap_mr.backtest.costs import CostModel, tbill_yield
from vwap_mr.backtest.engine import Result, run
from vwap_mr.backtest.metrics import ANN, summary
from vwap_mr.config import ROOT
from vwap_mr.experiments.h3 import Inputs, check_h1_stream, load_inputs, refit_summary
from vwap_mr.experiments.ledger import Ledger, clean
from vwap_mr.experiments.looks import LOOKS_PATH, check_window, looks, looks_total, record_look
from vwap_mr.experiments.persistence import _years, trailing_quantile
from vwap_mr.experiments.report import ReportRefused, _f, _p, _table
from vwap_mr.experiments.runner import CELLS_DIR, LEDGER_PATH, RunError, _hash, _rel
from vwap_mr.experiments.spec import Spec, SpecError, _num, check_clean, code_hash, load_spec, registered_sha
from vwap_mr.features.deviation import state_deviation
from vwap_mr.features.state import REGISTERED, realized_vol
from vwap_mr.regime.hmm import rolling_regime
from vwap_mr.stats.bootstrap import sharpe_test
from vwap_mr.strategies.blend import Blend, weight
from vwap_mr.strategies.gates import gate
from vwap_mr.strategies.trend import TREND, trend_state, vol_managed
from vwap_mr.strategies.vwap_mr import r0

P_SOURCES = ("B1", "B1_pct", "R2", "B6_diff")
KEEP = (
    "mean_bp_day",
    "ret_ann",
    "vol_ann",
    "sharpe",
    "max_dd",
    "round_trips_yr",
    "flat_share",
    "cost_bp_day",
)


def _validate_h4(spec: Spec) -> None:
    cf, names = spec.data.get("configurations"), [*spec["headlines"], *spec["variants"]]
    if not isinstance(cf, dict) or [n for n in names if n not in cf]:
        raise SpecError("an H4 spec needs a `configurations` entry for every headline and variant")
    if not _num(spec["floors"].get("net_ratio_min")):
        raise SpecError("floors.net_ratio_min must be a number")
    for k in ("blend", "weight_configuration"):
        if k not in spec.data:
            raise SpecError(f"an H4 spec needs `{k}`")
    wc = spec["weight_configuration"]
    if wc["B1"]["column"] != "B1_vol" or not _num(wc.get("trailing_quantile_window")):
        raise SpecError("weight_configuration: B1 reads B1_vol; trailing_quantile_window must be a number")
    for n in names:
        c = cf[n]
        if (
            c.get("P") not in P_SOURCES
            or c.get("tf") not in TREND
            or not (_num(c.get("kappa")) and 0 <= c["kappa"] <= 1)
        ):
            raise SpecError(f"configuration {n}: P in {P_SOURCES}, tf in {tuple(TREND)}, kappa in [0, 1]")  # fmt: skip
        if c["P"] == "B6_diff" and "undefined" not in c:
            raise SpecError(
                f"configuration {n}: B6_diff is undefined for its first window; register `undefined`"
            )
        for k in ("sigma", "undefined"):
            if k in c and not (_num(c[k]) and 0 < c[k] <= 1):
                raise SpecError(f"configuration {n}: {k} must be a number in (0, 1]")


def weight_state(frame: pd.DataFrame, wc: dict) -> tuple[pd.DataFrame, list[dict]]:
    """The long-series state the blends read (module docstring), and R2's refit log."""
    close = frame["close"].astype(float)
    st = trend_state(close)
    st["B1_vol"] = realized_vol(close.pct_change(), REGISTERED.vol)
    q = int(wc["trailing_quantile_window"])
    st["P_B1"] = gate(st, wc["B1"], q)
    st["P_B1_pct"] = trailing_quantile(st["B1_vol"], q)
    c = wc["R2"]
    d = state_deviation(close, frame["volume"].astype(float), c["n"])
    p, log = rolling_regime(d, pd.Series(d.shift(1)), kind="phi", fit_window=c["fit_window"],
                            refit_every=c["refit_every"])  # fmt: skip
    st["P_R2"] = p["p_next"]
    return st, log


def leg_turnover(res: Result, w: np.ndarray, f_mr: np.ndarray, f_tf: np.ndarray, scale: np.ndarray) -> dict:
    """Per-year turnover of each scaled leg, filled as the engine fills the blend (target t at t + 1's open), the
    netting between them, and the blend's cost split pro rata to the legs' turnover."""
    yrs = (len(w) - 1) / ANN
    tv = lambda f: float(np.abs(np.diff(np.r_[0.0, f[:-1]])).sum() / yrs)
    mr, tf = tv(w * f_mr * scale), tv((1 - w) * f_tf * scale)
    s = summary(res)
    cost = s["cost_bp_day"]
    return {"blend": s["turnover"], "mr_leg": mr, "tf_leg": tf, "netted": mr + tf - s["turnover"],
            "cost_bp_day": cost, "cost_mr_bp_day": cost * mr / (mr + tf) if mr + tf else 0.0,
            "cost_tf_bp_day": cost * tf / (mr + tf) if mr + tf else 0.0}  # fmt: skip


def net_ann(res: Result) -> float:
    """Annualized mean daily net return (the floor's 'net return', PLAN §6.5), after the first session."""
    return float(res.net.iloc[1:].mean() * ANN)


def corr_by_year(a: pd.Series, b: pd.Series) -> dict:
    yr = _years(a)
    return {int(y): float(a[yr == y].corr(b[yr == y])) for y in np.unique(yr)}


def run_h4(
    path: Path | str, *, inputs: Inputs = load_inputs, ledger_path: Path = LEDGER_PATH, looks_path: Path = LOOKS_PATH,
    cells_dir: Path = CELLS_DIR, repo: Path = ROOT, require_registered: bool = True, data_hash=None,
) -> dict:  # fmt: skip
    t0 = time.time()
    spec = load_spec(path)
    if spec["test"]["kind"] != "paired_sharpe":
        raise SpecError(f"run_h4 runs paired_sharpe specs, not {spec['test']['kind']}")
    _validate_h4(spec)
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
        names, cf = heads + variants, spec["configurations"]
        if len(names) > spec["budget"]:
            raise SpecError("over budget")
        market, frame, _ = inputs(spec)
        check_window(market.index, "dev")
        if not require_registered and market.source != "synthetic":
            raise RunError("an unregistered run may read synthetic markets only")
        if data_hash is None:
            from vwap_mr.data.store import verify_vintage, vintage_dir

            dh = verify_vintage(vintage_dir(spec["vintage"])) if require_registered else "synthetic"
            dh_diag = verify_vintage(vintage_dir("diag")) if require_registered else "synthetic"
        else:
            dh, dh_diag = data_hash(spec["vintage"]), data_hash("diag")
        tfs = sorted({cf[n]["tf"] for n in names} | {"TSMOM"})
        refs = [*tfs, "vol_managed"]
        look = record_look(f"U11 run {spec.id}@{spec_sha[:12]}", len(names) + len(refs),
                           f"{spec.id}: {len(heads)} headline + {len(variants)} variant blends; references {refs}",
                           looks_path, window="dev")  # fmt: skip

        c, t, wc = spec["cost"], spec["test"], spec["weight_configuration"]
        costs = CostModel(round_trip_bp=c["registered_bp"], borrow_rate=c["borrow_rate"], fees=c["fees"])
        y = tbill_yield(market)
        go = lambda m, s: run(m, s, costs=costs, cash_yield=y)
        mr = r0()
        base = go(market, mr)
        if require_registered:
            check_h1_stream(float(base.excess.iloc[1:].mean() * 1e4), prior_rows)
        long_state, r2_log = weight_state(frame, wc)
        st = long_state.reindex(market.index)
        sm = replace(market, state=st)
        alone = {k: go(sm, TREND[k]()) for k in tfs}
        vs = wc["B6_diff"]["window"]
        diff = (base.net - alone["TSMOM"].net).rolling(vs).sum()
        st["P_B6_diff"] = (diff > 0).astype(float).where(diff.notna())
        bm = spec["blend"]["benchmarks"]["vol_managed"]
        alone["vol_managed"] = go(sm, vol_managed(bm["sigma"]))
        kw = {"block": t["block"], "n_boot": t["n_boot"], "seed": t["seed"], "sided": t["sided"]}
        f_mr = mr.targets(market).to_numpy()
        ch = code_hash()
        out, statics = {}, {}
        for name in names:
            cc, is_head = cf[name], name in heads
            p = st[f"P_{cc['P']}"]
            w = weight(p, cc["kappa"])
            n_undef = int(w.isna().sum())
            if n_undef and "undefined" not in cc:
                raise RunError(
                    f"{name}: P_t ({cc['P']}) undefined on {n_undef} session(s) and no registered weight"
                )
            w = w.fillna(cc.get("undefined", np.nan))
            gm = replace(market, state=st.assign(w=w, w_static=0.5))
            tf, sig = TREND[cc["tf"]](), cc.get("sigma")
            res = go(gm, Blend(mr, tf, "w", sig, name))
            key = (cc["tf"], sig)
            if key not in statics:
                statics[key] = go(gm, Blend(mr, tf, "w_static", sig, "static"))
            sres = statics[key]
            bx, sx = res.excess.iloc[1:], sres.excess.iloc[1:]
            sb, ss = summary(res), summary(sres)
            rec: dict = {"name": name, "kind": "headline" if is_head else "variant", "config": cc,
                         "n_obs": len(bx), "n_undefined": n_undef, "blend": {k: sb[k] for k in KEEP},
                         "static": {k: ss[k] for k in KEEP}, "status": "ok",
                         "w_mean": float(w.mean()), "w_values": None}  # fmt: skip
            if w.nunique() <= 4:
                rec["w_values"] = {f"{v:g}": float((w == v).mean()) for v in np.unique(w)}
            try:
                lw = sharpe_test(bx.to_numpy(), sx.to_numpy(), alpha=0.10, **kw)
                rec |= {"d_sharpe_ann": lw["delta_ann"], "p": lw["p"], "lower90_ann": lw["ci"][0] * np.sqrt(ANN),
                        "t": lw["t"], "n_invalid": lw["n_invalid"]}  # fmt: skip
            except ValueError as e:
                rec |= {"status": "no_fit", "error": str(e), "p": None}
            rec["rho"] = float(bx.corr(sx))
            rec["ret_ann_volmatched"] = float(bx.mean() * ANN * sx.std(ddof=1) / bx.std(ddof=1))
            rec["dd_diff"] = sb["max_dd"] - ss["max_dd"]
            fl, nb, ns = spec["floors"]["net_ratio_min"], net_ann(res), net_ann(sres)
            rec |= {"net_ann": nb, "net_ann_static": ns, "floor": bool(nb >= ns - (1 - fl) * abs(ns))}
            # the matched-mean static (reported): w fixed at the blend's mean weight, so ΔSharpe is timing only
            pi = wc["B1"]["pi"] if cc["P"] == "B1" else None
            wm = 0.5 + cc["kappa"] * (pi - 0.5) if pi is not None else float(w.mean())
            mres = go(replace(market, state=st.assign(w_mm=wm)), Blend(mr, tf, "w_mm", sig, "matched_mean"))
            mx = mres.excess.iloc[1:]
            rec["matched_mean"] = {"w": wm, "source": "registered π" if pi is not None else "realized mean w",
                                   "sharpe": summary(mres)["sharpe"]}  # fmt: skip
            try:
                lm = sharpe_test(bx.to_numpy(), mx.to_numpy(), alpha=0.10, **kw)
                rec["matched_mean"] |= {"d_sharpe_ann": lm["delta_ann"], "p": lm["p"]}
            except ValueError as e:
                rec["matched_mean"]["error"] = str(e)
            scale = (
                np.ones(len(w))
                if sig is None
                else np.minimum(1.0, sig / (st["B1_vol"].to_numpy() * np.sqrt(ANN)))
            )
            rec["turnover"] = leg_turnover(
                res, w.to_numpy(), f_mr, TREND[cc["tf"]]().targets(sm).to_numpy(), scale
            )
            rec["passes"] = (
                bool(rec["p"] is not None and rec["p"] < t["alpha"] and rec["floor"]) if is_head else None
            )
            h = _hash(spec.id, spec_sha, name, market.symbol, costs.round_trip_bp, ch, dh)
            d = Path(cells_dir) / h
            d.mkdir(parents=True, exist_ok=True)
            pd.DataFrame({"blend_excess": bx, "static_excess": sx, "w": w.iloc[1:]}).to_parquet(
                d / "daily.parquet"
            )
            (d / "result.json").write_text(json.dumps(clean(rec), indent=1, sort_keys=True, allow_nan=False))
            rec["cell_hash"] = h
            out[name] = rec

        rows = [{"cell_hash": o["cell_hash"], "spec_id": spec.id, "spec_sha": spec_sha, "kind": o["kind"],
                 "name": n, "strategy": f"blend|{n}", "symbol": market.symbol, "git_sha": git_sha, "code_hash": ch,
                 "data_hash": dh, "vintage": spec["vintage"], "started_at": look["at"],
                 "runtime_s": round(time.time() - t0, 1), "n_obs": o["n_obs"], "d_sharpe_ann": o.get("d_sharpe_ann"),
                 "p_boot": o["p"], "sharpe": o["blend"]["sharpe"], "sharpe_static": o["static"]["sharpe"],
                 "mean_bp_day": o["blend"]["mean_bp_day"], "max_dd": o["blend"]["max_dd"],
                 "cost_bp": costs.round_trip_bp, "status": o["status"], "error_path": None}
                for n, o in out.items()]  # fmt: skip
        mx = base.excess.iloc[1:]
        references = {"R0": summary(base), **{k: summary(v) for k, v in alone.items()}}
        record = clean({
            "spec_id": spec.id, "spec_path": _rel(spec.path, repo), "spec_sha": spec_sha, "git_sha": git_sha,
            "code_hash": ch, "data_hash": dh, "data_hash_diag": dh_diag, "vintage": spec["vintage"],
            "spec": json.loads(json.dumps(spec.data, default=str)), "look": look, "k_at_look": looks_total(looks_path),
            "configurations": out, "references": {k: {kk: v[kk] for kk in KEEP} for k, v in references.items()},
            "corr_by_year": {k: corr_by_year(mx, alone[k].excess.iloc[1:]) for k in tfs},
            "corr_all": {k: float(mx.corr(alone[k].excess.iloc[1:])) for k in tfs},
            "r2_refits": refit_summary(r2_log, pd.DatetimeIndex(frame.index), spec["window"]["start"]),
            "passes": all(out[h]["passes"] for h in heads),
            "finished_at": datetime.now(UTC).isoformat(), "runtime_s": round(time.time() - t0, 1),
        })  # fmt: skip
        rp = Path(cells_dir) / f"run_{spec.id}_{spec_sha[:12]}.json"
        rp.parent.mkdir(parents=True, exist_ok=True)
        rp.write_text(json.dumps(record, indent=1, sort_keys=True, allow_nan=False))
        ledger.append(rows)  # last: a crash before this leaves the look but no trial rows
        record["record_path"] = rp.as_posix()
        return record


# ── report ──────────────────────────────────────────────────────────────────

CONF_HEADER = ["configuration", "P", "κ", "TF", "σ*", "Sharpe blend", "Sharpe static", "ΔSharpe (LW)", "90 % lower",
               "p one-sided (p·K)", "ρ", "%/yr blend / static", "floor"]  # fmt: skip


def _conf_row(n: str, o: dict, k: int) -> list:
    c = o["config"]
    return [n, c["P"], c["kappa"], c["tf"], c.get("sigma", "—"), _f(o["blend"]["sharpe"]), _f(o["static"]["sharpe"]),
            _f(o.get("d_sharpe_ann"), sign=True), _f(o.get("lower90_ann"), sign=True),
            _p(o["p"], k) if o["p"] is not None else o.get("error", "—"), _f(o["rho"], 3),
            f"{_f(o['blend']['ret_ann'] * 100, 2, True)} / {_f(o['static']['ret_ann'] * 100, 2, True)}", o["floor"]]  # fmt: skip


def render_h4(record: dict, looks_path: Path = LOOKS_PATH) -> str:
    look = record["look"]
    if not any(r.get("at") == look["at"] and r.get("label") == look["label"] and r.get("n") == look["n"]
               for r in looks(looks_path)):  # fmt: skip
        raise ReportRefused(
            f"no look row {look['label']!r} at {look['at']} in {looks_path}: record the look first"
        )
    k = looks_total(looks_path)
    spec, cs = record["spec"], record["configurations"]
    heads, pw = spec["headlines"], spec["power"]
    L = [f"# {record['spec_id']} — H4 regime-weighted blend vs static blend (PLAN.md §6.5)", ""]
    L += [(f"Spec `{record['spec_path']}` @ `{record['spec_sha'][:12]}`; code `{record['git_sha'][:12]}` (hash "
           f"`{record['code_hash'][:12]}`); data vintage `{record['vintage']}` (`{record['data_hash'][:12]}`). Window "
           f"{spec['window']['start']} → {spec['window']['end']}, {spec['instruments']['headline']}, "
           f"{spec['cost']['registered_bp']} bp, {spec['execution']['decision']} → {spec['execution']['fill']}. Look "
           f"`{look['label']}` (n {look['n']}, {look['at']}). **K = {k}** (every window); p·K beside every p."), "",
          ("**Registered as a diagnostic** (§11, 2026-10-21): the weight's state, B1, was chosen after its H3 Δμ was read "
           f"on this window; the expected ΔSharpe, {spec['expected']['delta_sharpe']}, is below the MDE "
           f"{pw['mde_delta_sharpe']} (ρ {pw['rho']}, n {pw['n']}). A pass cannot confirm anything; only the holdout "
           "(U12) and the forward test (U13) can."), ""]  # fmt: skip
    h = cs[heads[0]]
    L += ["## 1. Headline (one-sided H₁: ΔSharpe > 0, blend vs static 50/50)", "",
          _table(CONF_HEADER + ["passes"], [_conf_row(n, cs[n], k) + [cs[n]["passes"]] for n in heads]), "",
          (f"**Verdict: {'passes' if record['passes'] else 'null'} (diagnostic)** — pass: p < {spec['test']['alpha']} "
           f"and the blend's net return ≥ the static's − {1 - spec['floors']['net_ratio_min']:g} × |static's|. "
           f"w_t: mean {_f(h['w_mean'], 3)}, shares {h['w_values']}. ΔSharpe is annualized; the 90 % lower bound is "
           "one-sided."), ""]  # fmt: skip
    L += ["## 2. Variants (each vs its own static blend: the same TF and vol target, w = 0.5)", "",
          _table(CONF_HEADER + ["undefined P (w = registered)"],
                 [_conf_row(n, cs[n], k) + [cs[n]["n_undefined"]] for n in spec["variants"]]), ""]  # fmt: skip
    L += ["## 3. Matched volatility and drawdown", "",
          _table(["configuration", "vol %/yr blend / static", "%/yr blend at static vol", "%/yr static",
                  "max DD blend / static", "DD difference"],
                 [[n, f"{_f(o['blend']['vol_ann'] * 100, 1)} / {_f(o['static']['vol_ann'] * 100, 1)}",
                   _f(o["ret_ann_volmatched"] * 100, 2, True), _f(o["static"]["ret_ann"] * 100, 2, True),
                   f"{_f(o['blend']['max_dd'] * 100, 1)} / {_f(o['static']['max_dd'] * 100, 1)}",
                   _f(o["dd_diff"] * 100, 1, True)] for n, o in cs.items()]), ""]  # fmt: skip
    L += ["## 3b. Timing vs static tilt: each blend vs its matched-mean static (reported, not the test)", "",
          _table(["configuration", "matched w", "from", "Sharpe matched", "ΔSharpe vs matched (LW)",
                  "p one-sided (p·K)", "net %/yr blend / 50-50 static"],
                 [[n, _f(o["matched_mean"]["w"], 3), o["matched_mean"]["source"], _f(o["matched_mean"]["sharpe"]),
                   _f(o["matched_mean"].get("d_sharpe_ann"), sign=True),
                   _p(o["matched_mean"].get("p"), k) if o["matched_mean"].get("p") is not None else
                   o["matched_mean"].get("error", "—"),
                   f"{_f(o['net_ann'] * 100, 2, True)} / {_f(o['net_ann_static'] * 100, 2, True)}"]
                  for n, o in cs.items()]), "",
          ("The 50/50 static is the registered benchmark (§6.5), but E[w] = 0.5 + κ(π − 0.5) ≠ 0.5, so the headline "
           "ΔSharpe also holds a constant reallocation (0.5 − E[w])(μ_TF − μ_MR); the matched-mean static removes it "
           "(w fixed at the registered-π mean for B1 weights, at the realized mean otherwise: then it is "
           "descriptive). A varying w also raises the blend's volatility (E[w² + (1 − w)²] > 0.5), which lowers "
           "its Sharpe against either static."), ""]  # fmt: skip
    L += ["## 4. Benchmarks and components alone (no test; references)", "",
          _table(["stream", "bp/day", "%/yr", "vol %/yr", "Sharpe", "max DD %", "round trips/yr", "flat share",
                  "cost bp/day"],
                 [[n, _f(s["mean_bp_day"], sign=True), _f(s["ret_ann"] * 100, 2, True), _f(s["vol_ann"] * 100, 1),
                   _f(s["sharpe"]), _f(s["max_dd"] * 100, 1), _f(s["round_trips_yr"], 1), _f(s["flat_share"], 3),
                   _f(s["cost_bp_day"], 2)]
                  for n, s in [*record["references"].items(), (f"static ({heads[0]}'s)", h["static"])]]), ""]  # fmt: skip
    years = sorted({y for v in record["corr_by_year"].values() for y in v})
    L += ["## 5. MR–TF daily excess-return correlation by year", "",
          _table(["TF", *map(str, years), "all"],
                 [[tf, *[_f(v.get(y, v.get(str(y))), 2) for y in years], _f(record["corr_all"][tf], 2)]
                  for tf, v in record["corr_by_year"].items()]), "",
          "The static blend already harvests this diversification; H4 asks whether the weight adds to it.", ""]  # fmt: skip
    L += ["## 6. Turnover (one-way, × equity per year) and cost attributed to each leg", "",
          _table(["configuration", "blend", "MR leg (w f^MR)", "TF leg ((1 − w) f^TF)", "netted", "cost bp/day",
                  "MR share", "TF share"],
                 [[n, _f(o["turnover"]["blend"], 1), _f(o["turnover"]["mr_leg"], 1), _f(o["turnover"]["tf_leg"], 1),
                   _f(o["turnover"]["netted"], 1), _f(o["turnover"]["cost_bp_day"], 2),
                   _f(o["turnover"]["cost_mr_bp_day"], 2), _f(o["turnover"]["cost_tf_bp_day"], 2)]
                  for n, o in cs.items()]), "",
          "Legs are filled as the engine fills the blend; the cost split is pro rata to the legs' turnover.", ""]  # fmt: skip
    s = record["r2_refits"]
    L += ["## 7. R2 refits over the window (the R2 variant's caveat, U10)", "",
          (f"{s['refits']} refits, {s['not_converged']} not converged, {s['degenerate']} degenerate, {s['swap_flag']} "
           f"swap-flagged (|Δφ| < 0.05); median φ {_f(s['stat_lo_median'], 3)} / {_f(s['stat_hi_median'], 3)}, "
           f"separation {_f(s['sep_median'], 3)} (10 % quantile {_f(s['sep_q10'], 3)})."), ""]  # fmt: skip
    return "\n".join(L)
