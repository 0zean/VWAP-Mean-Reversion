"""Runs a registered spec once (U6; PLAN.md §0, §6.2, §7 U6, §8.7–§8.8).

`run_spec(path)`:
1. refuses an invalid, uncommitted or dirty spec and uncommitted code (spec.py); a spec already run (any trial row
   in the ledger carrying its id: an amendment is a new file); a name outside the registry; trials over budget;
2. loads the headline and split instruments and checks every session lies in the look's window (dev);
3. records the look (label `U6 run <id>@<sha12>`, n = configurations read), and only then computes;
4. backtests each cell at the registered cost, writes `results/cells/<hash>/` (daily ledger, `result.json`) and one
   ledger row per cell (§8.8), appended together at the end (a crash leaves no trial rows, only the look);
5. returns the run record (also written to `results/cells/run_<id>_<sha12>.json`) for `report.render`.

Configurations in the look: the headline, each variant, each split instrument and each of the headline's 16 cells
(the specification curve is read cell by cell). Years, vol / VIX bins, weekdays, sides, legs and cost levels are
splits of the headline's configuration (U5's counting rule). The buy-and-hold benchmark is a reference, not a
configuration.

Synthetic runs (`require_registered=False`) skip the git checks and are allowed only when every market is
synthetic: no registered statistic can come from an unregistered spec."""

import hashlib
import json
import time
from collections.abc import Callable
from dataclasses import dataclass
from datetime import UTC, datetime, timedelta
from pathlib import Path

import numpy as np
import pandas as pd

from vwap_mr.backtest import account
from vwap_mr.backtest.costs import CostModel, tbill_yield
from vwap_mr.backtest.engine import Result, reprice, run
from vwap_mr.backtest.market import Market, load_market
from vwap_mr.backtest.metrics import ANN, summary
from vwap_mr.config import RESULTS_DIR, ROOT, DecisionTime
from vwap_mr.experiments.diagnose import nw_mean
from vwap_mr.experiments.ledger import Ledger, clean
from vwap_mr.experiments.looks import LOOKS_PATH, check_window, looks_total, record_look
from vwap_mr.experiments.spec import Spec, SpecError, check_clean, code_hash, load_spec, registered_sha
from vwap_mr.stats.bootstrap import mean_test
from vwap_mr.strategies.vwap_mr import HEADLINES, NEEDS_RTH, VARIANTS

CELLS_DIR = RESULTS_DIR / "cells"
LEDGER_PATH = RESULTS_DIR / "ledger.jsonl"
Loader = Callable[[str, Spec, bool], Market]


class RunError(RuntimeError):
    pass


@dataclass(frozen=True)
class Hold:
    """Buy-and-hold at the headline's fill: the benchmark for beta / alpha."""

    name: str = "buy_and_hold"
    decision: DecisionTime = DecisionTime.CLOSE
    fill: str = "next_open"

    def target(self, view) -> float:
        return 1.0

    def targets(self, market) -> pd.Series:
        return pd.Series(1.0, index=market.index)


def load_registered(symbol: str, spec: Spec, with_rth: bool) -> Market:
    w = spec["window"]
    return load_market(symbol, w["start"], w["end"] + timedelta(days=1), vintage=spec["vintage"],
                       with_rth=with_rth)  # fmt: skip


def stream_stats(result: Result, test: dict, benchmark: pd.Series | None = None) -> dict:
    """Summary metrics, the one-sided mean test (p, the one-sided 90 % lower bound) and the two-sided 90 % interval of
    the daily net excess return, and the gross edge per round trip in bp."""
    x = result.excess.iloc[1:]
    kw = {"block": test["block"], "n_boot": test["n_boot"], "seed": test["seed"]}
    one = mean_test(x, alpha=0.10, sided=test["sided"], **kw)
    two = mean_test(x, alpha=0.10, sided="two", **kw)
    s = summary(result, benchmark)
    gross_ann = float(result.ledger["gross"].iloc[1:].mean() * ANN)
    rt = s["round_trips_yr"]
    return s | {"p_boot": one["p"], "lower90": one["ci"][0] * 1e4, "ci90": [two["ci"][0] * 1e4, two["ci"][1] * 1e4],
                "t_boot": one["t"], "t_nw": one["t_nw"], "se_bp": one["se"] * 1e4, "n_invalid": one["n_invalid"],
                "gross_bp_day": gross_ann / ANN * 1e4,
                "gross_bp_rt": gross_ann / rt * 1e4 if rt > 0 else float("nan")}  # fmt: skip


def _table(x: pd.Series, key) -> list[dict]:
    rows = []
    for k, g in x.groupby(key, observed=True):
        m, t = nw_mean(g) if len(g) > 10 else (float(g.mean()), float("nan"))
        rows.append({"bin": str(k), "n": len(g), "mean_bp_day": m * 1e4, "t_nw": t})
    return rows


def splits(result: Result, market: Market) -> dict:
    """§6.2 splits of the headline: net excess by year, by the decision-time 21-session realized-vol quintile and VIX
    tercile (bins from the sample's own quantiles: descriptive), by weekday; the gross by overnight / intraday leg
    and by long / short side (first order: a·r_on + b·r_id; the cross term is shown)."""
    x = result.excess.iloc[1:]
    when = market.decision_instants(DecisionTime.CLOSE)
    vol = market.sessions["close"].pct_change().rolling(21).std(ddof=1).shift(1).reindex(x.index)
    vix = pd.Series(market.exo.asof("VIX", when), index=market.index).shift(1).reindex(x.index) \
        if market.exo is not None else pd.Series(np.nan, index=x.index)  # fmt: skip
    lg, led = result.legs.iloc[1:], result.ledger.iloc[1:]
    a, b = led["exp_overnight"], led["exp_close"]
    parts = {
        "overnight leg (a·r_on)": a * lg["r_on"], "intraday leg (b·r_id)": b * lg["r_id"],
        "long side": a.clip(lower=0) * lg["r_on"] + b.clip(lower=0) * lg["r_id"],
        "short side": a.clip(upper=0) * lg["r_on"] + b.clip(upper=0) * lg["r_id"],
        "cross term": led["gross"] - a * lg["r_on"] - b * lg["r_id"],
    }  # fmt: skip
    legs = [
        {"bin": k, "n": len(v), "mean_bp_day": nw_mean(v)[0] * 1e4, "t_nw": nw_mean(v)[1]}
        for k, v in parts.items()
    ]
    return {
        "year": _table(x, x.index.year),
        "vol_quintile": _table(
            x[vol.notna()], pd.qcut(vol.dropna(), 5, labels=[f"Q{i}" for i in range(1, 6)])
        ),
        "vix_tercile": _table(x[vix.notna()], pd.qcut(vix.dropna(), 3, labels=["T1 low", "T2", "T3 high"])),
        "weekday": _table(x, x.index.day_name()),
        "side_leg": legs,
    }


def floors(head: dict, coherence: int, spec: Spec) -> dict:
    f, rt = spec["floors"], spec["cost"]["registered_bp"]
    need_rt = f["gross_rt_multiple"] * rt
    out = {
        "net_excess_ann": {"value": head["ret_ann"], "min": f["net_excess_ann_min"]},
        "gross_bp_rt": {"value": head["gross_bp_rt"], "min": need_rt},
        "coherence": {"value": coherence, "min": f["coherence_min"]},
    }
    for v in out.values():
        v["ok"] = bool(np.isfinite(v["value"]) and v["value"] >= v["min"])
    return out


def verdict(p: float, floors_ok: bool, alpha: float, promising: float) -> str:
    """§6.2: confirmed (p < alpha, floors, coherence), promising (p < promising), else rejected."""
    if floors_ok and p < alpha:
        return "confirmed"
    if floors_ok and p < promising:
        return "promising"
    return "rejected"


def _rel(path: Path, repo: Path) -> str:
    p = Path(path).resolve()
    return (
        p.relative_to(Path(repo).resolve()).as_posix()
        if p.is_relative_to(Path(repo).resolve())
        else p.as_posix()
    )


def _hash(*parts) -> str:
    return hashlib.sha256(json.dumps(parts, sort_keys=True, default=str).encode()).hexdigest()[:16]


def _strategies(spec: Spec) -> tuple[dict, dict]:
    if spec["test"]["kind"] != "mean_vs_zero":
        raise SpecError(
            f"this runner implements mean_vs_zero (H1); {spec['test']['kind']} arrives with U10 / U11"
        )
    unknown = [h for h in spec["headlines"] if h not in HEADLINES] + [
        v for v in spec["variants"] if v not in VARIANTS
    ]
    if unknown:
        raise SpecError(f"names outside the registry: {unknown}")
    heads = {h: HEADLINES[h]() for h in spec["headlines"]}
    variants = {v: VARIANTS[v]() for v in spec["variants"]}
    if len(heads) + len(variants) > spec["budget"]:
        raise SpecError("over budget")
    ex = spec["execution"]
    for s in [*heads.values(), *variants.values()]:
        if (s.decision.value, s.fill) != (ex["decision"], ex["fill"]):
            raise SpecError(
                f"{s.name} runs {s.decision}/{s.fill}, the spec registers {ex['decision']}/{ex['fill']}"
            )
    return heads, variants


def run_spec(
    path: Path | str, *, loader: Loader = load_registered, ledger_path: Path = LEDGER_PATH,
    looks_path: Path = LOOKS_PATH, cells_dir: Path = CELLS_DIR, repo: Path = ROOT, require_registered: bool = True,
    data_hash: Callable[[str], str] | None = None,
) -> dict:  # fmt: skip
    t0 = time.time()
    spec = load_spec(path)
    if require_registered:
        spec_sha, git_sha = registered_sha(spec.path, repo), check_clean(repo)
    else:
        spec_sha = git_sha = "unregistered"
    ledger = Ledger(ledger_path)
    with ledger.run_lock():
        if any(r.get("spec_id") == spec.id for r in ledger.rows() if "cell_hash" in r):
            raise RunError(
                f"{spec.id} has already run (ledger {ledger_path}); an amendment is a new spec file"
            )
        heads, variants = _strategies(spec)
        head_name, (head,) = spec["headlines"][0], heads.values()
        symbols = [spec["instruments"]["headline"], *spec["instruments"]["splits"]]
        rth = any(v in NEEDS_RTH for v in variants)
        markets = {s: loader(s, spec, rth and s == symbols[0]) for s in symbols}
        for m in markets.values():
            check_window(m.index, "dev")
        if not require_registered and any(m.source != "synthetic" for m in markets.values()):
            raise RunError("an unregistered run may read synthetic markets only")
        if data_hash is None:
            from vwap_mr.data.store import verify_vintage, vintage_dir

            dh = verify_vintage(vintage_dir(spec["vintage"])) if require_registered else "synthetic"
        else:
            dh = data_hash(spec["vintage"])
        cells = head.cells
        n_conf = len(heads) + len(variants) + len(symbols) - 1 + len(cells)
        label = f"U6 run {spec.id}@{spec_sha[:12]}"
        look = record_look(label, n_conf, f"{spec.id}: headline, {len(variants)} variants, {len(symbols) - 1} "
                           f"instruments, {len(cells)} region cells", looks_path, window="dev")  # fmt: skip

        costs = CostModel(round_trip_bp=spec["cost"]["registered_bp"], borrow_rate=spec["cost"]["borrow_rate"],
                          fees=spec["cost"]["fees"])  # fmt: skip
        test, base = spec["test"], markets[symbols[0]]
        y = tbill_yield(base)
        hold = run(base, Hold(), costs=costs, cash_yield=y)
        bench = hold.excess
        ch = code_hash()
        rows, cell_out = [], []

        def cell(kind: str, key: str, res: Result, market: Market, stats: dict) -> dict:
            h = _hash(spec.id, spec_sha, key, market.symbol, costs.round_trip_bp, ch, dh)
            d = Path(cells_dir) / h
            d.mkdir(parents=True, exist_ok=True)
            res.ledger.assign(r_on=res.legs.get("r_on"), r_id=res.legs.get("r_id")).to_parquet(
                d / "daily.parquet"
            )
            rec = clean({"key": key, "strategy": res.strategy, "symbol": market.symbol, "kind": kind,
                         "fallbacks": res.fallbacks, **stats})  # fmt: skip
            (d / "result.json").write_text(json.dumps(rec, indent=1, sort_keys=True, allow_nan=False))
            rows.append({"cell_hash": h, "spec_id": spec.id, "spec_sha": spec_sha, "kind": kind, "name": key,
                         "strategy": res.strategy, "symbol": market.symbol, "git_sha": git_sha, "code_hash": ch,
                         "data_hash": dh, "vintage": spec["vintage"], "started_at": look["at"],
                         "runtime_s": round(time.time() - t0, 1), "n_obs": stats["n_obs"],
                         "ret_ann": stats["ret_ann"], "vol_ann": stats["vol_ann"], "sharpe": stats["sharpe"],
                         "psr": stats["psr0"], "mean_bp_day": stats["mean_bp_day"], "p_boot": stats["p_boot"],
                         "t_nw": stats["t_nw"], "lower90": stats["lower90"], "max_dd": stats["max_dd"],
                         "turnover": stats["turnover"], "round_trips_yr": stats["round_trips_yr"],
                         "flat_share": stats["flat_share"], "cost_bp": costs.round_trip_bp, "status": "ok",
                         "error_path": None})  # fmt: skip
            return rec

        res = run(base, head, costs=costs, cash_yield=y)
        head_stats = stream_stats(res, test, bench)
        tradable, reasons = account.check(res, spec["account"])
        headline = cell("headline", head_name, res, base, head_stats) | {
            "account": {"profile": spec["account"], "tradable": tradable, "reasons": reasons}
        }
        for c in cells:
            cr = run(base, c, costs=costs, cash_yield=y)
            cell_out.append(cell("split", f"{head_name}/{c.name}", cr, base, stream_stats(cr, test)) |
                            {"n": c.n, "b": c.b})  # fmt: skip
        coherence = sum(c["mean_bp_day"] > 0 for c in cell_out)
        fl = floors(head_stats, coherence, spec)
        v = verdict(
            head_stats["p_boot"], all(f["ok"] for f in fl.values()), test["alpha"], test["promising_alpha"]
        )

        curve = []
        for bp in spec["cost"]["curve"]:
            r = reprice(res, bp)
            s = stream_stats(r, test)
            curve.append(
                {"bp": bp, **{k: s[k] for k in ("mean_bp_day", "p_boot", "lower90", "sharpe", "ret_ann")}}
            )
        m0 = next(c["mean_bp_day"] for c in curve if c["bp"] == min(spec["cost"]["curve"]))
        top = max(curve, key=lambda c: c["bp"])
        slope = (m0 - top["mean_bp_day"]) / top["bp"] if top["bp"] > 0 else float("nan")
        break_even = m0 / slope if m0 > 0 and slope > 0 else None

        var_out = []
        gaps = []
        if rth:
            miss = base.sessions[["rth_volume", "rth_vwap"]].isna().any(axis=1)
            gaps = [str(d.date()) for d in base.index[miss.to_numpy()]]
        for key, strat in variants.items():
            vr = run(base, strat, costs=costs, cash_yield=y)
            var_out.append(cell("variant", key, vr, base, stream_stats(vr, test, bench)))
        inst = []
        for sym in symbols[1:]:
            m = markets[sym]
            ir = run(m, head, costs=costs, cash_yield=tbill_yield(m))
            inst.append(cell("split", f"{head_name}@{sym}", ir, m, stream_stats(ir, test)))
        ledger.append(rows)

        record = clean({
            "spec_id": spec.id, "spec_path": _rel(spec.path, repo), "spec_sha": spec_sha, "git_sha": git_sha,
            "code_hash": ch, "data_hash": dh, "vintage": spec["vintage"],
            "spec": json.loads(json.dumps(spec.data, default=str)), "look": look,
            "k_at_look": looks_total(looks_path), "headline": headline, "cells": cell_out, "coherence": coherence,
            "floors": fl, "verdict": v, "cost_curve": curve, "break_even_bp": break_even,
            "splits": splits(res, base), "variants": var_out, "variant_input_gaps": gaps, "instruments": inst,
            "benchmark": stream_stats(hold, test), "finished_at": datetime.now(UTC).isoformat(),
            "runtime_s": round(time.time() - t0, 1),
        })  # fmt: skip
        out = Path(cells_dir) / f"run_{spec.id}_{spec_sha[:12]}.json"
        out.parent.mkdir(parents=True, exist_ok=True)
        out.write_text(json.dumps(record, indent=1, sort_keys=True, allow_nan=False))
        record["record_path"] = out.as_posix()
        return record
