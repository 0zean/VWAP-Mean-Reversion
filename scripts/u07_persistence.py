"""U7: H2 persistence and "is it still true" (PLAN.md §6.3, §7 U7) -> results/reports/U07_persistence.md.

    uv run python scripts/u07_persistence.py --vintage 2026-10
    uv run python scripts/u07_persistence.py --synthetic --scratch <tmp dir>      (dry run; scratch looks only)

Streams. The development-window stream is the H1 headline exactly as registered (R₀ on SPY, `next_open`, 1.0 bp,
borrow 0.4 %/yr, fees, DTB3; `specs/H1_vwap_mr.yaml`), rebuilt and checked against its ledger row (and the split
instruments against theirs) before anything is printed. The `diag` stream is R₀ on SPY 1993–2015 (`load_long`,
`next_open`), gross and at the documented historical cost schedule (`experiments/persistence.py`).

Looks (every table inside `looked`; U5's counting rule, n = configurations a table reads):
- dev: by-year and rolling 3-year (1); Bai–Perron (1); Kalman tracker (1); conditional tables, one per state
  variable (6: B1 vol quintile, B2 VIX tercile, prior-21-session sign of the region and of SPY, weekday, side);
  cross-instrument coherence (3, QQQ / IWM / DIA);
- diag: by-year and rolling 3-year (1); Bai–Perron (1).
No conditional table is computed on `diag`: under the §6.2 routing those sessions are H3's test sample (§11,
2026-10-21). State variables use the spliced long series and VIX from 2013-12 only as warm-up of the trailing
504-session windows; every conditioned return is a development-window session."""

import argparse
import json
import shutil
import sys
from pathlib import Path

import numpy as np
import pandas as pd

from vwap_mr.backtest.costs import CostModel, tbill_yield
from vwap_mr.backtest.engine import run, run_diagnostic
from vwap_mr.backtest.market import Market, load_market
from vwap_mr.config import DEV_START, HOLDOUT_START, RESULTS_DIR, SPECS_DIR, DecisionTime
from vwap_mr.data.alpaca import NY_TZ
from vwap_mr.data.exo import Exo
from vwap_mr.data.synthetic import daily_market
from vwap_mr.experiments import persistence as ps
from vwap_mr.experiments.diagnose import diag_market
from vwap_mr.experiments.ledger import Ledger
from vwap_mr.experiments.looks import LOOKS_PATH, check_window, looked, looks_total
from vwap_mr.experiments.runner import splits
from vwap_mr.experiments.spec import load_spec
from vwap_mr.stats.breaks import bai_perron, local_level, local_level_track
from vwap_mr.strategies.vwap_mr import r0

SYMBOLS = ("SPY", "QQQ", "IWM", "DIA")
SPEC = SPECS_DIR / "H1_vwap_mr.yaml"
DIAG_END = "2015-12-31"
WARMUP = "2013-12-01"


# ── data ────────────────────────────────────────────────────────────────────


class Data:
    def __init__(self, vintage: str | None):
        self.vintage = vintage
        if vintage is None:
            self._long = daily_market("SPY", "1993-01-29", "2025-12-31", 7)

    def dev(self, sym: str) -> Market:
        if (
            self.vintage is None
        ):  # synthetic SPY is the long series' tail, so states describe the traded series
            if sym == "SPY":
                keep = self._long.index >= pd.Timestamp(DEV_START, tz=NY_TZ)
                return Market(self._long.sessions[keep], "synthetic", exo=self._long.exo, symbol="SPY")
            return daily_market(sym, str(DEV_START), "2025-12-31", SYMBOLS.index(sym) + 1)
        return load_market(sym, DEV_START, HOLDOUT_START, vintage=self.vintage)

    def long(self) -> pd.DataFrame:
        """SPY 1993–2025: total-return close, nominal close (`close_split`), volume; `attrs['source'] = 'diag'`."""
        if self.vintage is None:
            s = self._long.sessions
            f = pd.DataFrame({"open": s["open"], "close": s["close"], "close_split": s["close_raw"],
                              "volume": s["volume"], "is_diag": s.index < pd.Timestamp(DEV_START, tz=NY_TZ)})  # fmt: skip
            f.attrs["source"] = "diag"
            return f
        from vwap_mr.data.secondary import load_long

        return load_long("SPY", self.vintage)[0]

    def exo(self, name: str, start) -> pd.DataFrame:
        if self.vintage is None:
            assert self._long.exo is not None
            f = self._long.exo._frames[name]
            return f[f.index >= pd.Timestamp(start)]
        from vwap_mr.data.exo import load_series

        return load_series(name, start, HOLDOUT_START, vintage=self.vintage)


# ── formatting ──────────────────────────────────────────────────────────────


def f2(x, nd=2, sign=True) -> str:
    return "—" if x is None or not np.isfinite(x) else (f"{x:+.{nd}f}" if sign else f"{x:.{nd}f}")


def table(header: list[str], rows: list[list]) -> str:
    out = ["| " + " | ".join(header) + " |", "|" + "---|" * len(header)]
    return "\n".join(out + ["| " + " | ".join(str(c) for c in r) + " |" for r in rows])


def period_table(rows: list[dict]) -> str:
    return table(["period", "n", "bp/day", "90 % CI", "t NW"],
                 [[r["period"], r["n"], f2(r["mean_bp_day"]), f"[{f2(r['ci90'][0])}, {f2(r['ci90'][1])}]",
                   f2(r["t_nw"], sign=False)] for r in rows])  # fmt: skip


# ── streams ─────────────────────────────────────────────────────────────────


def h1_streams(data: Data, spec, ledger_rows: list[dict] | None) -> tuple[dict, dict]:
    """R₀ on each instrument at the registered cost; checked against H1's ledger rows when given."""
    c = spec["cost"]
    costs = CostModel(round_trip_bp=c["registered_bp"], borrow_rate=c["borrow_rate"], fees=c["fees"])
    results, markets = {}, {}
    for sym in SYMBOLS:
        m = data.dev(sym)
        res = run(m, r0(), costs=costs, cash_yield=tbill_yield(m))
        if ledger_rows is not None:
            name = "R0" if sym == "SPY" else f"R0@{sym}"
            (row,) = [r for r in ledger_rows if r.get("spec_id") == spec.id and r.get("name") == name]
            got = float(res.excess.iloc[1:].mean() * 1e4)
            if abs(got - row["mean_bp_day"]) > 1e-9:
                raise RuntimeError(f"{name}: rebuilt stream {got} bp/day != H1 ledger {row['mean_bp_day']}")
        results[sym], markets[sym] = res, m
    return results, markets


def diag_stream(data: Data) -> pd.DataFrame:
    """R₀ on diag 1993–2015, next_open: gross, net excess at the historical schedule, the schedule itself."""
    frame = data.long().loc[:DIAG_END]
    frame.attrs["source"] = "diag"
    m = diag_market(frame, dtb3=data.exo("DTB3", "1992-12-01"))
    res = run_diagnostic(
        m, r0(), fill="next_open", costs=CostModel(0.0, fees=False), cash_yield=tbill_yield(m)
    )
    lg = res.ledger.iloc[1:]
    rt = ps.historical_rt_bp(frame["close_split"].shift(1)).reindex(lg.index)  # cost booked at row t's open
    net = ps.at_cost(lg["net"], lg["traded"], rt)
    return pd.DataFrame({"gross": lg["gross"], "excess_hist": net - lg["rf"], "rt_bp": rt})


# ── sections ────────────────────────────────────────────────────────────────


def periods_section(x: pd.Series, title: str) -> str:
    return "\n".join([f"**{title} — by calendar year**", "", period_table(ps.by_year(x)), "",
                      f"**{title} — rolling 3 calendar years**", "", period_table(ps.rolling_years(x)), ""])  # fmt: skip


def bp_section(x: pd.Series, title: str) -> tuple[str, dict]:
    b = bai_perron(x.to_numpy())
    idx = x.index
    seg = [str(idx[0].date())] + [str(idx[k].date()) for k in b["breaks"]]
    rows = [[f"{l['l']} → {l['l'] + 1}", f2(l["supF"], sign=False), f"{l['p']:.3f}"] for l in b["sequential"]]
    lines = [(f"**{title}** — breaks chosen (sequential, 5 %): **{b['n_breaks']}**; UDmax {f2(b['UDmax'], sign=False)} "
             f"(p {b['UDmax_p']:.3f}); h = {b['h']} sessions."), "",
             table(["step", r"supF(l+1 \| l)", "p"], rows), ""]  # fmt: skip
    if b["n_breaks"]:
        lines += [table(["segment from", "mean bp/day"], [[d, f2(mu * 1e4)] for d, mu in
                                                          zip(seg, b["segment_means"], strict=True)]),
                  "", "Break-date intervals (95 %): " + "; ".join(
                      f"{idx[lo].date()} … {idx[min(hi, len(idx) - 1)].date()}" for lo, hi in b["intervals"]), ""]  # fmt: skip
    return "\n".join(lines), b


def kalman_section(x: pd.Series) -> tuple[str, dict]:
    fit = local_level(x.to_numpy())
    tr = local_level_track(x.to_numpy(), min_obs=252, refit_every=21)
    ll = np.asarray(fit["profile_ll"])
    q = np.asarray(fit["profile_q"])
    show = [0] + [int(np.argmin(np.abs(q[1:] - v))) + 1 for v in (1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 1e-3)]
    prof = [[f"{q[i]:.0e}" if q[i] > 0 else "0", f2(ll[i] - ll.max(), 2)] for i in show]
    ends = x.groupby(ps._years(x)).tail(1).index
    pos = {d: i for i, d in enumerate(x.index)}
    traj = [[str(d.date()), f2(tr["mu"][pos[d]] * 1e4), f2(tr["sd"][pos[d]] * 1e4, sign=False),
             f2(tr["p_pos"][pos[d]], 3, sign=False)] for d in ends]  # fmt: skip
    nz = sum(r["q_zero"] for r in tr["refits"])
    lines = [
        (f"Whole-sample MLE: q̂ = {fit['q']:.2e} (σ̂_ε {np.sqrt(fit['sigma2_eps']) * 1e4:.1f} bp); LR test of q = 0: "
        f"{f2(fit['lr_q0'], sign=False)}, p {fit['p_q0']:.3f} → **q_zero = {fit['q_zero']}**"
        f"{' (q̂ at the grid edge)' if fit['q_at_grid_edge'] else ''}. Filtered μ_T = {f2(fit['mu_T'] * 1e4)} bp/day "
        f"(sd {f2(fit['sd_T'] * 1e4, sign=False)}; 90 % interval [{f2((fit['mu_T'] - 1.645 * fit['sd_T']) * 1e4)}, "
        f"{f2((fit['mu_T'] + 1.645 * fit['sd_T']) * 1e4)}]); **P(μ_T > 0) = {fit['P_mu_T_pos']:.3f}** at q̂, "
        f"{fit['P_mu_T_pos_q0']:.3f} at q = 0 (the full-sample mean's posterior)."), "",
        "Profile log-likelihood (relative to its maximum):", "", table(["q", "Δ log L"], prof), "",
        (f"Causal track (refit every 21 sessions on the past only, from session 252; {len(tr['refits'])} refits, "
        f"{nz} with q_zero): the U13 monitor (`live.monitor.EdgeMonitor`) reproduces it exactly."), "",
        table(["year end", "μ bp/day", "sd", "P(μ > 0)"], traj), "",
    ]  # fmt: skip
    return "\n".join(lines), fit | {"track_P_T": float(tr["p_pos"][-1])}


def states(data: Data, x: pd.Series, res, market: Market) -> dict[str, pd.Series]:
    """The conditioning keys of the return rows (state at decision t−1; module docstring)."""
    long = data.long()
    close = long["close"]
    sig = close.pct_change().rolling(21).std(ddof=1)
    b1 = ps.bins(ps.trailing_quantile(sig), 5)
    vix_f = data.exo("VIX", WARMUP)
    days = long.index[long.index >= pd.Timestamp(WARMUP, tz=NY_TZ)]
    when = pd.Series(
        (days.tz_localize(None) + pd.Timedelta(hours=16, minutes=25)).tz_localize(NY_TZ), index=days
    )
    dec = market.decision_instants(DecisionTime.CLOSE)
    when.loc[market.index] = dec  # calendar instants (early closes) on development sessions
    vix = pd.Series(Exo({"VIX": vix_f}).asof("VIX", pd.DatetimeIndex(when.to_numpy())), index=days)
    b2 = ps.bins(ps.trailing_quantile(vix), 3)
    spy21 = close / close.shift(21) - 1
    idx = x.index
    return {
        "B1": ps.known_at_previous_decision(b1, idx),
        "B2": ps.known_at_previous_decision(b2, idx),
        "region21": ps.known_at_previous_decision(np.sign(x.rolling(21).sum()), idx),
        "spy21": ps.known_at_previous_decision(np.sign(spy21), idx),
    }


def cond_section(data: Data, x: pd.Series, res, market: Market) -> str:
    st = states(data, x, res, market)
    sign = {-1.0: "negative", 0.0: "zero", 1.0: "positive"}
    parts = [
        ("B1 — 21-session realized vol, quintile vs trailing 504 (Q4–Q5 = §6.4's favourable set)", "B1",
         {float(i): f"Q{i}" for i in range(1, 6)}),
        ("B2 — VIX at the decision, tercile vs trailing 504", "B2", {1.0: "T1 low", 2.0: "T2", 3.0: "T3 high"}),
        ("Prior 21 sessions: sign of the region's net excess", "region21", sign),
        ("Prior 21 sessions: sign of SPY's total return", "spy21", sign),
    ]  # fmt: skip
    out = []
    for title, k, lab in parts:
        out += [f"**{title}**", "", period_table(ps.cond_table(x, st[k], lab)), ""]
    days = pd.Series(pd.DatetimeIndex(x.index).day_name(), index=x.index)  # ty: ignore[unresolved-attribute]
    out += ["**Weekday of the return session**", "", period_table(ps.cond_table(x, days)), ""]
    side = splits(res, market)["side_leg"]
    out += ["**Long vs short side and legs (gross, first order; as U6 §4)**", "",
            table(["part", "n", "bp/day", "t NW"], [[r["bin"], r["n"], f2(r["mean_bp_day"]), f2(r["t_nw"], sign=False)]
                                                    for r in side]), ""]  # fmt: skip
    return "\n".join(out)


def coherence_section(streams: dict[str, pd.Series]) -> str:
    rows = ps.coherence_by_year(streams)
    out = [table(["year", *streams, "others with SPY's sign"],
                 [[r["year"], *[f2(r[s]) for s in streams], f"{r['same_sign']} / {len(streams) - 1}"] for r in rows])]  # fmt: skip
    corr = pd.DataFrame(streams).corr()
    out += ["", "Correlation of the daily net excess streams:", "",
            table(["", *corr.columns], [[i, *[f2(v, 2, sign=False) for v in corr.loc[i]]] for i in corr.index]), ""]  # fmt: skip
    return "\n".join(out)


# ── main ────────────────────────────────────────────────────────────────────


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--vintage", default=None)
    ap.add_argument("--synthetic", action="store_true")
    ap.add_argument("--scratch", type=Path)
    ap.add_argument("--out", type=Path, default=RESULTS_DIR / "reports" / "U07_persistence.md")
    a = ap.parse_args(argv)
    if a.synthetic == (a.vintage is not None):
        ap.error("give exactly one of --vintage and --synthetic")
    if a.synthetic:
        if a.scratch is None:
            ap.error("--synthetic needs --scratch")
        a.scratch.mkdir(parents=True, exist_ok=True)
        looks_path = a.scratch / "looks.jsonl"
        shutil.copyfile(LOOKS_PATH, looks_path)
        out = a.scratch / "U07_persistence.md"
    else:
        looks_path, out = LOOKS_PATH, a.out
    k0 = looks_total(looks_path)
    data = Data(None if a.synthetic else a.vintage)
    spec = load_spec(SPEC)
    rows = None if a.synthetic else Ledger(RESULTS_DIR / "ledger.jsonl").rows()
    results, markets = h1_streams(data, spec, rows)  # computed, not printed: printed only inside looks below
    xs = {s: r.excess.iloc[1:] for s, r in results.items()}
    x, res, mkt = xs["SPY"], results["SPY"], markets["SPY"]
    for s in xs.values():
        check_window(s.index, "dev")
    dg = diag_stream(data)
    check_window(dg.index, "diag")

    def look(label, n, window, fn):
        return looked(f"U7 {label}", n, fn, window=window, path=looks_path)

    sec = {}

    def dev_periods():
        last5 = ps.period_row(x[ps._years(x) >= 2021], "2021–2025 (last 5 years; the summary line)")
        text = (periods_section(x, "H1 stream, net excess @1.0 bp") + "\n**H1 stream, net excess @1.0 bp — last "
                "5 years**\n\n" + period_table([last5]) + "\n\n" +
                periods_section(res.ledger["gross"].iloc[1:], "H1 stream, gross"))  # fmt: skip
        return text, last5

    sec["dev_periods"], last5 = look("dev by-year + rolling 3y + last 5y (H1 stream)", 1, "dev", dev_periods)
    sec["diag_periods"] = look("diag by-year + rolling 3y (R0 1993-2015)", 1, "diag",
                               lambda: periods_section(dg["gross"], "diag R₀, gross") + "\n" +
                               periods_section(dg["excess_hist"], "diag R₀, net excess at the historical schedule"))  # fmt: skip
    sec["dev_bp"], bp_dev = look(
        "dev Bai-Perron (H1 stream)", 1, "dev", lambda: bp_section(x, "H1 stream, net excess")
    )
    sec["diag_bp"], _ = look("diag Bai-Perron (R0 1993-2015)", 1, "diag",
                             lambda: bp_section(dg["excess_hist"], "diag R₀, net excess at the historical schedule"))  # fmt: skip
    sec["kalman"], kal = look("dev Kalman tracker (H1 stream)", 1, "dev", lambda: kalman_section(x))
    sec["cond"] = look("dev conditional tables (B1, B2, region21, spy21, weekday, side)", 6, "dev",
                       lambda: cond_section(data, x, res, mkt))  # fmt: skip
    sec["coh"] = look("dev cross-instrument coherence (QQQ, IWM, DIA by year)", 3, "dev",
                      lambda: coherence_section(xs))  # fmt: skip
    k1 = looks_total(looks_path)
    seq_p = bp_dev["sequential"][0]["p"]
    summary = (f"last-5-year (2021–2025) Newey–West t {last5['t_nw']:.2f} ({last5['mean_bp_day']:+.2f} bp/day); Bai–Perron "
               f"{bp_dev['n_breaks']} break(s) (supF(1) p {seq_p:.3f}, UDmax p {bp_dev['UDmax_p']:.3f}); "
               f"P(μ_T > 0) = {kal['P_mu_T_pos']:.3f} at q̂ ({kal['P_mu_T_pos_q0']:.3f} at q = 0; causal track "
               f"{kal['track_P_T']:.3f}), q_zero {kal['q_zero']}")  # fmt: skip
    sched = dg["rt_bp"]
    L = ["# U7 — H2 persistence: is the edge still there? (PLAN.md §6.3)", "",
         (f"Looks: K {k0} → **{k1}** (every window). Development-window stream: the H1 headline (R₀ on SPY, "
         f"`next_open`, {spec['cost']['registered_bp']} bp round trip, fees, borrow, DTB3), "
         f"{x.index[0].date()} → {x.index[-1].date()}, n {len(x)}"
         f"{'; rebuilt and equal to its H1 ledger row (and QQQ, IWM, DIA to theirs) to 1e-9 bp/day' if rows else ''}. "
         f"`diag` stream: R₀ on SPY {dg.index[0].date()} → {dg.index[-1].date()} (n {len(dg)}), `next_open`, gross "
         f"and at the historical cost schedule (one tick / previous close, $1/16 to 2001-04-09 then $0.01, floor 1.0 bp; "
         f"mean {sched.mean():.1f} bp, {sched.iloc[0]:.1f} bp at the start, no fees; an assumption, stated in "
         f"`experiments/persistence.py`). No conditional table uses `diag` (H3's test sample under the §6.2 "
         f"routing). Intervals: two-sided 90 % block bootstrap (block 21, 5,000, seed 0)."), "",
         f"**Summary line (§6.3):** {summary}.", "",
         "## 1. Edge by period", "", sec["dev_periods"], sec["diag_periods"],
         "## 2. Structural breaks in the mean (Bai–Perron, HAC)", "", sec["dev_bp"], sec["diag_bp"],
         "## 3. Kalman edge tracker (local level) — is it true today?", "", sec["kalman"],
         "## 4. Conditional tables (development window; reported, never used to change any F_S of §6.4)", "",
         sec["cond"], "## 5. Cross-instrument coherence (R₀ at the registered cost)", "", sec["coh"]]  # fmt: skip
    out.write_text("\n".join(L), encoding="utf-8")
    (out.with_suffix(".json")).write_text(json.dumps({"summary": summary, "K": k1}, indent=1))
    print(f"report {out}; K {k0} -> {k1}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
