"""U5: reproduce the inherited claim and diagnose the mechanism (PLAN.md §7 U5) -> results/reports/U05_diagnosis.md.

    uv run python scripts/u05_diagnose.py --vintage 2026-10
    uv run python scripts/u05_diagnose.py --synthetic --looks <tmp>/looks.jsonl --out <tmp>/U05.md   (dry run)

Every table is computed inside `looks.looked`, which appends its look row first (window "dev" for anything touching
2016-01-04 → 2025-12-31, "diag" for 1993–2015 only). Counting rule: n = the configurations a table reads
(strategy cell × execution, or statistic × instrument × window); splits of one configuration (years,
sub-periods, cost levels, legs, the data-source comparison) belong to its look. The only strategy P&L on the
development window is the inherited cell `VWAP.R` (n = 1); every other strategy number is on `diag` 1993–2015.

Seeds: every bootstrap uses seed 0 and 5,000 resamples, block 21. `--synthetic` replaces every loader with seeded
random data (AR(1) −0.05 returns) so the pipeline can be checked without reading a market result."""

import argparse
import sys
import time
from datetime import date
from pathlib import Path

import numpy as np
import pandas as pd

from vwap_mr.backtest.costs import CostModel, tbill_yield
from vwap_mr.backtest.engine import run_diagnostic
from vwap_mr.backtest.market import Market, load_market
from vwap_mr.config import DEV_START, HOLDOUT_START, RESULTS_DIR
from vwap_mr.data.alpaca import NY_TZ
from vwap_mr.experiments import diagnose as dg
from vwap_mr.experiments.looks import LOOKS_PATH, looked, looks_total
from vwap_mr.stats.bootstrap import mean_test, sharpe_test
from vwap_mr.stats.power import mde_mean
from vwap_mr.strategies.region import Region
from vwap_mr.strategies.vwap_mr import VWAP_R, r0, r0_cells

SYMBOLS = ("SPY", "QQQ", "IWM", "DIA")
README_SPAN = ("2007-01-03", "2023-01-06")  # quantmod's default from=2007-01-01; README committed 2023-01-09
SUBPERIODS = {"2007–09": ("2007", "2009"), "2010–15": ("2010", "2015"), "2016–25": ("2016", "2025")}
DIAG_SUBPERIODS = {"1993–2006": ("1993", "2006"), "2007–09": ("2007", "2009"), "2010–15": ("2010", "2015")}
DIAG_END = "2015-12-31"
COSTS = (0.0, 0.3, 1.0, 2.3)
REGISTERED_BP = 1.0
LAGS = (1, 2, 3, 4, 5)
VR_Q = (2, 5, 10)
N_STRUCT = len(LAGS) + len(VR_Q) + 3  # + leg means, corr(r_cc, next r_on), corr(r_cc, next r_id)
BOOT = {"n_boot": 5000, "block": 21, "seed": 0}


# ── data ────────────────────────────────────────────────────────────────────


def _tag(df: pd.DataFrame) -> pd.DataFrame:
    df.attrs["source"] = "diag"
    return df


def synthetic_frame(seed: int, start: str = "1993-01-29") -> pd.DataFrame:
    rng = np.random.default_rng(seed)
    idx = pd.bdate_range(start, "2025-12-31").tz_localize(NY_TZ)
    n = len(idx)
    e = rng.standard_normal(n) * 0.01
    r = np.empty(n)
    r[0] = e[0]
    for t in range(1, n):
        r[t] = -0.05 * r[t - 1] + e[t]
    close = 100 * np.cumprod(1 + r)
    opn = np.r_[100.0, close[:-1]] * (1 + rng.standard_normal(n) * 0.003)
    vol = rng.lognormal(18, 0.3, n)
    return _tag(pd.DataFrame({"open": opn, "close": close, "close_split": close, "volume": vol,
                              "is_diag": idx < pd.Timestamp(DEV_START, tz=NY_TZ)}, index=idx))  # fmt: skip


def synthetic_dtb3() -> pd.DataFrame:
    days = pd.bdate_range("1992-12-01", "2026-01-05")
    return pd.DataFrame(
        {"value": 2.0, "available_at": (days + pd.Timedelta(hours=1)).tz_localize(NY_TZ)}, index=days
    )


class Data:
    """Loaders: the frozen vintage, or seeded synthetic stand-ins (`--synthetic`)."""

    def __init__(self, vintage: str | None):
        self.vintage, self.reports, self.fallbacks, self._dev = vintage, {}, {}, {}

    def long(self, sym: str) -> pd.DataFrame:
        """`load_long`, with a missing cross from 2016 replaced by the session's RTH 5Min open / close exactly as
        `load_market` does for the engine (counted in `fallbacks`, printed in the report)."""
        if self.vintage is None:
            return synthetic_frame(SYMBOLS.index(sym))
        from vwap_mr.data.secondary import load_long

        frame, rep = load_long(sym, self.vintage)
        self.reports[sym] = rep
        s = self.dev_market(sym).sessions
        gaps = frame.index[frame[["open", "close", "close_split"]].isna().any(axis=1).to_numpy()]
        if not gaps.isin(s.index).all():
            raise ValueError(f"{sym}: missing prints outside the Alpaca window: {list(gaps)}")
        for d in gaps:
            close_raw = (
                s.at[d, "close_raw"] if np.isfinite(s.at[d, "close_raw"]) else s.at[d, "close_fallback_raw"]
            )
            frame.loc[d, ["open", "close", "close_split"]] = [s.at[d, "open"], s.at[d, "close"], close_raw]
        self.fallbacks[sym] = [str(d.date()) for d in gaps]
        return frame

    def yahoo(self) -> pd.DataFrame:
        """SPY from Yahoo alone, as `VWAP.R` downloaded it: `close_split` = Yahoo's close, `close` = its adjclose."""
        if self.vintage is None:
            return synthetic_frame(9)
        from vwap_mr.data.secondary import load_yahoo

        raw = load_yahoo("SPY")
        return _tag(pd.DataFrame({"open": raw["open"] * raw["adjclose"] / raw["close"], "close": raw["adjclose"],
                                  "close_split": raw["close"], "volume": raw["volume"]}))  # fmt: skip

    def dtb3(self) -> pd.DataFrame:
        if self.vintage is None:
            return synthetic_dtb3()
        from vwap_mr.data.exo import load_series

        return load_series("DTB3", date(1992, 12, 1), HOLDOUT_START, vintage=self.vintage)

    def dev_market(self, sym: str) -> Market:
        if self.vintage is None:
            f = synthetic_frame(SYMBOLS.index(sym)).loc[str(DEV_START) :]
            s = pd.DataFrame({"open_raw": f["open"], "close_raw": f["close"], "dividend": 0.0, "open": f["open"],
                              "close": f["close"], "volume": f["volume"]})  # fmt: skip
            return Market(s, "synthetic", symbol=sym)
        if sym not in self._dev:
            self._dev[sym] = load_market(sym, DEV_START, HOLDOUT_START, vintage=self.vintage)
        return self._dev[sym]


# ── formatting ──────────────────────────────────────────────────────────────


def bp(x: float) -> str:
    return f"{x * 1e4:+.2f}"


def ci_bp(ci) -> str:
    return f"[{ci[0] * 1e4:+.2f}, {ci[1] * 1e4:+.2f}]"


def table(header: list[str], rows: list[list]) -> str:
    out = ["| " + " | ".join(header) + " |", "|" + "---|" * len(header)]
    out += ["| " + " | ".join(str(c) for c in r) + " |" for r in rows]
    return "\n".join(out)


def mean_row(x: pd.Series) -> dict:
    m = mean_test(x.to_numpy(), sided="two", alpha=0.10, **BOOT)
    return {"bp": m["mean_bp_day"], "ci": m["ci"], "t_nw": m["t_nw"], "p2": m["p"], "n": m["n_obs"]}


def sharpe_row(x: pd.Series) -> dict:
    s = sharpe_test(x.to_numpy(), sided="two", alpha=0.05, **BOOT)
    k = np.sqrt(252)
    return {"sr": s["delta_ann"], "ci": (s["ci"][0] * k, s["ci"][1] * k)}


# ── sections ────────────────────────────────────────────────────────────────


def readme_section(data: Data) -> str:
    """The inherited cell, `same_close`, zero cost, from 2007, Yahoo alone and spliced; sub-periods."""
    rows, sub_rows = [], []
    sources = {
        "Yahoo alone (as VWAP.R read it)": data.yahoo(),
        "spliced (Alpaca prints + volume from 2016)": data.long("SPY"),
    }
    for label, frame in sources.items():
        frame = _tag(frame.loc["2007-01-01":])
        for basis in ("close_split", "close"):
            m = dg.diag_market(frame, price=basis)
            st = dg.vwap_r_streams(m, VWAP_R.targets(m))
            eng = run_diagnostic(m, VWAP_R, fill="same_close", costs=CostModel(0.0, borrow_rate=0.0, fees=False),
                                 cash_yield=0.0)  # fmt: skip
            # the engine's same_close gross is VWAP.R's arithmetic (row t+1 = f_t·r_{t+1})
            gap = float((eng.ledger["gross"].shift(-1).loc[st.index] - st["strategy"]).abs().max())
            if gap > 1e-12:
                raise AssertionError(f"engine same_close differs from the VWAP.R arithmetic by {gap}")
            spans = {"README span " + "→".join(README_SPAN): README_SPAN, "2007 → 2025": ("2007", "2025")}
            for span_label, (a, b) in spans.items():
                w = st.loc[a:b]
                d = sharpe_test(
                    w["strategy"].to_numpy(), w["index"].to_numpy(), sided="two", alpha=0.05, **BOOT
                )
                rows.append([label, "split-adjusted close (R)" if basis == "close_split" else "total return",
                             span_label, len(w), f"{dg.r_sharpe(w['strategy']):.3f}", f"{dg.r_sharpe(w['index']):.3f}",
                             f"{d['delta_ann']:+.3f} (p {d['p']:.3f})"])  # fmt: skip
            if basis == "close_split":
                for name, (a, b) in SUBPERIODS.items():
                    w = st.loc[a:b]
                    s, i = sharpe_row(w["strategy"]), sharpe_row(w["index"])
                    mr = mean_row(w["strategy"])
                    sub_rows.append([label, name, len(w), f"{dg.r_sharpe(w['strategy']):.3f}",
                                     f"{s['sr']:.2f} [{s['ci'][0]:.2f}, {s['ci'][1]:.2f}]",
                                     f"{dg.r_sharpe(w['index']):.3f}", f"{i['sr']:.2f} [{i['ci'][0]:.2f}, {i['ci'][1]:.2f}]",
                                     f"{mr['bp']:+.2f} {ci_bp(mr['ci'])}", f"{mr['t_nw']:.2f}"])  # fmt: skip
    out = [
        (
            "README: Sharpe 0.468 (strategy) vs 0.314 (SPY), PerformanceAnalytics `SharpeRatio.annualized` defaults "
            "(geometric, Rf 0). Columns `R Sharpe` use that formula; Δ is the arithmetic Sharpe difference, paired "
            "Ledoit–Wolf, two-sided p."
        ),
        "",
        table(
            ["data", "price basis", "span", "T", "R Sharpe strategy", "R Sharpe SPY", "ΔSharpe ann (LW)"],
            rows,
        ),
        "",
        (
            "Sub-periods (split-adjusted close, the R convention; signals warmed from 2007). Arithmetic Sharpe with its "
            "two-sided 95 % Ledoit–Wolf interval; mean bp/day with its two-sided 90 % block-bootstrap interval."
        ),
        "",
        table(
            [
                "data",
                "period",
                "T",
                "R Sharpe",
                "Sharpe [95 %]",
                "SPY R Sharpe",
                "SPY Sharpe [95 %]",
                "strategy bp/day [90 %]",
                "t_NW",
            ],
            sub_rows,
        ),
    ]
    return "\n".join(out)


def diag_cell_section(data: Data) -> str:
    """The inherited cell on diag 1993–2015 under next_open and same_close (the moc stand-in) at the cost curve."""
    frame = _tag(data.long("SPY").loc[:DIAG_END])
    m = dg.diag_market(frame, dtb3=data.dtb3())
    y = tbill_yield(m)
    rows, leg_rows = [], []
    for fill in ("next_open", "same_close"):
        results = [
            run_diagnostic(m, VWAP_R, fill=fill, costs=CostModel(c, fees=False), cash_yield=y) for c in COSTS
        ]
        for c, res in zip(COSTS, results, strict=True):
            x = res.excess.iloc[1:]
            mr, sr = mean_row(x), sharpe_row(x)
            rows.append([fill if fill == "next_open" else "same_close (moc stand-in)", f"{c:g}",
                         f"{mr['bp']:+.2f} {ci_bp(mr['ci'])}", f"{mr['t_nw']:.2f}",
                         f"{sr['sr']:.2f} [{sr['ci'][0]:.2f}, {sr['ci'][1]:.2f}]",
                         f"{res.ledger['traded'].sum() / (len(x) / 252) / 2:.0f}"])  # fmt: skip
        if fill == "next_open":
            contrib = dg.leg_contributions(results[0])
            for col, what in (("on_same_close", "f_t · r_on(t+1): overnight leg a same-close fill earns"),
                              ("intraday", "f_t · r_id(t+1): intraday leg, both fills"),
                              ("on_next_open", "f_{t−1} · r_on(t+1): stale overnight leg next_open holds")):  # fmt: skip
                lr = mean_row(contrib[col])
                leg_rows.append([what, f"{lr['bp']:+.2f} {ci_bp(lr['ci'])}", f"{lr['t_nw']:.2f}"])
    sub = []
    for name, (a, b) in DIAG_SUBPERIODS.items():
        for fill in ("next_open", "same_close"):
            res = run_diagnostic(
                m, VWAP_R, fill=fill, costs=CostModel(REGISTERED_BP, fees=False), cash_yield=y
            )
            w = res.ledger.loc[a:b]
            gr, ex = mean_row(w["gross"]), mean_row(w["excess"])
            sub.append([name, fill, len(w), f"{gr['bp']:+.2f} {ci_bp(gr['ci'])}", f"{ex['bp']:+.2f} {ci_bp(ex['ci'])}",
                        f"{ex['t_nw']:.2f}"])  # fmt: skip
    out = [
        (
            f"SPY `diag` {frame.index[0].date()} → {frame.index[-1].date()} (total-return prices, Yahoo opens / closes), "
            "excess of the DTB3 cash yield, borrow 0.4 %/yr, no SEC/TAF (the fee table starts 2015). `same_close` "
            "stands in for `moc`: `diag` has no 15:45 snapshot, so it is the R fill (decision at the close it trades "
            "at), an optimistic bound for a 15:45 decision."
        ),
        "",
        table(
            ["fill", "round trip bp", "excess bp/day [90 %]", "t_NW", "Sharpe [95 %]", "round trips/yr"], rows
        ),
        "",
        "Where the return comes from (gross, zero cost; per session, bp/day [90 %]):",
        "",
        table(["leg", "bp/day [90 %]", "t_NW"], leg_rows),
        "",
        "By sub-period at the registered 1.0 bp round trip:",
        "",
        table(["period", "fill", "T", "gross bp/day [90 %]", "excess bp/day [90 %]", "t_NW (excess)"], sub),
    ]
    return "\n".join(out)


def structure_stats(r: pd.Series, lg: pd.DataFrame) -> list:
    acs = [dg.autocorr(r, k) for k in LAGS]
    vrs = [dg.variance_ratio(r, q) for q in VR_Q]
    on, idd = dg.nw_mean(lg["r_on"]), dg.nw_mean(lg["r_id"])
    c_on = dg.cross_corr(lg["r_cc"], lg["r_on"])
    c_id = dg.cross_corr(lg["r_cc"], lg["r_id"])
    return ([f"{a:+.3f} ({a / s:+.1f})" for a, s in acs] + [f"{v:.3f} ({z:+.1f})" for v, z in vrs]
            + [f"{on[0] * 1e4:+.2f} ({on[1]:+.1f}) / {idd[0] * 1e4:+.2f} ({idd[1]:+.1f})",
               f"{c_on[0]:+.3f} ({c_on[0] / c_on[1]:+.1f})", f"{c_id[0]:+.3f} ({c_id[0] / c_id[1]:+.1f})"])  # fmt: skip


def structure_section(data: Data, sym: str, window: str) -> str:
    if window == "dev":
        s = data.dev_market(sym).sessions
    else:
        s = data.long(sym).loc[:DIAG_END]
    lg = dg.legs_of(s).iloc[1:]
    r = lg["r_cc"]
    head = [f"ρ{k} (t)" for k in LAGS] + [f"VR({q}) (z*)" for q in VR_Q] + ["on / id bp/day (t_NW)",
                                                                            "ρ(cc, next on) (t)", "ρ(cc, next id) (t)"]  # fmt: skip
    full = [["all", len(r)] + structure_stats(r, lg)]
    years = []
    for y, g in lg.groupby(lg.index.year):
        if len(g) < 100:
            continue
        years.append([y, len(g)] + structure_stats(g["r_cc"], g))
    span = f"{r.index[0].date()} → {r.index[-1].date()}"
    return "\n".join([(f"**{sym}, {'Alpaca' if window == 'dev' else 'diag'} {span}** (total-return legs; "
                      "t = robust ρ̂/se; z* = Lo–MacKinlay robust)."), "",
                      table(["year", "T"] + head, full + years)])  # fmt: skip


def r0_section(data: Data) -> tuple[str, dict]:
    """The 16 R₀ cells and the region on diag 1993–2015, next_open: gross, and net excess at 1.0 bp."""
    frame = _tag(data.long("SPY").loc[:DIAG_END])
    m = dg.diag_market(frame, dtb3=data.dtb3())
    y = tbill_yield(m)
    rows, region_res = [], None
    for strat in (*r0_cells(), r0()):
        res = run_diagnostic(
            m, strat, fill="next_open", costs=CostModel(REGISTERED_BP, fees=False), cash_yield=y
        )
        lg = res.ledger.iloc[1:]
        g, ex = mean_row(lg["gross"]), mean_row(lg["excess"])
        turn_day = lg["traded"].mean()
        be = 2 * lg["gross"].mean() / turn_day * 1e4 if turn_day > 0 else float("nan")
        rows.append([strat.name, f"{g['bp']:+.2f} {ci_bp(g['ci'])}", f"{g['t_nw']:.2f}",
                     f"{ex['bp']:+.2f} {ci_bp(ex['ci'])}", f"{turn_day * 252 / 2:.0f}", f"{be:.1f}",
                     f"{(res.ledger['exp_close'] == 0).mean():.2f}"])  # fmt: skip
        if isinstance(strat, Region):
            region_res = res
    assert region_res is not None
    lg = region_res.ledger.iloc[1:]
    pos = sum(1 for r in rows[:-1] if float(r[1].split()[0]) > 0)
    sub = []
    for name, (a, b) in DIAG_SUBPERIODS.items():
        w = lg.loc[a:b]
        g, ex = mean_row(w["gross"]), mean_row(w["excess"])
        sub.append([name, len(w), f"{g['bp']:+.2f} {ci_bp(g['ci'])}", f"{ex['bp']:+.2f} {ci_bp(ex['ci'])}",
                    f"{ex['t_nw']:.2f}", f"{w['excess'].std() * 1e4:.1f}"])  # fmt: skip
    out = [
        (
            f"SPY `diag` {frame.index[1].date()} → {frame.index[-1].date()}, `next_open` (the headline execution), "
            "VWAP from the total-return close × Yahoo volume; net excess at the registered 1.0 bp round trip, DTB3 "
            "cash, borrow 0.4 %/yr. Break-even = the round-trip cost (bp) at which the gross edge is spent."
        ),
        "",
        table(
            [
                "cell",
                "gross bp/day [90 %]",
                "t_NW",
                "net excess @1.0 bp [90 %]",
                "round trips/yr",
                "break-even bp/RT",
                "flat share",
            ],
            rows,
        ),
        "",
        f"Coherence: {pos} of 16 cells have a positive gross mean.",
        "",
        "Region by sub-period (σ = daily excess-return standard deviation, bp):",
        "",
        table(["period", "T", "gross bp/day [90 %]", "net excess @1.0 bp [90 %]", "t_NW", "σ_d bp"], sub),
    ]
    stats = {"n": len(lg), "full": mean_row(lg["excess"]), "full_gross": mean_row(lg["gross"]), "sigma": float(lg["excess"].std()),
             "recent": mean_row(lg.loc["2010":"2015", "excess"]), "coherence": pos}  # fmt: skip
    return "\n".join(out), stats


def magnitude_section(r0s: dict, dev_n: int, diag_n: int) -> str:
    full, recent, sigma = r0s["full"], r0s["recent"], r0s["sigma"]
    mde = mde_mean(dev_n, sigma)
    mde_long = mde_mean(dev_n + diag_n, sigma)
    from scipy.stats import norm

    def power(mu_bp: float, n: int) -> float:
        return float(norm.cdf(mu_bp * 1e-4 / (sigma / np.sqrt(n)) - norm.ppf(0.95)))

    rows = [
        ["R₀ net excess @1.0 bp, diag 1993–2015", f"{full['bp']:+.2f} {ci_bp(full['ci'])}",
         f"{power(full['bp'], dev_n):.2f}"],
        ["R₀ net excess @1.0 bp, diag 2010–15", f"{recent['bp']:+.2f} {ci_bp(recent['ci'])}",
         f"{power(recent['bp'], dev_n):.2f}"],
    ]  # fmt: skip
    return "\n".join([
        (f"σ_d of the R₀ region's daily excess return on diag: {sigma * 1e4:.1f} bp. At n = {dev_n} development "
        f"sessions, one-sided α 0.05, 80 % power: **MDE {mde['mde_bp_day']:.2f} bp/day** "
        f"(SE {mde['se_bp_day']:.2f} bp; Sharpe {mde['mde_sharpe_ann']:.2f}); on diag + Alpaca (n {dev_n + diag_n}): "
        f"{mde_long['mde_bp_day']:.2f} bp/day."),
        "",
        table(["candidate expected magnitude", "bp/day [90 %]", "power at n_dev"], rows),
    ])  # fmt: skip


# ── main ────────────────────────────────────────────────────────────────────


def main(argv=None) -> int:
    p = argparse.ArgumentParser()
    g = p.add_mutually_exclusive_group(required=True)
    g.add_argument("--vintage")
    g.add_argument("--synthetic", action="store_true")
    p.add_argument("--looks", type=Path, default=LOOKS_PATH)
    p.add_argument("--out", type=Path, default=RESULTS_DIR / "reports" / "U05_diagnosis.md")
    a = p.parse_args(argv)
    if a.synthetic and a.looks == LOOKS_PATH:
        p.error("--synthetic must not write the real looks ledger; pass --looks")
    data = Data(None if a.synthetic else a.vintage)
    t0 = time.time()
    k0, k0_all = looks_total(a.looks), looks_total(a.looks, window=None)
    sec = {}

    def look(label, n, window, fn, note=""):
        print(f"[U5] look: {label} (n {n}, {window})", flush=True)
        return looked(f"U5 {label}", n, fn, window=window, note=note, path=a.looks)

    sec["readme"] = look("VWAP.R reproduction from 2007 (inherited cell, same_close, 0 cost; sub-periods)", 1,
                         "dev", lambda: readme_section(data),
                         "the single permitted strategy-P&L look on the development window (PLAN §7 U5)")  # fmt: skip
    sec["diag_cell"] = look("VWAP.R cell on diag 1993-2015: next_open, same_close x cost curve, legs", 2,
                               "diag", lambda: diag_cell_section(data))  # fmt: skip
    struct = []
    for sym in SYMBOLS:
        for window in ("diag", "dev"):
            struct.append(look(f"reversal structure {sym} {window}", N_STRUCT, window,
                               lambda s=sym, w=window: structure_section(data, s, w)))  # fmt: skip
    sec["r0"], r0s = look("R0 specification curve on diag 1993-2015, next_open (16 cells + region)", 17, "diag",
                          lambda: r0_section(data))  # fmt: skip
    dev_n = len(data.dev_market("SPY").sessions) - 1
    mag = magnitude_section(r0s, dev_n, r0s["n"])
    k1, k1_all = looks_total(a.looks), looks_total(a.looks, window=None)
    splice = "\n".join(f"- {s}: overlap {r['n_overlap']} sessions, adjusted-close corr {r['adjusted_close_corr']:.8f}, "
                       f"1DayPrint / Yahoo volume median {r['volume_ratio_print_vs_yahoo_median']:.4f}; missing "
                       f"crosses from 2016 replaced by the RTH 5Min open / close (as `load_market`): "
                       f"{data.fallbacks.get(s) or 'none'}"
                       for s, r in data.reports.items())  # fmt: skip
    doc = f"""# U5 — Reproduce the inherited claim and diagnose the mechanism

{"**SYNTHETIC DRY RUN — no market data.**" if a.synthetic else ""}
Generated by `scripts/u05_diagnose.py` ({"--synthetic" if a.synthetic else "--vintage " + a.vintage}) in
{time.time() - t0:.0f} s. Looks: **K (development window) {k0} → {k1}**; all windows {k0_all} → {k1_all}
(counting rule in the script docstring; every table below was recorded in `{a.looks.name}` before it was computed).
Intervals: means two-sided 90 % studentized circular block bootstrap (block 21, 5,000, seed 0); Sharpe ratios
two-sided 95 % Ledoit–Wolf; t_NW = Newey–West (5 lags).

## 0. Data

{splice or "- synthetic"}

## 1. The README claim (inherited cell n = 3, b = 0.1 %, VWAP from close × volume, `same_close`, zero cost)

{sec["readme"]}

## 2. The honest version before 2016 (`diag` only)

{sec["diag_cell"]}

## 3. Reversal structure of daily index returns (no strategy)

{chr(10).join(s + chr(10) for s in struct)}
## 4. The R₀ specification curve on `diag` 1993–2015 (gross and net; R₀ itself unchanged)

{sec["r0"]}

## 5. H1's expected magnitude and the MDE (inputs to U6's spec)

{mag}
"""
    a.out.parent.mkdir(parents=True, exist_ok=True)
    a.out.write_text(doc, encoding="utf-8")
    print(f"[U5] wrote {a.out} (K {k0} -> {k1}; all {k0_all} -> {k1_all})")
    return 0


if __name__ == "__main__":
    sys.exit(main())
