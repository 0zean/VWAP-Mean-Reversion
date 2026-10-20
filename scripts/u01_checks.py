"""U1 done-when demonstrations (PLAN.md §7 U1): data conventions measured on the frozen vintage. No strategy P&L,
no return statistic: prints, volumes and adjustment factors only.

    uv run python scripts/u01_checks.py --vintage 2026-10 [--symbols SPY QQQ IWM DIA]

1. `1DayPrint` volume / RTH 5Min volume sum (does the print bar's volume include extended hours?).
2. `1DayPrint` close vs the last RTH 5Min close (bp): median |d|, p99, max, by year.
3. `1DayPrint` open / close vs Yahoo's (official consolidated) open / close, raw prices, by year.
4. Dividend adjustment: Alpaca `adjustment=all` factor (adjusted / raw open) vs Yahoo's (adjclose / close); every
   ex-date where the two factor steps differ by more than 2e-4.
"""

import argparse

import numpy as np
import pandas as pd

from vwap_mr.config import DEV_START, HOLDOUT_START
from vwap_mr.data.bars import load_bars
from vwap_mr.data.secondary import load_yahoo

pd.set_option("display.width", 200)
pd.set_option("display.max_columns", 30)


def q(x: pd.Series) -> str:
    return f"p1 {x.quantile(0.01):.4f}  p50 {x.median():.4f}  p99 {x.quantile(0.99):.4f}  min {x.min():.4f}  max {x.max():.4f}"


def main() -> None:
    p = argparse.ArgumentParser()
    p.add_argument("--vintage", required=True)
    p.add_argument("--symbols", nargs="+", default=["SPY", "QQQ", "IWM", "DIA"])
    a = p.parse_args()
    span = (DEV_START, HOLDOUT_START)
    for sym in a.symbols:
        print(f"\n## {sym}")
        pr = load_bars(sym, "1DayPrint", *span, vintage=a.vintage)
        rth = load_bars(sym, "1DayRTH", *span, vintage=a.vintage)
        raw = load_bars(sym, "1DayPrint", *span, vintage=a.vintage, adjustment="raw")
        c = pr.index.intersection(rth.index)
        print(f"sessions: 1DayPrint {len(pr)}, 1DayRTH {len(rth)}, common {len(c)}; "
              f"print-only {[str(d.date()) for d in pr.index.difference(rth.index)]}")  # fmt: skip

        vr = pr.loc[c, "volume"] / rth.loc[c, "volume"]
        print(f"1. volume ratio print/RTH: {q(vr)}")
        print(
            "   by year (median):",
            {int(y): round(float(v), 3) for y, v in vr.groupby(c.year).median().items()},
        )

        d = (pr.loc[c, "close"] / rth.loc[c, "close"] - 1) * 1e4
        print(f"2. print close vs last RTH 5Min close, bp: median |d| {d.abs().median():.2f}, "
              f"p99 |d| {d.abs().quantile(0.99):.2f}, max |d| {d.abs().max():.1f} on {d.abs().idxmax().date()}, "
              f"n |d| > 10 bp: {(d.abs() > 10).sum()}")  # fmt: skip
        print(
            "   median |d| bp by year:",
            {int(y): round(float(v), 2) for y, v in d.abs().groupby(c.year).median().items()},
        )

        try:
            y = load_yahoo(sym)
        except Exception as e:  # noqa: BLE001  (reported, not hidden: this symbol's check is skipped)
            print(f"3./4. skipped: Yahoo history unavailable ({type(e).__name__}: {e})")
            continue
        cy = raw.index.intersection(y.index)
        dco = (raw.loc[cy, "close"] - y.loc[cy, "close"]).abs()
        doo = (raw.loc[cy, "open"] - y.loc[cy, "open"]).abs()
        drth = (rth.loc[cy.intersection(rth.index), "close"] / pr.loc[cy.intersection(rth.index), "close"]
                * raw.loc[cy.intersection(rth.index), "close"] - y.loc[cy.intersection(rth.index), "close"]).abs()  # fmt: skip
        print(f"3. raw print vs Yahoo, sessions {len(cy)}: close |d| > 1.5c: {(dco > 0.015).sum()} "
              f"(max {dco.max():.2f} on {dco.idxmax().date()}); open |d| > 1.5c: {(doo > 0.015).sum()}; "
              f"last-RTH-5Min close vs Yahoo close |d| > 1.5c: {(drth > 0.015).sum()}")  # fmt: skip
        tab = pd.DataFrame({"close>1.5c": (dco > 0.015).groupby(cy.year).sum(),
                            "close_bp_med": (dco / y.loc[cy, "close"] * 1e4).groupby(cy.year).median(),
                            "close_bp_p99": (dco / y.loc[cy, "close"] * 1e4).groupby(cy.year).quantile(0.99),
                            "open>1.5c": (doo > 0.015).groupby(cy.year).sum()})  # fmt: skip
        print(tab.round(2).to_string())

        fa = pr.loc[cy, "open"] / raw.loc[cy, "open"]
        fy = y.loc[cy, "adjclose"] / y.loc[cy, "close"]
        steps = pd.DataFrame({"alpaca_step": fa.pct_change(), "yahoo_step": fy.pct_change()})
        steps["diff"] = steps["alpaca_step"] - steps["yahoo_step"]
        bad = steps[steps["diff"].abs() > 2e-4]
        n_ex = int((steps["yahoo_step"].abs() > 1e-5).sum())
        print(f"4. Yahoo ex-dates in window: {n_ex}; factor-step mismatches > 2e-4: {len(bad)}")
        if len(bad):
            print(bad.to_string())
        lvl = (fa / fy) / np.median(fa / fy) - 1
        print("   (Alpaca/Yahoo factor ratio - 1) median by year:",
              {int(k): f"{v:.1e}" for k, v in lvl.groupby(cy.year).median().items()})  # fmt: skip


if __name__ == "__main__":
    main()
