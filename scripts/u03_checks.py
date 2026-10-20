"""U3 done-when demonstrations on the frozen vintage (PLAN.md §7 U3). **No strategy P&L is read**: targets here are a
constant long, a seeded random ±1 and a deterministic function of the view's length and VIX, and the script prints
only equality residuals, counts, tradability and runtimes, never a return, Sharpe or equity level (PLAN.md §0).

    uv run python scripts/u03_checks.py --vintage 2026-10 [--symbols SPY QQQ IWM DIA]

1. load_market: sessions, missing crosses replaced by the RTH 5Min fallback (counted), ex-dates, early closes.
2. DTB3 as of every 16:25 decision is visible and fresh (≤ 10 days) over the window.
3. Always-long at zero cost = an independent share ledger (both fills) and the adjusted close (moc): max rel. error.
4. Vectorized engine = the per-session dollar loop at registered costs with fees, borrow and the T-bill.
5. Vectorized targets = the per-session DecisionView loop, for a view-dependent target using exo.
6. Account: a ±1 target is tradable on margin_30k and refused on cash_30k.
7. Fee-table rows used by the window's sessions.
"""

import argparse
import time
from dataclasses import dataclass

import numpy as np
import pandas as pd

from vwap_mr.backtest import account
from vwap_mr.backtest.costs import ZERO, CostModel, fee_rates, tbill_yield
from vwap_mr.backtest.engine import run
from vwap_mr.backtest.market import load_market
from vwap_mr.config import DEV_START, HOLDOUT_START, DecisionTime
from vwap_mr.strategies.base import target_series


@dataclass(frozen=True)
class Given:
    f: tuple
    decision: DecisionTime = DecisionTime.CLOSE
    fill: str = "next_open"
    name: str = "given"

    def target(self, view) -> float:
        return float(self.f[len(view.sessions) - 1 + (self.decision == DecisionTime.T1545)])

    def targets(self, market):
        return pd.Series(self.f, index=market.index, dtype=float)


@dataclass(frozen=True)
class ViewProbe:
    """Not a trading rule: sin(number of visible sessions) scaled by VIX as of the decision, clipped to [−1, 1]."""

    decision: DecisionTime = DecisionTime.CLOSE
    fill: str = "next_open"
    name: str = "view_probe"

    def target(self, view) -> float:
        return float(np.clip(np.sin(len(view.sessions)) * view.exo.asof("VIX") / 20, -1, 1))

    def targets(self, market):
        vix = market.exo.asof("VIX", market.decision_instants(self.decision))
        return pd.Series(
            np.clip(np.sin(np.arange(1, len(market.index) + 1)) * vix / 20, -1, 1), index=market.index
        )


def bh(m, fill: str) -> np.ndarray:
    o, c, d = (m.sessions[k].to_numpy() for k in ("open_raw", "close_raw", "dividend"))
    o = np.where(
        np.isnan(o), m.sessions.get("open_fallback_raw", pd.Series(np.nan, index=m.index)).to_numpy(), o
    )
    c = np.where(
        np.isnan(c), m.sessions.get("close_fallback_raw", pd.Series(np.nan, index=m.index)).to_numpy(), c
    )
    eq, shares = np.empty(len(c)), (1 / c[0] if fill == "moc" else 1 / o[1])
    eq[0] = 1.0
    for i in range(1, len(c)):
        if d[i]:
            shares *= (o[i] + d[i]) / o[i] if fill == "next_open" else (c[i] + d[i]) / c[i]
        eq[i] = shares * c[i]
    return eq


def rel(a, b) -> float:
    a, b = np.asarray(a, float), np.asarray(b, float)
    return float(np.max(np.abs(a - b) / np.maximum(np.abs(b), 1e-300)))


def main() -> None:
    p = argparse.ArgumentParser()
    p.add_argument("--vintage", required=True)
    p.add_argument("--symbols", nargs="+", default=["SPY", "QQQ", "IWM", "DIA"])
    a = p.parse_args()
    for sym in a.symbols:
        print(f"\n=== {sym}")
        m = load_market(sym, DEV_START, HOLDOUT_START, vintage=a.vintage)
        s = m.sessions
        when = m.decision_instants(DecisionTime.CLOSE)
        early = sum(t.hour == 13 for t in when)
        print(f"1. sessions {len(s)} {s.index[0].date()} -> {s.index[-1].date()}; missing open cross "
              f"{int(s['open_raw'].isna().sum())}, close cross {int(s['close_raw'].isna().sum())}; ex-dates "
              f"{int((s['dividend'] > 0).sum())}; early-close decisions at 13:25: {early}")  # fmt: skip
        y = tbill_yield(m)
        print(f"2. DTB3 visible and fresh at all {len(y)} close decisions")
        for decision, fill in ((DecisionTime.CLOSE, "next_open"), (DecisionTime.T1545, "moc")):
            r = run(m, Given((1.0,) * len(s), decision, fill), costs=ZERO, cash_yield=0.0, equity0=1.0)
            line = f"3. always-long {fill}: equity vs share ledger max rel err {rel(r.ledger['equity'], bh(m, fill)):.2e}"
            if fill == "moc":
                adj = s["close"].to_numpy()
                line += f"; vs adjusted close {rel(r.ledger['equity'], adj / adj[0]):.2e}"
            print(line + f"; fallbacks {r.fallbacks}")
        f = tuple(np.random.default_rng(20261020).choice([-1.0, 1.0], len(s)))
        costs = CostModel()
        t0 = time.perf_counter()
        v = run(m, Given(f), costs=costs, cash_yield=y)
        t1 = time.perf_counter()
        lp = run(m, Given(f), costs=costs, cash_yield=y, vectorized=False)
        t2 = time.perf_counter()
        print(f"4. random ±1, registered costs: vectorized vs loop max |Δnet| "
              f"{np.abs(v.ledger['net'] - lp.ledger['net']).max():.2e}, equity max rel err "
              f"{rel(v.ledger['equity'], lp.ledger['equity']):.2e}, every ledger cell within 1e-12 rel + 1e-9 abs "
              f"{np.allclose(v.ledger, lp.ledger, rtol=1e-12, atol=1e-9)}; "
              f"runtime vectorized {1e3 * (t1 - t0):.1f} ms, loop {1e3 * (t2 - t1):.1f} ms")  # fmt: skip
        t0 = time.perf_counter()
        tv = target_series(ViewProbe(), m)
        t1 = time.perf_counter()
        tl = target_series(ViewProbe(), m, vectorized=False)
        t2 = time.perf_counter()
        print(f"5. view probe: vectorized vs DecisionView loop max |Δtarget| {np.abs(tv - tl).max():.2e}; "
              f"{1e3 * (t1 - t0):.1f} ms vs {1e3 * (t2 - t1):.0f} ms")  # fmt: skip
        print(
            f"6. account margin_30k: {account.check(v, 'margin_30k')}; cash_30k: {account.check(v, 'cash_30k')}"
        )
        fees = fee_rates(m.index)
        used = fees.groupby([fees["sec"] * 1e6, fees["taf"]]).size()
        print("7. fee rows used (SEC $/million, TAF $/share -> sessions):")
        print(used.to_string())


if __name__ == "__main__":
    main()
