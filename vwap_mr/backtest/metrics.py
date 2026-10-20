"""Performance metrics on excess returns (U3, SPEC.md §3): Sharpe, Sortino, drawdown, PSR(0), turnover, beta/alpha.

Every ratio is on the daily **excess** return (net − the T-bill accrual, costs.py), annualized with 252 sessions.
Drawdowns are on the net equity path. Turnover is Σ|Δf| per year (one-way, fractions of equity); a round trip is
2 units of it."""

import numpy as np
import pandas as pd

from vwap_mr.stats.sharpe import psr_of

ANN = 252


def _years(index: pd.DatetimeIndex) -> float:
    return len(index) / ANN


def drawdown(net: pd.Series) -> tuple[float, int]:
    """(max drawdown as a positive fraction, longest underwater spell in sessions) of the equity path Π(1 + net)."""
    eq = np.cumprod(1 + net.to_numpy(float))
    peak = np.maximum.accumulate(np.r_[1.0, eq])[1:]
    dd = 1 - eq / peak
    under, longest = 0, 0
    for x in dd > 1e-15:
        under = under + 1 if x else 0
        longest = max(longest, under)
    return float(dd.max(initial=0.0)), longest


def summary(result, benchmark_excess: pd.Series | None = None) -> dict:
    """Headline numbers of one result (rows after the first session, which carries no return)."""
    lg = result.ledger.iloc[1:]
    x, net = lg["excess"], lg["net"]
    sd = x.std(ddof=1)
    down = np.sqrt(np.mean(np.minimum(x.to_numpy(), 0) ** 2))
    mdd, dur = drawdown(net)
    turnover = result.ledger["traded"].sum() / _years(lg.index)
    out = {
        "n_obs": len(lg),
        "ret_ann": float(x.mean() * ANN),
        "vol_ann": float(sd * np.sqrt(ANN)),
        "sharpe": float(x.mean() / sd * np.sqrt(ANN)) if sd > 0 else np.nan,
        "sortino": float(x.mean() / down * np.sqrt(ANN)) if down > 0 else np.nan,
        "psr0": psr_of(x) if sd > 0 else np.nan,
        "max_dd": mdd,
        "max_dd_sessions": dur,
        "turnover": float(turnover),
        "round_trips_yr": float(turnover / 2),
        "flat_share": float((result.ledger["exp_close"] == 0).mean()),
        "worst_day": float(net.min()),
        "cost_bp_day": float(lg["cost_ret"].mean() * 1e4),
        "mean_bp_day": float(x.mean() * 1e4),
    }
    if benchmark_excess is not None:
        b = benchmark_excess.reindex(lg.index)
        if b.isna().any():
            raise ValueError("benchmark has no return for some sessions of the result")
        beta, alpha = np.polyfit(b.to_numpy(), x.to_numpy(), 1)
        out |= {"beta": float(beta), "alpha_ann": float(alpha * ANN)}
    return out


def by_year(result) -> pd.DataFrame:
    """Per calendar year: sessions, annualized excess return, vol, Sharpe, max drawdown, turnover."""
    lg = result.ledger.iloc[1:]
    rows = {}
    for year, g in lg.groupby(lg.index.year):
        sd = g["excess"].std(ddof=1)
        rows[year] = {"n": len(g), "ret_ann": g["excess"].mean() * ANN, "vol_ann": sd * np.sqrt(ANN),
                      "sharpe": g["excess"].mean() / sd * np.sqrt(ANN) if sd > 0 else np.nan,
                      "max_dd": drawdown(g["net"])[0], "turnover": g["traded"].sum() / _years(g.index)}  # fmt: skip
    return pd.DataFrame.from_dict(rows, orient="index")
