"""H2 persistence tables (U7, PLAN.md §6.3): edge by period with block-bootstrap intervals, the `diag` historical
cost schedule, the causal state variables of the conditional tables, and cross-instrument coherence.

Conventions: daily return streams on the return-session index (row t = session t's return under `next_open`, earned
by the decisions of t−2 and t−1). A state used to condition row t is the state **known at decision t−1** (its
16:25 close), so every conditional table is causal. Intervals are two-sided 90 % studentized circular block
bootstraps (block 21, 5,000 resamples, seed 0, `stats.bootstrap.mean_test`); a period shorter than two blocks
gets the Newey–West t only.

**Historical cost schedule (`diag` only; an assumption, stated, not measured).** Spread + slippage per round trip
is one minimum tick divided by the session's close: $1/16 until 2001-04-09 (the end of the SEC's decimalization
phase-in for US equity markets), $0.01 after; floored at the registered 1.0 bp. On SPY that is ≈ 14 bp in 1993,
≈ 4–5 bp in 1999–2000, and the 1.0 bp floor from 2001. Fees are not charged on `diag` (U5's convention), borrow
and the T-bill are. With fees off the engine's daily growth is m·(1 − c_t)·after, so the stream at a session-varying
one-way rate c_t is exactly (1 + net₀)(1 − traded_t·c_t) − 1, with net₀ the zero-spread stream."""

import numpy as np
import pandas as pd

from vwap_mr.experiments.diagnose import nw_mean
from vwap_mr.stats.bootstrap import mean_test

DECIMAL = pd.Timestamp("2001-04-09")
BOOT = {"block": 21, "n_boot": 5000, "seed": 0}


def historical_rt_bp(close_raw: pd.Series, floor_bp: float = 1.0) -> pd.Series:
    """Round-trip spread + slippage in bp per session: one tick / close, floored (module docstring)."""
    idx = pd.DatetimeIndex(close_raw.index)
    naive = idx.tz_convert("America/New_York").tz_localize(None) if idx.tz is not None else idx
    tick = np.where(naive < DECIMAL, 1 / 16, 0.01)
    return pd.Series(np.maximum(floor_bp, tick / close_raw.to_numpy(float) * 1e4), index=close_raw.index)


def at_cost(net0: pd.Series, traded: pd.Series, rt_bp: pd.Series) -> pd.Series:
    """The fee-free stream at a session-varying round-trip cost (exact; module docstring)."""
    return (1 + net0) * (1 - traded * rt_bp / 2 * 1e-4) - 1


def period_row(x: pd.Series, label: str) -> dict:
    m, t = nw_mean(x) if len(x) > 10 else (float(x.mean()), float("nan"))
    row = {"period": label, "n": len(x), "mean_bp_day": m * 1e4, "t_nw": t, "ci90": [float("nan")] * 2}
    if len(x) >= 2 * BOOT["block"] and x.std() > 0:
        ci = mean_test(x, alpha=0.10, sided="two", **BOOT)["ci"]
        row["ci90"] = [ci[0] * 1e4, ci[1] * 1e4]
    return row


def _years(x: pd.Series | pd.DataFrame) -> np.ndarray:
    return pd.DatetimeIndex(x.index).year.to_numpy()  # ty: ignore[unresolved-attribute]


def by_year(x: pd.Series) -> list[dict]:
    return [period_row(g, str(y)) for y, g in x.groupby(_years(x))]


def rolling_years(x: pd.Series, span: int = 3) -> list[dict]:
    """Windows of `span` calendar years, stepped by one year (a window must hold all its years)."""
    yr = _years(x)
    years = sorted(set(yr.tolist()))
    return [period_row(x[(yr >= y) & (yr < y + span)], f"{y}–{y + span - 1}")
            for y in years if y + span - 1 <= years[-1]]  # fmt: skip


def trailing_quantile(s: pd.Series, window: int = 504) -> pd.Series:
    """Share of the last `window` values (today included) at or below today's: a causal percentile; NaN until
    `window` values exist."""
    return s.rolling(window).apply(lambda w: float(np.mean(w <= w[-1])), raw=True)


def bins(q: pd.Series, k: int) -> pd.Series:
    """Quantile bucket 1 … k of a percentile in (0, 1]; NaN stays NaN."""
    return pd.Series(np.ceil(q * k).clip(1, k), index=q.index)


def known_at_previous_decision(state: pd.Series, index: pd.Index) -> pd.Series:
    """Align a state stamped at session t's close decision to the return rows: row t gets the state of t−1."""
    return pd.Series(state.shift(1).reindex(index))


def cond_table(x: pd.Series, key: pd.Series, labels: dict | None = None) -> list[dict]:
    """Mean of x by the value of `key` (aligned to x; NaN keys dropped and counted in the row 'no state')."""
    k = key.reindex(x.index)
    rows = [period_row(g, (labels or {}).get(v, str(v))) for v, g in x[k.notna()].groupby(k[k.notna()])]
    if k.isna().any():
        rows.append({"period": "no state (warm-up)", "n": int(k.isna().sum()), "mean_bp_day": float("nan"),
                     "t_nw": float("nan"), "ci90": [float("nan")] * 2})  # fmt: skip
    return rows


def coherence_by_year(streams: dict[str, pd.Series]) -> list[dict]:
    """Per year: each instrument's mean bp/day and how many share the sign of the headline (the first stream)."""
    df = pd.DataFrame(streams)
    head = df.columns[0]
    out = []
    for y, g in df.groupby(_years(df)):
        m = g.mean() * 1e4
        out.append({"year": int(str(y)), **{c: float(m[c]) for c in df.columns},
                    "same_sign": int((np.sign(m) == np.sign(m[head])).sum())})  # fmt: skip
    return out
