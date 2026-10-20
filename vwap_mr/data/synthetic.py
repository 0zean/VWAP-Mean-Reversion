"""Seeded synthetic bars (U1/U8): GBM with Markov volatility regimes on an RTH 5Min grid, aggregated to daily with the
same code as real bars. Deterministic per seed; no network, no calendar fetch."""

import numpy as np
import pandas as pd

from vwap_mr.data.alpaca import BAR_COLUMNS, NY_TZ, calendar_from_raw
from vwap_mr.data.bars import resample_daily

BARS_PER_SESSION = 78  # 09:30-16:00 in 5Min bars


def synthetic_calendar(start: str, n_sessions: int) -> pd.DataFrame:
    """Weekday sessions 09:30-16:00 (no holidays, no early closes) in the calendar_from_raw format."""
    days = pd.bdate_range(start, periods=n_sessions)
    return calendar_from_raw(
        [{"date": d.strftime("%Y-%m-%d"), "open": "09:30", "close": "16:00"} for d in days]
    )


def gbm_5min(
    n_sessions: int,
    seed: int,
    *,
    start: str = "2016-01-04",
    s0: float = 100.0,
    mu_ann: float = 0.05,
    vols_ann: tuple[float, ...] = (0.12, 0.35),
    stay: float = 0.98,
    overnight_share: float = 0.2,
) -> tuple[pd.DataFrame, pd.DataFrame, np.ndarray]:
    """5Min RTH bars with a session-level Markov volatility regime.

    Each session's regime persists with probability `stay` per session (else a uniform draw among the others).
    A fraction `overnight_share` of the session variance falls in the overnight gap. Bar OHLC come from four
    sub-steps per bar; volume is lognormal with a U-shaped intraday profile; vwap is the mean of the sub-step prices.

    Returns:
        (bars, calendar, regime): bars with BAR_COLUMNS on a NY index; regime index per session.
    """
    rng = np.random.default_rng(seed)
    cal = synthetic_calendar(start, n_sessions)
    k = len(vols_ann)
    regime = np.empty(n_sessions, dtype=int)
    regime[0] = rng.integers(k)
    for t in range(1, n_sessions):
        regime[t] = (
            regime[t - 1]
            if rng.random() < stay or k == 1
            else rng.choice([j for j in range(k) if j != regime[t - 1]])
        )
    sig_d = np.asarray(vols_ann)[regime] / np.sqrt(252)
    sub = 4
    n_steps = BARS_PER_SESSION * sub
    step_sd = sig_d * np.sqrt((1 - overnight_share) / n_steps)
    on_sd = sig_d * np.sqrt(overnight_share)
    drift = mu_ann / 252 / (n_steps + 1)
    shocks = rng.standard_normal((n_sessions, n_steps)) * step_sd[:, None] + drift
    gaps = rng.standard_normal(n_sessions) * on_sd + drift
    logp = np.log(s0) + np.cumsum(np.concatenate([gaps[:, None], shocks], axis=1), axis=None).reshape(
        n_sessions, -1
    )
    path = np.exp(logp[:, 1:]).reshape(n_sessions, BARS_PER_SESSION, sub)
    first = np.exp(logp[:, :1]).reshape(n_sessions, 1, 1)
    prev = np.concatenate([first, path[:, :-1, -1:]], axis=1)  # each bar opens at the previous sub-step price
    pts = np.concatenate([prev, path], axis=2)
    u = np.linspace(-1, 1, BARS_PER_SESSION)
    vol = np.exp(rng.normal(np.log(2e4) + 0.8 * u**2, 0.3, (n_sessions, BARS_PER_SESSION)))
    stamps = pd.DatetimeIndex(
        [o + pd.Timedelta(minutes=5 * j) for o in cal["session_open"] for j in range(BARS_PER_SESSION)]
    )
    bars = pd.DataFrame(
        {
            "open": pts[:, :, 0].ravel(),
            "high": pts.max(axis=2).ravel(),
            "low": pts.min(axis=2).ravel(),
            "close": pts[:, :, -1].ravel(),
            "volume": np.round(vol).ravel(),
            "vwap": path.mean(axis=2).ravel(),
            "trade_count": np.full(n_sessions * BARS_PER_SESSION, 100.0),
        },
        index=stamps.tz_convert(NY_TZ).as_unit("ns"),
    )[BAR_COLUMNS]
    return bars, cal, regime


def gbm_daily(n_sessions: int, seed: int, **kw) -> tuple[pd.DataFrame, np.ndarray]:
    """`gbm_5min` aggregated to one bar per session (resample_daily); returns (daily, regime)."""
    bars, cal, regime = gbm_5min(n_sessions, seed, **kw)
    return resample_daily(bars, cal), regime
