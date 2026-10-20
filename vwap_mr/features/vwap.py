"""Rolling VWAP variants, session VWAP and the 15:45 snapshot (U3; session VWAP ported from
wavelet-meta-model `features/kernels.py`).

Contract: VWAP_n at t uses sessions t-n+1..t only. Session VWAP and the snapshot are built in the data layer
(`1DayRTH`, `snap1545`, data/bars.py)."""

import pandas as pd


def rolling_vwap(price: pd.Series, volume: pd.Series, n: int) -> pd.Series:
    """VWAP_n(t) = Σ_{i<n} p_{t−i} v_{t−i} / Σ v_{t−i} (PLAN.md §8.1); NaN until n sessions exist or when the window's
    volume is 0."""
    if n < 1:
        raise ValueError(f"n must be >= 1, got {n}")
    pv = (price * volume).rolling(n, min_periods=n).sum()
    v = volume.rolling(n, min_periods=n).sum()
    return (pv / v.where(v > 0)).rename(f"vwap_{n}")
