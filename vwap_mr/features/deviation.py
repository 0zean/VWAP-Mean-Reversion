"""Trading deviation d_t and state deviation d*_t (U3, PLAN.md §8.1). Causal at the decision time."""

import pandas as pd

from vwap_mr.features.vwap import rolling_vwap


def trading_deviation(close: pd.Series, volume: pd.Series, n: int) -> pd.Series:
    """d_t = close_t / VWAP_n(t) − 1, the anchor including session t (as the inherited rule does). Needs session t's
    close and full volume, so it is a `close`-decision feature."""
    return (close / rolling_vwap(close, volume, n) - 1).rename(f"d_{n}")


def state_deviation(close: pd.Series, volume: pd.Series, n: int = 5) -> pd.Series:
    """d*_t = close_t / VWAP_n(t−1) − 1: the anchor over sessions t−n … t−1, so close_t is not on both sides."""
    return (close / rolling_vwap(close, volume, n).shift(1) - 1).rename(f"dstar_{n}")
