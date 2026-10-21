"""Trend region and the vol-managed benchmark (U11, PLAN.md §6.5): rules whose inputs live in `Market.state`.

Their windows (up to 252 sessions) need history before 2016-01-04, so their inputs are computed on the spliced long
series (`features.state.long_inputs`, adjusted closes: total return) and handed to the strategy as state columns;
row t is as of session t's close decision, like every state (base.py). A NaN input is never filled: the target is
NaN and `target_series` raises.

- TSMOM (Moskowitz, Ooi & Pedersen 2012; the horizons of PLAN §6.5): mean over L ∈ {21, 63, 126, 252} of
  sign(close_t / close_{t−L} − 1), 1× notional, no volatility scaling.
- Donchian-55 (variant): long when close_t exceeds the highest of the previous 55 closes, short below the lowest,
  held until the opposite breakout; 0 before the first breakout. Closes, not highs / lows (`diag` has closes only).
- Vol-managed buy-and-hold (the retail benchmark): min(1, σ* / σ̂_t), σ* 15 %, σ̂ the annualized σ of returns over
  21 (the `B1_vol` column), never levered."""

from collections.abc import Callable
from dataclasses import dataclass

import numpy as np
import pandas as pd

from vwap_mr.config import DecisionTime

TSMOM_LOOKBACKS = (21, 63, 126, 252)
DONCHIAN = 55
VOL_MANAGED = 0.15
ANN = 252


def donchian_position(close: pd.Series, n: int = DONCHIAN) -> pd.Series:
    hi, lo = close.shift(1).rolling(n).max(), close.shift(1).rolling(n).min()
    sig = pd.Series(np.where(close > hi, 1.0, np.where(close < lo, -1.0, np.nan)), index=close.index)
    return sig.ffill().fillna(0.0).where(hi.notna())  # hold until the opposite breakout


def trend_state(close: pd.Series, lookbacks=TSMOM_LOOKBACKS, donchian: int = DONCHIAN) -> pd.DataFrame:
    """`TF_<L>` trailing total returns and `DC_<n>` the Donchian position on an adjusted close series."""
    if close.isna().any():
        raise ValueError("trend_state: NaN close (a trailing return would silently skip it)")
    out = {f"TF_{L}": close / close.shift(L) - 1 for L in lookbacks}
    out[f"DC_{donchian}"] = donchian_position(close, donchian)
    return pd.DataFrame(out, index=close.index)


def state_cols(market, cols: tuple[str, ...]) -> np.ndarray:
    if market.state is None or not set(cols) <= set(market.state.columns):
        raise ValueError(f"the market's state lacks {cols}")
    return market.state[list(cols)].reindex(market.index).to_numpy(float)


def state_row(view, cols: tuple[str, ...]) -> np.ndarray:
    """The state row of the view's session (a close view's last row); raises if it is absent."""
    st = view.state
    if st is None or not len(st) or st.index[-1] != view.sessions.index[-1]:
        raise ValueError(f"no state row for {view.sessions.index[-1]}")
    return st[list(cols)].iloc[-1:].to_numpy(float)


@dataclass(frozen=True)
class StateRule:
    """Target = fn(state columns), row-wise (fn maps an (n, len(cols)) array to n targets)."""

    name: str
    cols: tuple[str, ...]
    fn: Callable[[np.ndarray], np.ndarray]
    decision: DecisionTime = DecisionTime.CLOSE
    fill: str = "next_open"

    def target(self, view) -> float:
        return float(self.fn(state_row(view, self.cols))[0])

    def targets(self, market) -> pd.Series:
        return pd.Series(self.fn(state_cols(market, self.cols)), index=market.index, name=self.name)


def vol_scale(sd21: np.ndarray, sigma: float) -> np.ndarray:
    """min(1, σ* / σ̂) with σ̂ = daily σ̂ × √252; NaN where σ̂ is NaN or 0 (never a silent full exposure)."""
    sd = np.asarray(sd21, float) * np.sqrt(ANN)
    with np.errstate(divide="ignore", invalid="ignore"):
        return np.where(sd > 0, np.minimum(1.0, sigma / sd), np.nan)


def tsmom(lookbacks=TSMOM_LOOKBACKS) -> StateRule:
    return StateRule("TSMOM", tuple(f"TF_{L}" for L in lookbacks), lambda x: np.sign(x).mean(axis=1))


def donchian(n: int = DONCHIAN) -> StateRule:
    return StateRule(f"DC{n}", (f"DC_{n}",), lambda x: x[:, 0])


def vol_managed(sigma: float = VOL_MANAGED) -> StateRule:
    return StateRule(f"BH_vm{sigma * 100:g}", ("B1_vol",), lambda x: vol_scale(x[:, 0], sigma))


TREND = {"TSMOM": tsmom, "DC55": donchian}
