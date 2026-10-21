"""Blend (U11, PLAN.md §6.5): f_t = w_t f^MR_t + (1 − w_t) f^TF_t, with w_t = 0.5 + κ (P_t − 0.5).

P_t is a state known at session t's close decision (a `Market.state` column, as of the same decision as both
components' targets), so the blend trades one netted target: the components' opposite trades cancel before costs.
w_t must lie in [0, 1] and be finite (an undefined P_t raises unless the spec registers a value for it), so
|f_t| ≤ 1. `sigma` vol-targets the blend: f_t × min(1, σ* / σ̂_t) with σ̂ from the `B1_vol` column."""

from dataclasses import dataclass

import numpy as np
import pandas as pd

from vwap_mr.strategies.region import Region
from vwap_mr.strategies.trend import StateRule, state_cols, state_row, vol_scale


def weight(p: pd.Series, kappa: float) -> pd.Series:
    if not 0 <= kappa <= 1:
        raise ValueError(f"κ must lie in [0, 1]; got {kappa}")
    if ((p < 0) | (p > 1)).any():
        raise ValueError("P_t outside [0, 1]")
    return 0.5 + kappa * (p - 0.5)


@dataclass(frozen=True)
class Blend:
    mr: Region
    tf: StateRule
    weight: str  # the Market.state column holding w_t
    sigma: float | None = None
    name: str = "blend"

    @property
    def decision(self):
        return self.mr.decision

    @property
    def fill(self) -> str:
        return self.mr.fill

    def _cols(self) -> tuple[str, ...]:
        return (self.weight,) if self.sigma is None else (self.weight, "B1_vol")

    def _mix(self, x: np.ndarray, f_mr, f_tf):
        f = x[:, 0] * f_mr + (1 - x[:, 0]) * f_tf
        return f if self.sigma is None else f * vol_scale(x[:, 1], self.sigma)

    def target(self, view) -> float:
        return float(self._mix(state_row(view, self._cols()), self.mr.target(view), self.tf.target(view))[0])

    def targets(self, market) -> pd.Series:
        f = self._mix(state_cols(market, self._cols()), self.mr.targets(market).to_numpy(),
                      self.tf.targets(market).to_numpy())  # fmt: skip
        return pd.Series(f, index=market.index, name=self.name)
