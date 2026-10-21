"""The VWAP-MR cell and the region R₀ (PLAN.md §6.1, §8.1). Written in U5 (its diagnostics run the cells on `diag`);
U6 registers R₀.

Cell (n, b): target −1 if d_t > b, +1 if d_t < −b, else 0, with d_t = close_t / VWAP_n(t) − 1 over sessions
t−n+1 … t (`Market.sessions` `close` × `volume`); 0 until n sessions exist. A close decision, so the fill is
`next_open` (or `same_close` through `run_diagnostic`). `VWAP.R` is the cell (3, 0.001) on Yahoo's split-adjusted
close and volume (`< 0.999` long, `> 1.001` short).

R₀ = the equal-weight region over n ∈ {2, 3, 5, 10} × b ∈ {0, 0.10, 0.25, 0.50 %}."""

from dataclasses import dataclass

import numpy as np
import pandas as pd

from vwap_mr.config import DecisionTime
from vwap_mr.features.deviation import trading_deviation
from vwap_mr.strategies.region import Region

R0_N = (2, 3, 5, 10)
R0_B = (0.0, 0.001, 0.0025, 0.005)


def cell_target(d, b: float):
    """−1 / 0 / +1 per §8.1; a NaN deviation (warm-up, zero volume) is flat."""
    d = np.asarray(d, dtype=float)
    return np.where(np.isnan(d), 0.0, np.where(d > b, -1.0, np.where(d < -b, 1.0, 0.0)))


@dataclass(frozen=True)
class VwapCell:
    n: int
    b: float
    decision: DecisionTime = DecisionTime.CLOSE
    fill: str = "next_open"

    def __post_init__(self):
        if self.n < 1 or not self.b >= 0:
            raise ValueError(f"need n >= 1 and b >= 0; got {self.n}, {self.b}")
        if self.decision != DecisionTime.CLOSE:
            raise ValueError("the VWAP-MR cell needs session t's close and volume: a close decision")

    @property
    def name(self) -> str:
        return f"vwap_mr_n{self.n}_b{self.b * 1e4:g}bp"

    def target(self, view) -> float:
        s = view.sessions.iloc[-self.n :]
        if len(s) < self.n:
            return 0.0
        return float(cell_target(trading_deviation(s["close"], s["volume"], self.n).iloc[-1], self.b))

    def targets(self, market) -> pd.Series:
        s = market.sessions
        d = trading_deviation(s["close"], s["volume"], self.n)
        return pd.Series(cell_target(d, self.b), index=s.index, name=self.name)


def r0_cells() -> tuple[VwapCell, ...]:
    return tuple(VwapCell(n, b) for n in R0_N for b in R0_B)


def r0() -> Region:
    return Region("R0", r0_cells())


VWAP_R = VwapCell(3, 0.001)  # the inherited cell
