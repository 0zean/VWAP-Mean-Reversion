"""Region strategy (U3): target = equal-weight mean of its cells' targets (PLAN.md §5, §6.1)."""

from dataclasses import dataclass

import pandas as pd

from vwap_mr.backtest.market import Market
from vwap_mr.config import DecisionTime
from vwap_mr.strategies.base import DecisionView, check_strategy, target_series


@dataclass(frozen=True)
class Region:
    name: str
    cells: tuple

    def __post_init__(self):
        if not self.cells:
            raise ValueError(f"{self.name}: a region needs at least one cell")
        for c in self.cells:
            check_strategy(c)
        if len({(c.decision, c.fill) for c in self.cells}) != 1:
            raise ValueError(f"{self.name}: cells must share one decision time and fill")

    @property
    def decision(self) -> DecisionTime:
        return self.cells[0].decision

    @property
    def fill(self) -> str:
        return self.cells[0].fill

    def target(self, view: DecisionView) -> float:
        return sum(float(c.target(view)) for c in self.cells) / len(self.cells)

    def targets(self, market: Market) -> pd.Series:
        return pd.concat([target_series(c, market) for c in self.cells], axis=1).mean(axis=1)
