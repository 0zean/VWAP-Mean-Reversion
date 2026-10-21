"""The VWAP-MR cell, the region R₀ and its registered variants (PLAN.md §6.1, §8.1). The cell was written in U5 (its
diagnostics run the cells on `diag`); U6 registers R₀ and adds the variants.

Cell (n, b): target −1 if d_t > b, +1 if d_t < −b, else 0, with d_t = close_t / VWAP_n(t) − 1 over sessions
t−n+1 … t; 0 until n sessions exist. The headline VWAP averages the `1DayAuction` close weighted by the `1DayPrint`
volume (`Market.sessions` `close` × `volume`). A close decision, so the fill is `next_open` (or `same_close` through
`run_diagnostic`). `VWAP.R` is the cell (3, 0.001) on Yahoo's split-adjusted close and volume (`< 0.999` long,
`> 1.001` short).

Variant cells change one input each (`load_market(..., with_rth=True)` adds the columns):
- `weight="rth_volume"`: RTH 5Min volume + the closing-cross size (§8.1; the 16:00 cross sits in the bar the RTH
  filter drops, U1's proposal);
- `anchor="rth_vwap"`: the VWAP of the sessions' own RTH VWAPs (same volume weights as the headline);
- `side=+1 / −1`: long-only / short-only (the other side's signal is flat).
A session whose variant input is missing (SPY 2019-08-12 has no RTH last bar and no cross size) makes every window
that contains it NaN, so the cell is flat there; the runner counts those sessions.

R₀ = the equal-weight region over n ∈ {2, 3, 5, 10} × b ∈ {0, 0.10, 0.25, 0.50 %}. `VARIANTS` holds the §6.1 variants
U6 builds; "the other execution" (`t1545` → `moc`) is not built: §9.2 / §9.3 are at their defaults (Basic plan, no
real-time snapshot)."""

from collections.abc import Callable
from dataclasses import dataclass

import numpy as np
import pandas as pd

from vwap_mr.config import DecisionTime
from vwap_mr.features.vwap import rolling_vwap
from vwap_mr.strategies.region import Region

R0_N = (2, 3, 5, 10)
R0_B = (0.0, 0.001, 0.0025, 0.005)
ANCHORS = ("close", "rth_vwap")
WEIGHTS = ("volume", "rth_volume")
ANN = 252


def cell_target(d, b: float):
    """−1 / 0 / +1 per §8.1; a NaN deviation (warm-up, zero volume, a missing variant input) is flat."""
    d = np.asarray(d, dtype=float)
    return np.where(np.isnan(d), 0.0, np.where(d > b, -1.0, np.where(d < -b, 1.0, 0.0)))


@dataclass(frozen=True)
class VwapCell:
    n: int
    b: float
    decision: DecisionTime = DecisionTime.CLOSE
    fill: str = "next_open"
    anchor: str = "close"
    weight: str = "volume"
    side: int = 0  # 0 both sides; +1 long-only; −1 short-only

    def __post_init__(self):
        if self.n < 1 or not self.b >= 0:
            raise ValueError(f"need n >= 1 and b >= 0; got {self.n}, {self.b}")
        if self.decision != DecisionTime.CLOSE:
            raise ValueError("the VWAP-MR cell needs session t's close and volume: a close decision")
        if self.anchor not in ANCHORS or self.weight not in WEIGHTS or self.side not in (-1, 0, 1):
            raise ValueError(f"anchor in {ANCHORS}, weight in {WEIGHTS}, side in (-1, 0, 1); got "
                             f"{self.anchor!r}, {self.weight!r}, {self.side!r}")  # fmt: skip

    @property
    def name(self) -> str:
        tags = [t for t, on in (("rthvol", self.weight == "rth_volume"), ("svwap", self.anchor == "rth_vwap"),
                                ("long", self.side == 1), ("short", self.side == -1)) if on]  # fmt: skip
        return "_".join([f"vwap_mr_n{self.n}_b{self.b * 1e4:g}bp", *tags])

    def _targets(self, s: pd.DataFrame) -> np.ndarray:
        d = s["close"] / rolling_vwap(s[self.anchor], s[self.weight], self.n) - 1
        t = cell_target(d, self.b)
        return t if self.side == 0 else np.where(t * self.side > 0, t, 0.0)

    def target(self, view) -> float:
        s = view.sessions.iloc[-self.n :]
        if len(s) < self.n:
            return 0.0
        return float(self._targets(s)[-1])

    def targets(self, market) -> pd.Series:
        s = market.sessions
        return pd.Series(self._targets(s), index=s.index, name=self.name)


def r0_cells(**kw) -> tuple[VwapCell, ...]:
    """The 16 cells; keyword arguments (anchor, weight, side) build a variant's cells."""
    return tuple(VwapCell(n, b, **kw) for n in R0_N for b in R0_B)


def r0() -> Region:
    return Region("R0", r0_cells())


def vol_scale(close: pd.Series, sigma: float, window: int) -> np.ndarray:
    """min(1, σ* / σ̂_t), σ̂_t the annualized sample std of the last `window` close-to-close returns to session t
    (adjusted closes: total return); 0 until `window` returns exist; 1 when σ̂ is 0."""
    sd = close.pct_change().rolling(window).std(ddof=1).to_numpy() * np.sqrt(ANN)
    with np.errstate(divide="ignore"):
        scale = np.minimum(1.0, np.where(sd > 0, sigma / np.where(sd > 0, sd, 1.0), 1.0))
    return np.where(np.isnan(sd), 0.0, scale)


@dataclass(frozen=True)
class VolTarget:
    """A strategy's target scaled to σ* annualized instrument volatility, never levered (|f| ≤ 1, Reg-T at the
    headline): f_t · min(1, σ* / σ̂_t)."""

    inner: Region
    sigma: float = 0.10
    window: int = 21

    @property
    def name(self) -> str:
        return f"{self.inner.name}_vt{self.sigma * 100:g}"

    @property
    def decision(self) -> DecisionTime:
        return self.inner.decision

    @property
    def fill(self) -> str:
        return self.inner.fill

    def target(self, view) -> float:
        s = view.sessions["close"].iloc[-(self.window + 1) :]
        return float(self.inner.target(view)) * float(vol_scale(s, self.sigma, self.window)[-1])

    def targets(self, market) -> pd.Series:
        return self.inner.targets(market) * vol_scale(market.sessions["close"], self.sigma, self.window)


def _region(name: str, cells) -> Region:
    return Region(name, tuple(cells))


VARIANTS: dict[str, Callable[[], "Region | VolTarget"]] = {
    "rth_volume": lambda: _region("R0_rth_volume", r0_cells(weight="rth_volume")),
    "session_vwap": lambda: _region("R0_session_vwap", r0_cells(anchor="rth_vwap")),
    "b_positive": lambda: _region("R0_b_positive", (c for c in r0_cells() if c.b > 0)),
    "n_le_5": lambda: _region("R0_n_le_5", (c for c in r0_cells() if c.n <= 5)),
    "long_only": lambda: _region("R0_long_only", r0_cells(side=1)),
    "short_only": lambda: _region("R0_short_only", r0_cells(side=-1)),
    "vol_target_10": lambda: VolTarget(r0(), 0.10, 21),
}
HEADLINES: dict[str, Callable[[], Region]] = {"R0": r0}
NEEDS_RTH = ("rth_volume", "session_vwap")


VWAP_R = VwapCell(3, 0.001)  # the inherited cell
