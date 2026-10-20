"""Costs by fill kind (U3, SPEC.md §4): half-spread + slippage per auction fill, SEC Section 31 and FINRA TAF on sells
from a dated table, short borrow, T-bill on cash.

- Spread + slippage: `round_trip_bp` split evenly over the two fills of a round trip, so every fill (`open_auction`
  or `close_auction`) pays round_trip_bp / 2 one way on its traded notional. Registered 1.0 bp; curve 0.3 / 1.0 /
  2.3; `"measured"` reads `data/costs/measured.csv` (written only by U13's opt-in live trades; absent until then).
- Sells (a lower target, a short sale included) pay the SEC Section 31 rate on proceeds and the FINRA TAF per share
  sold (capped per trade), from `data/costs/fees.csv` by the fill's session date (rates change each fiscal year;
  the 2025-05-14 zero rate is a row).
- Borrow: `borrow_rate` per year (ACT/360) on short notional held over each night (calendar days to the next
  session); 0.4 %/yr by default (Alpaca's easy-to-borrow ETFs cost less).
- Cash: DTB3 (percent, read as an ACT/360 money-market yield) as of each session's close decision, on the free
  cash (1 − |exposure|) of equity held over each night. Short-sale proceeds earn nothing (a retail account gets no
  rebate), so a short pays the T-bill in excess-return terms.

Contract: refuses to cost `diag` rows."""

from dataclasses import dataclass, replace
from pathlib import Path

import numpy as np
import pandas as pd

from vwap_mr.config import ROOT, DecisionTime
from vwap_mr.data.alpaca import midnight
from vwap_mr.data.secondary import refuse_diag

FEES_CSV = ROOT / "data" / "costs" / "fees.csv"
MEASURED_CSV = ROOT / "data" / "costs" / "measured.csv"
FILL_KINDS = ("open_auction", "close_auction")
REGISTERED_BP = 1.0
COST_CURVE = (0.3, 1.0, 2.3)
DTB3_MAX_AGE = pd.Timedelta(
    days=10
)  # longest gap between visible H.15 observations around holidays is ~5 days


def check_costable(bars: pd.DataFrame) -> None:
    """The cost model's entry guard (U1): `diag` bars are never costed as fills."""
    refuse_diag(bars, "costing as Alpaca fills")


@dataclass(frozen=True)
class CostModel:
    round_trip_bp: float | str = REGISTERED_BP
    borrow_rate: float = 0.004
    fees: bool = True

    def __post_init__(self):
        if isinstance(self.round_trip_bp, str):
            if self.round_trip_bp != "measured":
                raise ValueError(f"round_trip_bp is a number or 'measured', got {self.round_trip_bp!r}")
        elif not self.round_trip_bp >= 0:
            raise ValueError(f"round_trip_bp must be >= 0, got {self.round_trip_bp}")
        if not self.borrow_rate >= 0:
            raise ValueError("borrow_rate must be >= 0")

    def one_way(self, kind: str) -> float:
        """Spread + slippage fraction of one fill of `kind`."""
        if kind not in FILL_KINDS:
            raise ValueError(f"fill kind must be one of {FILL_KINDS}, got {kind!r}")
        if self.round_trip_bp == "measured":
            return measured(kind)
        return float(self.round_trip_bp) / 2 * 1e-4

    def with_round_trip(self, bp: float | str) -> "CostModel":
        return replace(self, round_trip_bp=bp)


ZERO = CostModel(round_trip_bp=0.0, borrow_rate=0.0, fees=False)


def measured(kind: str, path: Path = MEASURED_CSV) -> float:
    """Measured one-way cost (bp → fraction) by fill kind; columns kind, one_way_bp, n. Raises until U13 writes it."""
    if not path.exists():
        raise FileNotFoundError(f"round_trip_bp='measured' needs {path} (U13's opt-in live one-share trades)")
    t = pd.read_csv(path)
    hit = t.loc[t["kind"] == kind, "one_way_bp"]
    if len(hit) != 1:
        raise ValueError(f"{path}: need exactly one row for fill kind {kind!r}")
    return float(hit.iloc[0]) * 1e-4


def fee_table(path: Path = FEES_CSV) -> pd.DataFrame:
    t = pd.read_csv(path, parse_dates=["effective"]).sort_values("effective")
    if t["effective"].duplicated().any():
        raise ValueError(f"{path}: duplicate effective dates")
    return t


def fee_rates(days: pd.DatetimeIndex, table: pd.DataFrame | None = None) -> pd.DataFrame:
    """Per session date: `sec` (fraction of sale proceeds), `taf` ($ per share sold), `taf_max` ($ per trade).
    A date before the table's first row raises."""
    t = fee_table() if table is None else table
    d = pd.DatetimeIndex(days)
    d = d.tz_convert("America/New_York").tz_localize(None) if d.tz is not None else d
    pos = np.searchsorted(t["effective"].to_numpy(), midnight(d).to_numpy(), side="right") - 1
    if (pos < 0).any():
        raise ValueError(f"fee table starts {t['effective'].iloc[0].date()}, after {d[pos < 0][0].date()}")
    rows = t.iloc[pos]
    return pd.DataFrame({"sec": rows["sec_per_million"].to_numpy() * 1e-6, "taf": rows["taf_per_share"].to_numpy(),
                         "taf_max": rows["taf_max"].to_numpy()}, index=days)  # fmt: skip


def tbill_yield(market) -> pd.Series:
    """DTB3 / 100 as of each session's close decision (16:25 ET); a missing or stale (> DTB3_MAX_AGE) value raises."""
    if market.exo is None:
        raise ValueError("tbill_yield needs Market.exo with DTB3")
    when = market.decision_instants(DecisionTime.CLOSE)
    y = market.exo.asof("DTB3", when, max_age=DTB3_MAX_AGE) / 100
    if np.isnan(y).any():
        raise ValueError(f"DTB3 not visible at {when[np.isnan(y)][0]}")
    return pd.Series(y, index=market.index, name="dtb3")
