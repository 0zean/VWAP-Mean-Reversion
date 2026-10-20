"""Costs by fill kind (U3, ported from wavelet-meta-model `risk/costs.py`): half-spread + slippage, SEC/TAF
on sells from a dated table, short borrow, T-bill on cash.

Contract: refuses to cost `diag` rows."""

import pandas as pd

from vwap_mr.data.secondary import refuse_diag


def check_costable(bars: pd.DataFrame) -> None:
    """The cost model's entry guard (U1; the cost functions arrive in U3): `diag` bars are never costed as fills."""
    refuse_diag(bars, "costing as Alpaca fills")
