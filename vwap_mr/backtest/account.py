"""Account check (U3, SPEC.md §5): `check(result, profile) -> (tradable, reasons)`; margin vs cash, Reg-T, day trades.

Contract: reports tradability, never changes a P&L. The registered profile is `margin_30k` (PLAN.md §9.4, answered
2026-10-20: margin, $30k, no leverage, shorts allowed).

- Cash account: no short exposure at any time.
- Leverage: |exposure| ≤ `max_exposure` (1.0, no leverage) at every fill and overnight. Reg-T would allow 2× overnight
  on margin; the profile is stricter by choice.
- Shorting on margin needs equity ≥ `min_short_equity` ($2,000, the FINRA minimum).
- Day trades: a position opened and closed in the same session. The daily rules fill once per session per symbol (a
  long → short flip at one print is one trade), so the count is 0 by construction; it is computed from the fills so
  an intraday family (U14) is checked by the same code. Under `pdt_min_equity` more than `max_day_trades` in any
  5 sessions flags the account."""

from dataclasses import dataclass
from typing import Literal

import numpy as np
import pandas as pd

PDT_WINDOW = 5


@dataclass(frozen=True)
class AccountProfile:
    name: str
    kind: Literal["margin", "cash"] = "margin"
    equity: float = 30_000.0
    max_exposure: float = 1.0
    min_short_equity: float = 2_000.0
    pdt_min_equity: float = 25_000.0
    max_day_trades: int = 3

    def __post_init__(self):
        if self.kind not in ("margin", "cash"):
            raise ValueError(f"kind must be 'margin' or 'cash', got {self.kind!r}")
        if not (self.equity > 0 and self.max_exposure > 0):
            raise ValueError("equity and max_exposure must be > 0")


PROFILES = {"margin_30k": AccountProfile("margin_30k"), "cash_30k": AccountProfile("cash_30k", kind="cash")}


def get_profile(name: str) -> AccountProfile:
    try:
        return PROFILES[name]
    except KeyError:
        raise ValueError(f"unknown account profile {name!r}; expected one of {sorted(PROFILES)}") from None


def fills(result) -> pd.DataFrame:
    """One row per fill (the ledger row is the fill's session under every fill): side (+1 buy / −1 sell), change in
    exposure."""
    lg = result.ledger
    d = (lg["exp_close"] - lg["exp_overnight"]).to_numpy()
    keep = np.abs(d) > 0
    return pd.DataFrame({"side": np.sign(d[keep]).astype(int), "delta": d[keep]}, index=lg.index[keep])


def day_trades(f: pd.DataFrame) -> pd.Series:
    """Day trades per session: sessions holding both a buy and a sell of the symbol."""
    if f.empty:
        return pd.Series(dtype=int)
    g = f.groupby(level=0)["side"]
    both = (g.max() > 0) & (g.min() < 0)
    return both[both].astype(int)


def check(result, profile: AccountProfile | str) -> tuple[bool, list[str]]:
    p = get_profile(profile) if isinstance(profile, str) else profile
    lg, reasons = result.ledger, []
    exposure = pd.concat([lg["exp_overnight"], lg["exp_close"]])
    if abs(result.equity0 - p.equity) > 0.005:
        reasons.append(f"backtest equity {result.equity0:,.2f} != profile equity {p.equity:,.2f}")
    if p.kind == "cash" and (exposure < 0).any():
        reasons.append(f"cash account cannot short: {int((lg['exp_close'] < 0).sum())} session(s) end short")
    over = exposure.abs() > p.max_exposure + 1e-12
    if over.any():
        reasons.append(f"|exposure| above {p.max_exposure} in {int(over.sum())} position(s)")
    if p.kind == "margin" and (exposure < 0).any() and (lg["equity"] < p.min_short_equity).any():
        reasons.append(f"equity below the {p.min_short_equity:,.0f} shorting minimum while short")
    dt = day_trades(fills(result))
    if len(dt):
        per = dt.reindex(lg.index, fill_value=0).rolling(PDT_WINDOW, min_periods=1).sum()
        if (
            (per > p.max_day_trades) & (lg["equity"].shift(1, fill_value=result.equity0) < p.pdt_min_equity)
        ).any():
            reasons.append(
                f"more than {p.max_day_trades} day trades in {PDT_WINDOW} sessions under the PDT floor"
            )
    return not reasons, reasons
