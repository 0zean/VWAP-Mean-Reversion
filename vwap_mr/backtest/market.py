"""`Market`: the engine's one input frame set (U3, SPEC.md §1), and `load_market` that builds it from the frozen vintage.

`Market.sessions` is one row per session (tz-aware NY midnight index) with
- `open_raw`, `close_raw`: the listing exchange's opening / closing cross (1DayAuction, raw dollars): the fill and
  mark prices; `dividend`: the cash dividend whose ex-date is that session (0 otherwise);
- `close` (and `open` where present): the same prints total-return adjusted by the reconciled dividend table, for
  strategies (exact close-to-close returns; the engine itself never uses adjusted prices);
- `volume`: the 1DayPrint consolidated volume (the headline VWAP weight);
- optional `open_fallback_raw`, `close_fallback_raw`: the first / last RTH 5Min open / close, used by the engine only
  where a cross is missing, and counted (Result.fallbacks); `close_fallback_end`: when that last bar ended (a `moc`
  fill at a fallback that ended by 15:45 is counted as `close_seen_at_decision`).

`source` makes the U1 diag guard sticky: "alpaca" is accepted only for a frame whose `attrs["source"]` is "alpaca"
(set by `load_market`; a frame rebuilt from columns loses it), "diag" for the secondary history (the engine refuses
to cost it outside `diagnostic=True`), "synthetic" for tests and noise studies.
"""

from dataclasses import dataclass

import numpy as np
import pandas as pd

from vwap_mr.config import DecisionTime
from vwap_mr.data.alpaca import NY_TZ, midnight
from vwap_mr.data.exo import Exo
from vwap_mr.data.secondary import DiagDataError

SESSION_COLUMNS = ("open_raw", "close_raw", "dividend", "close", "volume")
FALLBACK_COLUMNS = ("open_fallback_raw", "close_fallback_raw")
SOURCES = ("alpaca", "diag", "synthetic")
CLOSE_LAG = pd.Timedelta(hours=16, minutes=25)  # decision instants without a calendar (synthetic sessions)
T1545_LAG = pd.Timedelta(hours=15, minutes=45)


@dataclass(frozen=True)
class Market:
    sessions: pd.DataFrame
    source: str
    snapshot: pd.DataFrame | None = None  # snap1545 (adjusted): last, volume, vwap, n_bars
    state: pd.DataFrame | None = (
        None  # row t = the state as of session t's close decision (t1545 views see ≤ t−1)
    )
    exo: Exo | None = None
    calendar: pd.DataFrame | None = None
    symbol: str = ""

    def __post_init__(self):
        s = self.sessions
        missing = [c for c in SESSION_COLUMNS if c not in s.columns]
        if missing:
            raise ValueError(f"Market.sessions lacks columns {missing}")
        idx = pd.DatetimeIndex(s.index)
        if idx.tz is None or not idx.is_monotonic_increasing or not idx.is_unique:
            raise ValueError("Market.sessions needs a unique, increasing, tz-aware index")
        if self.source not in SOURCES:
            raise ValueError(f"source must be one of {SOURCES}, got {self.source!r}")
        tagged_diag = s.attrs.get("source") == "diag" or ("is_diag" in s.columns and bool(s["is_diag"].any()))
        if tagged_diag and self.source != "diag":
            raise DiagDataError(f"diag rows passed as source={self.source!r}")
        if self.source == "alpaca" and s.attrs.get("source") != "alpaca":
            raise DiagDataError(
                "source='alpaca' needs a frame from load_market (attrs['source'] == 'alpaca')"
            )
        for name, f in (("snapshot", self.snapshot), ("state", self.state)):
            if f is not None and not pd.DatetimeIndex(f.index).isin(idx).all():
                raise ValueError(f"Market.{name} has rows off the session index")

    @property
    def index(self) -> pd.DatetimeIndex:
        return pd.DatetimeIndex(self.sessions.index)

    def decision_instants(self, decision: DecisionTime) -> pd.DatetimeIndex:
        """Decision instant (tz-aware NY) per session: from the exchange calendar (early closes respected), else
        16:25 / 15:45 on the session date."""
        if self.calendar is not None:
            from vwap_mr.data.calendar import decision_times

            return decision_times(self.index, self.calendar, decision)
        days = midnight(self.index.tz_convert(NY_TZ).tz_localize(None))
        lag = CLOSE_LAG if decision == DecisionTime.CLOSE else T1545_LAG
        return (days + lag).tz_localize(NY_TZ)


def load_market(symbol: str, start, end, *, vintage: str, with_snapshot: bool = False, **kw) -> Market:
    """SPEC.md §1: 1DayAuction (adjusted, with raw prints and dividends) × 1DayPrint volume, the RTH 5Min open / close
    as the counted fallback for a missing cross, VIX and DTB3, and the vintage's calendar. A session without
    consolidated volume raises."""
    from vwap_mr.data.bars import load_bars
    from vwap_mr.data.calendar import get_calendar
    from vwap_mr.data.exo import load_series
    from vwap_mr.data.store import vintage_dir

    auc = load_bars(symbol, "1DayAuction", start, end, vintage=vintage, **kw)
    vol = load_bars(symbol, "1DayPrint", start, end, vintage=vintage, **kw)["volume"].reindex(auc.index)
    if vol.isna().any():
        bad = [str(d.date()) for d in auc.index[vol.isna().to_numpy()][:10]]
        raise ValueError(f"{symbol}: no 1DayPrint volume for sessions {bad}")
    sessions = auc[["open", "close", "open_raw", "close_raw", "dividend", "factor"]].copy()
    sessions["volume"] = vol
    gaps = auc.index[(auc["open_raw"].isna() | auc["close_raw"].isna()).to_numpy()]
    if len(gaps):
        sessions["open_fallback_raw"] = np.nan
        sessions["close_fallback_raw"] = np.nan
        sessions["close_fallback_end"] = pd.Series(
            pd.NaT, index=sessions.index, dtype=f"datetime64[ns, {NY_TZ}]"
        )
        for d in gaps:
            # raw 5Min of the session itself, not 1DayRTH: that drops a session whose last bar is missing, which is
            # exactly when a cross can be missing too (2019-08-12: no SIP trades 15:35-16:00 for SPY / IWM / DIA)
            day = d.tz_localize(None)
            bars = load_bars(
                symbol, "5Min", day, day + pd.Timedelta(days=1), vintage=vintage, adjustment="raw", **kw
            )
            if bars.empty:
                continue
            sessions.loc[d, ["open_fallback_raw", "close_fallback_raw"]] = [
                bars["open"].iloc[0],
                bars["close"].iloc[-1],
            ]
            last_end = bars.index[-1] + pd.Timedelta(minutes=5)
            sessions.loc[d, "close_fallback_end"] = last_end
            print(f"[MARKET] {symbol} {day.date()}: cross missing (open {np.isnan(auc.loc[d, 'open_raw'])}, close "
                  f"{np.isnan(auc.loc[d, 'close_raw'])}); RTH 5Min fallback: first bar {bars.index[0].time()}, last "
                  f"bar ends {last_end.time()}")  # fmt: skip
        # strategies see the counted fallback prints too (adjusted with the session's factor), never a NaN hole
        for side in ("open", "close"):
            miss = sessions[side].isna()
            sessions.loc[miss, side] = (
                sessions.loc[miss, f"{side}_fallback_raw"] * sessions.loc[miss, "factor"]
            )
    sessions.attrs["source"] = "alpaca"
    snap = load_bars(symbol, "snap1545", start, end, vintage=vintage, **kw) if with_snapshot else None
    a, b = auc.index[0].tz_localize(None), auc.index[-1].tz_localize(None) + pd.Timedelta(days=1)
    exo = Exo({n: load_series(n, a - pd.Timedelta(days=30), b, vintage=vintage) for n in ("VIX", "DTB3")})
    calendar = get_calendar(vintage_dir(vintage), b)
    return Market(sessions, "alpaca", snapshot=snap, exo=exo, calendar=calendar, symbol=symbol.upper())
