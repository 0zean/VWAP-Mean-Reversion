"""Alpaca SIP bars and the exchange calendar (U1; ported from 0zean/wavelet-meta-model `data/alpaca_source.py`:
`AlpacaSource`, `bars_from_raw`, `calendar_from_raw`).

Contract: credentials come from `credentials.credential` (ALPACA_API_KEY, ALPACA_API_SECRET), never logged; bars
are stamped with the bar OPEN time in UTC (a 5Min bar stamped 14:30Z covers [14:30, 14:35) and is known at 14:35;
a daily bar is stamped at the session's NY midnight); Alpaca's inclusive `end` is trimmed to `[start, end)`; a
refused request raises (never a fall back to another feed).

Auction prints (PLAN.md §11, 2026-10-20 amendment): Alpaca's daily bar open/close are not the official auction prints
on many 2016-2022 sessions, so `1DayAuction` is built from SIP trades: the listing exchange's opening cross
(condition `O`) and closing cross (condition `6`), the prices an OPG / MOC order is filled at."""

from datetime import date, datetime

import pandas as pd

from vwap_mr.data.credentials import credential

NY_TZ = "America/New_York"
BAR_COLUMNS = ["open", "high", "low", "close", "volume", "vwap", "trade_count"]
_RAW_KEYS = {
    "o": "open",
    "h": "high",
    "l": "low",
    "c": "close",
    "v": "volume",
    "vw": "vwap",
    "n": "trade_count",
}
FEEDS = ("sip", "iex")
ADJUSTMENTS = ("raw", "split", "dividend", "all")
NATIVE_MINUTES = {
    "5Min": 5,
    "1DayPrint": None,
}  # None = Alpaca's daily bar (consolidated volume; its open/close are not the crosses, see 1DayAuction)


# Listing exchange in Alpaca's SIP exchange codes (P = NYSE Arca; T = Nasdaq, as Alpaca reports its crosses)
PRIMARY_EXCHANGE = {"SPY": "P", "IWM": "P", "DIA": "P", "QQQ": "T"}
# The cross print (O = opening, 6 = closing) first; else the listing exchange's official open / close report
# (Q / M), which carries the same price (seen where Alpaca lacks the cross print, e.g. SPY 2025-09-05)
OPEN_CROSS, CLOSE_CROSS = ("O", "Q"), ("6", "M")


def ts(x) -> pd.Timestamp:
    """`pd.Timestamp(x)`, refusing NaT."""
    t = pd.Timestamp(x)
    if not isinstance(t, pd.Timestamp):
        raise TypeError(f"not a timestamp: {x!r}")
    return t


def midnight(index) -> pd.DatetimeIndex:
    """Each stamp's midnight in its own time zone (`DatetimeIndex.normalize`, which pandas generates at runtime and
    the type checker cannot see)."""
    return pd.DatetimeIndex(index).normalize()  # ty: ignore[unresolved-attribute]


def bars_from_raw(raw: list[dict]) -> pd.DataFrame:
    """Alpaca raw bar dicts -> UTC-indexed float64 frame with BAR_COLUMNS."""
    if not raw:
        return pd.DataFrame(columns=BAR_COLUMNS, index=pd.DatetimeIndex([], tz="UTC"), dtype="float64")
    df = pd.DataFrame(raw)
    out = df.rename(columns=_RAW_KEYS).reindex(columns=BAR_COLUMNS).astype("float64")
    out.index = pd.DatetimeIndex(pd.to_datetime(df["t"], utc=True)).rename(None)
    return out


def calendar_from_raw(raw: list[dict]) -> pd.DataFrame:
    """Alpaca calendar rows {"date", "open", "close"} (NY wall clock) -> frame indexed by the tz-naive session date
    with tz-aware NY `session_open` / `session_close` (early closes kept as published)."""
    df = pd.DataFrame(raw)[["date", "open", "close"]]
    out = pd.DataFrame(
        {
            "session_open": pd.to_datetime(df["date"] + " " + df["open"]).dt.tz_localize(NY_TZ),
            "session_close": pd.to_datetime(df["date"] + " " + df["close"]).dt.tz_localize(NY_TZ),
        }
    )
    out.index = pd.DatetimeIndex(pd.to_datetime(df["date"]), name="date")
    return out.sort_index()


class AlpacaSource:
    """Thin wrapper over alpaca-py; tests substitute any object with `bars` and `calendar`."""

    def __init__(self, feed: str = "sip"):
        from alpaca.data.historical import StockHistoricalDataClient
        from alpaca.trading.client import TradingClient

        if feed not in FEEDS:
            raise ValueError(f"feed must be one of {FEEDS}, got {feed!r}")
        key, secret = credential("ALPACA_API_KEY", "ALPACA_API_SECRET")
        self.feed = feed
        self._data = StockHistoricalDataClient(key, secret, raw_data=True)
        self._trading = TradingClient(key, secret, paper=True, raw_data=True)

    def bars(
        self, symbol: str, timeframe: str, start: datetime, end: datetime, adjustment: str
    ) -> pd.DataFrame:
        """Native bars in [start, end) (tz-aware bounds), UTC index. Any API error propagates."""
        from alpaca.data.enums import Adjustment, DataFeed
        from alpaca.data.requests import StockBarsRequest
        from alpaca.data.timeframe import TimeFrame, TimeFrameUnit

        if timeframe not in NATIVE_MINUTES:
            raise ValueError(
                f"{timeframe!r} is not a native timeframe; expected one of {list(NATIVE_MINUTES)}"
            )
        if adjustment not in ADJUSTMENTS:
            raise ValueError(f"adjustment must be one of {ADJUSTMENTS}, got {adjustment!r}")
        minutes = NATIVE_MINUTES[timeframe]
        stop = ts(end).tz_convert("UTC")
        req = StockBarsRequest(
            symbol_or_symbols=symbol,
            timeframe=TimeFrame.Day if minutes is None else TimeFrame(minutes, TimeFrameUnit.Minute),
            start=ts(start).tz_convert("UTC").to_pydatetime(),
            end=stop.to_pydatetime(),
            adjustment=Adjustment(adjustment),
            feed=DataFeed(self.feed),
        )
        raw = self._data.get_stock_bars(req)
        if not isinstance(raw, dict):
            raise TypeError(f"expected raw bar data, got {type(raw).__name__}")
        df = bars_from_raw(raw.get(symbol, []))
        return df[df.index < stop]  # Alpaca's `end` is inclusive

    def calendar(self, start: date, end: date) -> pd.DataFrame:
        """Exchange sessions in [start, end] (calendar_from_raw)."""
        from alpaca.trading.requests import GetCalendarRequest

        raw = self._trading.get_calendar(GetCalendarRequest(start=start, end=end))
        if not isinstance(raw, list):
            raise TypeError(f"expected raw calendar rows, got {type(raw).__name__}")
        return calendar_from_raw([dict(r) for r in raw])

    def trades(self, symbol: str, start: datetime, end: datetime) -> list[dict]:
        """Raw SIP trades in [start, end) (tz-aware bounds): {"t", "p", "s", "x", "c", ...}. Errors propagate."""
        from alpaca.data.enums import DataFeed
        from alpaca.data.requests import StockTradesRequest

        stop = ts(end).tz_convert("UTC")
        req = StockTradesRequest(symbol_or_symbols=symbol, start=ts(start).tz_convert("UTC").to_pydatetime(),
                                 end=stop.to_pydatetime(), feed=DataFeed(self.feed))  # fmt: skip
        raw = self._data.get_stock_trades(req)
        if not isinstance(raw, dict):
            raise TypeError(f"expected raw trade data, got {type(raw).__name__}")
        return [t for t in raw.get(symbol, []) if ts(t["t"]) < stop]

    def corporate_actions(self, symbol: str, start: date, end: date) -> dict[str, list[dict]]:
        """Alpaca corporate actions for `symbol` with ex/process dates in [start, end]: {type: [rows]}."""
        from alpaca.data.historical.corporate_actions import CorporateActionsClient
        from alpaca.data.requests import CorporateActionsRequest

        key, secret = credential("ALPACA_API_KEY", "ALPACA_API_SECRET")
        client = CorporateActionsClient(key, secret, raw_data=True)
        raw = client.get_corporate_actions(CorporateActionsRequest(symbols=[symbol], start=start, end=end))
        if not isinstance(raw, dict):
            raise TypeError(f"expected raw corporate actions, got {type(raw).__name__}")
        return {k: [r for r in v if r.get("symbol") == symbol] for k, v in raw.items()}


def auction_from_trades(
    trades: list[dict], primary: str, conditions: tuple[str, ...]
) -> tuple[float, float, int] | None:
    """(price, size, n_distinct_prices) of the listing exchange's cross: the largest trade at `primary` carrying
    `conditions[0]`, else the first later condition present; None if there is none. Duplicate reports of one cross
    are common; distinct prices are counted for the quality report, never averaged."""
    at_primary = [t for t in trades if t.get("x") == primary]
    hits = []
    for cond in conditions:
        hits = [t for t in at_primary if cond in (t.get("c") or [])]
        if hits:
            break
    if not hits:
        return None
    best = max(hits, key=lambda t: t["s"])
    return float(best["p"]), float(best["s"]), len({t["p"] for t in hits})
