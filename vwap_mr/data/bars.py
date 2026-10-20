"""`load_bars(symbol, timeframe, start, end, *, vintage, allow_forward=False)` — the only way research code obtains
bars (U1; ported from 0zean/wavelet-meta-model `data/bars.py::load_bars`, `data/sessions.py` (`rth_filter`,
`resample_daily`, `drop_sessions_missing_last_bar`) and `data/timeframes.py` (`1DayPrint`)).

Ranges are whole NY days `[start, end)`. Timeframes:
- `1DayPrint`: Alpaca's native daily bar, one row per session stamped at the session's NY midnight. Used for its
  consolidated volume (it includes extended hours and the closing cross; U1 measured print / RTH-5Min = 1.05-1.19).
  Its open / close are **not** reliable auction prints before 2023 (U1); use `1DayAuction`.
- `1DayAuction`: the listing exchange's opening and closing crosses from SIP trades (alpaca.py), one row per session
  (NaN and logged where no cross is found, never filled). `adjustment="all"` (default) returns `open` / `close`
  total-return adjusted by the reconciled dividend table (dividends.py) with `open_raw`, `close_raw`, `dividend`,
  `factor`; `adjustment="raw"` returns raw prices. Always cached raw (`sip/raw/1DayAuction`).
- `5Min`: native, RTH-filtered against the calendar (early closes respected); stamped at the bar open.
- `1DayRTH`: resampled from `5Min`: RTH OHLC, RTH volume, session VWAP = Σ bar_vwap·volume / Σ volume. Sessions
  whose last 5Min bar is missing are dropped and logged (their close would not be the session close).
- `snap1545`: from `5Min`, the state at the `t1545` decision (session close − 15 min): `last` = close of the last
  bar that ends by then, `volume` and `vwap` = cumulative RTH volume and session VWAP to then, `n_bars`.

Contract: any request whose `end` is past `HOLDOUT_START` raises `HoldoutError` unless `allow_forward`, which only
the `live` cache accepts and which appends an audit row to `results/forward_access.jsonl`. `end` is clamped to
today (NY), so only completed sessions are cached. A frozen vintage is read-only: a request it does not cover
raises; nothing writes into it. Missing bars are never filled."""

import json
from datetime import datetime, timedelta
from pathlib import Path

import pandas as pd

from vwap_mr.config import CACHE_DIR, HOLDOUT_START, RESULTS_DIR, DecisionTime
from vwap_mr.data.alpaca import (
    CLOSE_CROSS,
    NATIVE_MINUTES,
    NY_TZ,
    OPEN_CROSS,
    PRIMARY_EXCHANGE,
    AlpacaSource,
    auction_from_trades,
    midnight,
    ts,
)
from vwap_mr.data.calendar import decision_times, get_calendar, today_ny
from vwap_mr.data.store import (
    LIVE,
    MANIFEST,
    FrozenVintageError,
    load_frame,
    merge_bars,
    save_frame,
    vintage_dir,
)

TIMEFRAMES = ("1DayPrint", "1DayAuction", "5Min", "1DayRTH", "snap1545")
DERIVED_FROM = {"1DayRTH": "5Min", "snap1545": "5Min"}
HOLDOUT = ts(HOLDOUT_START)
# re-fetched on top-ups to detect a retroactive (dividend / split) re-adjustment
OVERLAP = pd.Timedelta(days=7)
ADJ_RTOL = 1e-6
FETCH_CHUNK = pd.Timedelta(days=366)
FORWARD_ACCESS = RESULTS_DIR / "forward_access.jsonl"
SNAP_COLUMNS = ["last", "volume", "vwap", "n_bars"]
AUCTION_COLUMNS = [
    "open",
    "close",
    "open_size",
    "close_size",
    "open_nprice",
    "close_nprice",
    "open_wide",
    "close_wide",
]
AUCTION_WINDOWS = (
    pd.Timedelta(seconds=5),
    pd.Timedelta(minutes=15),
)  # searched in order from the session bound


class HoldoutError(ValueError):
    pass


def _day(x) -> pd.Timestamp:
    """Date-like -> tz-naive midnight (a NY calendar day); refuses a time of day."""
    t = ts(x)
    if t.tz is not None:
        t = t.tz_convert(NY_TZ).tz_localize(None)
    if t != t.normalize():
        raise ValueError(f"load_bars ranges are whole days; got {x!r}")
    return t


def _log(msg: str) -> None:
    print(f"[DATA]  {msg}")


# ── Sessions ────────────────────────────────────────────────────────────────


def to_ny(df: pd.DataFrame) -> pd.DataFrame:
    out = (
        df.tz_convert(NY_TZ)
        if pd.DatetimeIndex(df.index).tz is not None
        else df.tz_localize("UTC").tz_convert(NY_TZ)
    )
    out.index = pd.DatetimeIndex(out.index).as_unit("ns")
    return out


def _naive_days(index) -> pd.DatetimeIndex:
    return midnight(pd.DatetimeIndex(index).tz_convert(NY_TZ).tz_localize(None))


def _session_bounds(index, calendar: pd.DataFrame):
    cal = calendar.reindex(_naive_days(index))
    return pd.DatetimeIndex(cal["session_open"]), pd.DatetimeIndex(cal["session_close"])


def _check_calendar_covers(index, calendar: pd.DataFrame) -> None:
    if len(index) == 0:
        return
    days = _naive_days(index)
    if calendar.empty or calendar.index[0] > days[0] or calendar.index[-1] < days[-1]:
        raise ValueError(f"calendar does not cover {days[0].date()} -> {days[-1].date()}")


def rth_filter(df: pd.DataFrame, calendar: pd.DataFrame, bar_minutes: int) -> pd.DataFrame:
    """Keep intraday bars whose whole interval [t, t + bar_minutes) lies inside a calendar session (NY index)."""
    df = to_ny(df)
    _check_calendar_covers(df.index, calendar)
    opens, closes = _session_bounds(df.index, calendar)
    keep = (df.index >= opens) & (
        df.index + pd.Timedelta(minutes=bar_minutes) <= closes
    )  # NaT compares False
    return df[keep]


def session_filter(df: pd.DataFrame, calendar: pd.DataFrame) -> pd.DataFrame:
    """Daily bars: keep rows whose NY date is a session, stamped at that session's NY midnight."""
    df = to_ny(df)
    _check_calendar_covers(df.index, calendar)
    day = midnight(df.index)
    keep = day.tz_localize(None).isin(calendar.index)
    dropped = df.index[~keep]
    if len(dropped):
        _log(f"dropping {len(dropped)} daily bar(s) off the calendar: {[str(d.date()) for d in dropped[:5]]}")
    out = df[keep].copy()
    out.index = day[keep]
    return out


def sessions_missing_last_bar(df: pd.DataFrame, calendar: pd.DataFrame, bar_minutes: int) -> pd.DatetimeIndex:
    """Sessions (tz-naive dates) that have bars but whose final bar (ending at the session close) is absent."""
    if df.empty:
        return pd.DatetimeIndex([])
    last_bar = pd.Series(df.index, index=_naive_days(df.index)).groupby(level=0).max()
    close = calendar["session_close"].reindex(last_bar.index)
    return pd.DatetimeIndex(last_bar.index[(last_bar + pd.Timedelta(minutes=bar_minutes) < close).to_numpy()])


def resample_daily(df: pd.DataFrame, calendar: pd.DataFrame, label: str = "") -> pd.DataFrame:
    """RTH 5Min bars -> one bar per session stamped at its NY midnight; sessions missing their last bar dropped and
    logged. vwap = Σ bar_vwap·volume / Σ volume."""
    df = to_ny(df)
    bad = sessions_missing_last_bar(df, calendar, 5)
    if len(bad):
        _log(f"{label}dropping {len(bad)} session(s) missing their last bar: {[str(d.date()) for d in bad]}")
        df = df[~_naive_days(df.index).isin(bad)]
    key = midnight(df.index)
    g = df.assign(_pv=df["vwap"] * df["volume"]).groupby(key, sort=True)
    out = g.agg(
        open=("open", "first"),
        high=("high", "max"),
        low=("low", "min"),
        close=("close", "last"),
        volume=("volume", "sum"),
        _pv=("_pv", "sum"),
        trade_count=("trade_count", "sum"),
    )
    out["vwap"] = out["_pv"] / out["volume"].where(out["volume"] > 0)
    out.index = pd.DatetimeIndex(out.index).as_unit("ns").rename(None)
    return out[df.columns.tolist()]


def snapshot(df: pd.DataFrame, calendar: pd.DataFrame) -> pd.DataFrame:
    """RTH 5Min bars -> per session the state at the `t1545` decision: bars ending at or before
    session close − 15 min only. Index: session NY midnight; columns SNAP_COLUMNS."""
    df = to_ny(df)
    if df.empty:
        return pd.DataFrame(columns=SNAP_COLUMNS, index=pd.DatetimeIndex([], tz=NY_TZ), dtype="float64")
    days = _naive_days(df.index)
    cutoff = decision_times(days, calendar, DecisionTime.T1545)
    seen = df[(df.index + pd.Timedelta(minutes=5)) <= cutoff]
    g = seen.assign(_pv=seen["vwap"] * seen["volume"]).groupby(midnight(seen.index), sort=True)
    out = g.agg(
        last=("close", "last"), volume=("volume", "sum"), _pv=("_pv", "sum"), n_bars=("close", "size")
    )
    out["vwap"] = out["_pv"] / out["volume"].where(out["volume"] > 0)
    out["n_bars"] = out["n_bars"].astype("float64")
    out.index = pd.DatetimeIndex(out.index).as_unit("ns").rename(None)
    return out[SNAP_COLUMNS]


# ── Fetch and cache ─────────────────────────────────────────────────────────


def _cross(source, symbol: str, at: pd.Timestamp, condition: str) -> list[float]:
    """[price, size, n_distinct_prices, window index] of the listing exchange's cross at or after `at`."""
    for k, width in enumerate(AUCTION_WINDOWS):
        hit = auction_from_trades(source.trades(symbol, at, at + width), PRIMARY_EXCHANGE[symbol], condition)
        if hit is not None:
            return [*hit, float(k)]
    return [float("nan"), float("nan"), 0.0, float("nan")]


def _fetch_auction(source, symbol, start, end, calendar) -> pd.DataFrame:
    """One row per session in [start, end): opening and closing cross (AUCTION_COLUMNS); missing crosses logged."""
    if symbol not in PRIMARY_EXCHANGE:
        raise ValueError(f"no listing exchange configured for {symbol}; add it to alpaca.PRIMARY_EXCHANGE")
    sess = calendar[(calendar.index >= start) & (calendar.index < end)]
    rows = []
    for o, c in zip(sess["session_open"], sess["session_close"], strict=True):
        op, osz, on, ow = _cross(source, symbol, o, OPEN_CROSS)
        cp, csz, cn, cw = _cross(source, symbol, c, CLOSE_CROSS)
        rows.append([op, cp, osz, csz, on, cn, ow, cw])
    index = pd.DatetimeIndex(sess.index).tz_localize(NY_TZ).as_unit("ns").rename(None)
    out = pd.DataFrame(rows, columns=AUCTION_COLUMNS, index=index, dtype="float64")
    for side in ("open", "close"):
        miss = out.index[out[side].isna()]
        if len(miss):
            days = [str(d.date()) for d in miss[:10]]
            _log(f"{symbol} 1DayAuction: no {side} cross in {len(miss)} session(s): {days}")
    return out


def _fetch(source, symbol, timeframe, start, end, adjustment, calendar) -> pd.DataFrame:
    """Native bars for NY days [start, end), fetched in yearly chunks; daily -> session_filter, 5Min -> RTH."""
    if timeframe == "1DayAuction":
        return _fetch_auction(source, symbol, start, end, calendar)
    parts, a = [], start
    while a < end:
        b = min(a + FETCH_CHUNK, end)
        parts.append(source.bars(symbol, timeframe, a.tz_localize(NY_TZ), b.tz_localize(NY_TZ), adjustment))
        a = b
    df = to_ny(pd.concat(parts))
    df = df[~df.index.duplicated(keep="last")].sort_index()
    minutes = NATIVE_MINUTES[timeframe]
    return session_filter(df, calendar) if minutes is None else rth_filter(df, calendar, minutes)


def _adjustment_changed(cached: pd.DataFrame, fresh: pd.DataFrame) -> bool:
    common = cached.index.intersection(fresh.index)
    if common.empty:
        return False
    a, b = cached.loc[common, "close"], fresh.loc[common, "close"]
    return bool(((a - b).abs() / b.abs()).max() > ADJ_RTOL)


def _verified_end(df, calendar, lo, hi, prev_end, label) -> pd.Timestamp:
    """Coverage end to record: `hi`, unless trailing sessions came back empty (then stop after the last session
    with bars so a later load re-fetches them). Empty sessions inside the range are logged."""
    days = _naive_days(df.index).unique()
    sessions = calendar.index[(calendar.index >= lo) & (calendar.index < hi)]
    empty = sessions[~sessions.isin(days)]
    if empty.empty:
        return hi
    head, tail = empty[empty < days[0]], empty[empty > days[-1]]
    middle = empty[(empty > days[0]) & (empty < days[-1])]
    if len(head):
        _log(f"{label}no bars in {len(head)} session(s) {head[0].date()} -> {head[-1].date()}")
    if len(middle):
        _log(
            f"{label}no bars in {len(middle)} session(s) inside the range: {[str(d.date()) for d in middle[:10]]}"
        )
    if len(tail):
        new_hi = days[-1] + timedelta(days=1)
        if prev_end is not None:
            new_hi = max(new_hi, prev_end)
        _log(f"{label}no bars for {len(tail)} trailing session(s); coverage ends {new_hi.date()}")
        return new_hi
    return hi


def _native(
    symbol, timeframe, start, end, *, vdir, frozen, feed, adjustment, source, calendar
) -> pd.DataFrame:
    base = vdir / feed / adjustment / timeframe / symbol
    ident = {"symbol": symbol, "timeframe": timeframe, "feed": feed, "adjustment": adjustment}
    label = f"{symbol} {timeframe}: "
    exists = base.with_suffix(".json").exists()
    if frozen:
        if not exists:
            raise FrozenVintageError(f"{vdir.name} has no {feed}/{adjustment}/{timeframe}/{symbol}")
        df, meta = load_frame(base, expect=ident)
        cs, ce = _day(meta["coverage_start"]), _day(meta["coverage_end"])
        if start < cs or end > ce:
            raise FrozenVintageError(
                f"{label}{vdir.name} covers [{cs.date()}, {ce.date()}); requested [{start.date()}, {end.date()})"
            )
        return df

    def src():
        nonlocal source
        if source is None:
            source = AlpacaSource(feed=feed)
        return source

    if not exists:
        lo, hi, ce = start, end, None
        df = _fetch(src(), symbol, timeframe, lo, hi, adjustment, calendar)
        changed = True
    else:
        df, meta = load_frame(base, expect=ident)
        cs, ce = _day(meta["coverage_start"]), _day(meta["coverage_end"])
        lo, hi = min(start, cs), max(end, ce)
        changed = lo < cs or hi > ce
        if changed:
            fresh = []
            if lo < cs:
                fresh.append(
                    _fetch(src(), symbol, timeframe, lo, min(cs + OVERLAP, ce), adjustment, calendar)
                )
            if hi > ce:
                fresh.append(
                    _fetch(src(), symbol, timeframe, max(ce - OVERLAP, cs), hi, adjustment, calendar)
                )
            fresh = [f for f in fresh if not f.empty]
            if any(_adjustment_changed(df, f) for f in fresh):
                _log(f"{label}cached prices no longer match Alpaca's adjustment; re-fetching the full range")
                df = _fetch(src(), symbol, timeframe, lo, hi, adjustment, calendar)
            else:
                for f in fresh:
                    df = merge_bars(df, f)
    if changed:
        if df.empty:
            raise ValueError(
                f"Alpaca returned no {timeframe} bars for {symbol} in [{lo.date()}, {hi.date()})"
            )
        hi = _verified_end(df, calendar, lo, hi, ce, label)
        meta = {**ident, "source": "alpaca", "coverage_start": lo.date().isoformat(),
                "coverage_end": hi.date().isoformat(), "fetched_at": datetime.now().astimezone().isoformat()}  # fmt: skip
        save_frame(base, df, meta)
    return df


def _adjust(raw: pd.DataFrame, symbol: str, vintage: str, cache_root: Path, source) -> pd.DataFrame:
    """Total-return adjust a cached 1DayAuction frame with the reconciled dividend table over its whole span (so a
    slice gets the same factors as the full frame)."""
    from vwap_mr.data.dividends import adjustment_factor, load_dividends

    days = pd.DatetimeIndex(raw.index).tz_convert(NY_TZ).tz_localize(None)
    divs = load_dividends(symbol, days[0], days[-1] + pd.Timedelta(days=1), vintage=vintage, cache_root=cache_root,
                          source=source)  # fmt: skip
    factor = adjustment_factor(raw["close"], divs)
    out = raw.rename(columns={"open": "open_raw", "close": "close_raw"})
    out.insert(0, "open", out["open_raw"] * factor)
    out.insert(1, "close", out["close_raw"] * factor)
    out["dividend"] = divs["amount"].reindex(days).fillna(0.0).to_numpy()
    out["factor"] = factor
    return out


def log_forward_access(event: dict, path: Path = FORWARD_ACCESS) -> None:
    """Append one audit row for a read past HOLDOUT_START (PLAN.md §4)."""
    row = {"at": pd.Timestamp.now(tz="UTC").isoformat(), **event}
    path.parent.mkdir(parents=True, exist_ok=True)
    with open(path, "a", encoding="utf-8") as fh:
        fh.write(json.dumps(row, sort_keys=True, default=str) + "\n")


def load_bars(
    symbol: str,
    timeframe: str,
    start,
    end,
    *,
    vintage: str,
    allow_forward: bool = False,
    feed: str = "sip",
    adjustment: str = "all",
    cache_root: Path = CACHE_DIR,
    source=None,
    forward_log: Path = FORWARD_ACCESS,
) -> pd.DataFrame:
    """Bars for `symbol` over NY days [start, end) from the cache `vintage` (see the module docstring).

    Args:
        symbol: Ticker.
        timeframe: One of TIMEFRAMES.
        start, end: NY dates, [start, end).
        vintage: Cache name (`2026-10` -> `data/cache/vintage_2026-10/`, or `live`).
        allow_forward: Permit `end` past HOLDOUT_START (live cache only; audited).
        feed, adjustment: Alpaca request parameters; never substituted on failure.
        cache_root: Cache root directory.
        source: Object with `.bars` / `.calendar` (AlpacaSource, created only if a fetch is needed).
        forward_log: Audit file for forward reads.

    Returns:
        pd.DataFrame: NY-indexed bars (columns per timeframe).
    """
    if timeframe not in TIMEFRAMES:
        raise ValueError(f"unknown timeframe {timeframe!r}; expected one of {TIMEFRAMES}")
    symbol = symbol.upper()
    start, end = _day(start), _day(end)
    if end > HOLDOUT and not allow_forward:
        raise HoldoutError(
            f"{symbol}: requested bars through {end.date()}, past HOLDOUT_START {HOLDOUT.date()}"
        )
    if allow_forward and vintage != LIVE:
        raise HoldoutError(f"allow_forward is for the live cache only, not vintage {vintage!r}")
    vdir = vintage_dir(vintage, cache_root)
    frozen = (vdir / MANIFEST).exists()
    if not frozen:
        end = min(end, today_ny())
    if start >= end:
        raise ValueError(f"empty range [{start.date()}, {end.date()})")
    if end > HOLDOUT:
        log_forward_access({"event": "load_bars", "symbol": symbol, "timeframe": timeframe,
                            "start": start.date(), "end": end.date(), "vintage": vintage}, forward_log)  # fmt: skip
    calendar = get_calendar(vdir, end, source=source)
    native_tf = DERIVED_FROM.get(timeframe, timeframe)
    auction = timeframe == "1DayAuction"
    if auction and adjustment not in ("all", "raw"):
        raise ValueError(f"1DayAuction adjustment is 'all' (own dividend table) or 'raw', not {adjustment!r}")
    df = _native(symbol, native_tf, start, end, vdir=vdir, frozen=frozen, feed=feed,
                 adjustment="raw" if auction else adjustment, source=source, calendar=calendar)  # fmt: skip
    if auction and adjustment == "all":
        df = _adjust(df, symbol, vintage, cache_root, source)
    out = df[(df.index >= start.tz_localize(NY_TZ)) & (df.index < end.tz_localize(NY_TZ))]
    if timeframe == "1DayRTH":
        return resample_daily(out, calendar, label=f"{symbol} {timeframe}: ")
    if timeframe == "snap1545":
        return snapshot(out, calendar)
    return out
