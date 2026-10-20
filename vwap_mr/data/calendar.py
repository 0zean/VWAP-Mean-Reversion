"""Exchange calendar from Alpaca (U1; ported from wavelet-meta-model `data/bars.py::get_calendar`) and the
decision instants of PLAN.md §8.2.

Contract: `calendar.json` lives in the vintage directory (raw Alpaca rows + fetched_at, written atomically); a
non-frozen cache re-fetches when the request passes its range or it is older than 30 days (unscheduled closures are
announced at short notice); a frozen vintage never re-fetches. Early closes are kept as published. A session the
data layer dropped is logged by the caller, never silently skipped."""

import json
from datetime import date, timedelta
from pathlib import Path

import pandas as pd

from vwap_mr.config import DecisionTime
from vwap_mr.data.alpaca import NY_TZ, AlpacaSource, calendar_from_raw, midnight, ts
from vwap_mr.data.store import CacheError, _file_sha, _read_manifest, atomic_write, frozen_root

CALENDAR_FILE = "calendar.json"
CALENDAR_FIRST_DAY = date(2015, 1, 1)  # a year of overlap before DEV_START for the diag splice
CALENDAR_MAX_AGE = pd.Timedelta(days=30)
CLOSE_DECISION_LAG = pd.Timedelta(minutes=25)  # `close`: 16:25 ET on a full day (PLAN.md §4, §8.2)
T1545_LEAD = pd.Timedelta(minutes=15)  # `t1545`: 15:45 ET on a full day


def _listed(root: Path, path: Path) -> str | None:
    return _read_manifest(root)["files"].get(path.resolve().relative_to(root).as_posix())


def today_ny() -> pd.Timestamp:
    return ts(pd.Timestamp.now(tz=NY_TZ).date())


def get_calendar(vdir: Path, end: pd.Timestamp, source=None) -> pd.DataFrame:
    """Sessions from CALENDAR_FIRST_DAY through at least `end` (a tz-naive NY date) for the vintage `vdir`."""
    path = Path(vdir) / CALENDAR_FILE
    root = frozen_root(path)
    frozen = root is not None
    if path.exists():
        if root is not None and _listed(root, path) != _file_sha(path):
            raise CacheError(f"{path}: sha256 differs from the manifest")
        blob = json.loads(path.read_text(encoding="utf-8"))
        cal = calendar_from_raw(blob["rows"])
        age = pd.Timestamp.now(tz="UTC") - pd.Timestamp(blob["fetched_at"])
        if frozen:
            if cal.index[-1] < end:
                raise CacheError(f"frozen calendar {path} ends {cal.index[-1].date()}, before {end.date()}")
            return cal
        if cal.index[-1] >= end and age <= CALENDAR_MAX_AGE:
            return cal
    elif frozen:
        raise CacheError(f"frozen vintage {vdir} has no {CALENDAR_FILE}")
    source = source or AlpacaSource()
    fetch_end = max(end, today_ny()) + timedelta(days=366)
    cal = source.calendar(CALENDAR_FIRST_DAY, fetch_end.date())
    rows = [
        {"date": d.strftime("%Y-%m-%d"), "open": o.strftime("%H:%M"), "close": c.strftime("%H:%M")}
        for d, o, c in zip(cal.index, cal["session_open"], cal["session_close"], strict=True)
    ]
    blob = {"fetched_at": pd.Timestamp.now(tz="UTC").isoformat(), "rows": rows}
    atomic_write(path, lambda fh: fh.write(json.dumps(blob).encode()))
    if cal.index[-1] < end:
        raise CacheError(f"Alpaca calendar ends {cal.index[-1].date()}, before the requested {end.date()}")
    return cal


def decision_times(sessions, calendar: pd.DataFrame, decision: DecisionTime) -> pd.DatetimeIndex:
    """The decision instant (tz-aware NY) for each session date: `close` = session close + 25 min (16:25 ET,
    13:25 on an early close); `t1545` = session close − 15 min (15:45 ET, 12:45 on an early close)."""
    days = pd.DatetimeIndex(sessions)
    if days.tz is not None:
        days = days.tz_convert(NY_TZ).tz_localize(None)
    days = midnight(days)
    closes = calendar["session_close"].reindex(days)
    if closes.isna().any():
        bad = days[closes.isna().to_numpy()]
        raise ValueError(f"not exchange sessions: {[str(d.date()) for d in bad[:5]]}")
    closes = pd.DatetimeIndex(closes)
    if decision == DecisionTime.CLOSE:
        return closes + CLOSE_DECISION_LAG
    if decision == DecisionTime.T1545:
        return closes - T1545_LEAD
    raise ValueError(f"unknown decision time {decision!r}")
