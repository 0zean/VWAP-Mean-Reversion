"""Data quality report (U1; ported from 0zean/wavelet-meta-model `data/quality.py`): missing, duplicate,
OHLC-violation and off-calendar rows per (symbol, timeframe) -> results/data_quality.csv.

Expected bars come from the exchange calendar between the first and last bar's sessions, so early closes count
as complete. Alpaca omits bars with no trades, so `missing_bars` > 0 is possible on 5Min; every missing session is
listed by date."""

import numpy as np
import pandas as pd

from vwap_mr.data.bars import _naive_days, sessions_missing_last_bar

QUALITY_TIMEFRAMES = {"1DayPrint": None, "1DayAuction": None, "5Min": 5}


def expected_bars_per_session(calendar: pd.DataFrame, bar_minutes: int | None) -> pd.Series:
    if bar_minutes is None:
        return pd.Series(1, index=calendar.index)
    minutes = (calendar["session_close"] - calendar["session_open"]).dt.total_seconds() / 60
    return np.ceil(minutes / bar_minutes).astype(int)


def quality_report(df: pd.DataFrame, calendar: pd.DataFrame, timeframe: str) -> dict:
    """Summary statistics for one bar set from `load_bars` (1DayPrint or 5Min)."""
    if timeframe not in QUALITY_TIMEFRAMES:
        raise ValueError(f"quality_report covers {list(QUALITY_TIMEFRAMES)}, not {timeframe!r}")
    if df.empty:
        raise ValueError("quality_report on an empty frame")
    minutes = QUALITY_TIMEFRAMES[timeframe]
    if timeframe == "1DayAuction":
        return _auction_report(df, calendar)
    day = _naive_days(df.index)
    sessions = calendar.loc[day[0] : day[-1]]
    expected = expected_bars_per_session(sessions, minutes)
    per_session = pd.Series(1, index=day).groupby(level=0).size().reindex(sessions.index, fill_value=0)
    missing_sessions = per_session.index[per_session == 0]
    o, h, lo, c = (df[k] for k in ("open", "high", "low", "close"))
    ohlc_bad = (lo > np.minimum(o, c)) | (h < np.maximum(o, c)) | (lo > h)
    no_last = sessions_missing_last_bar(df, calendar, minutes) if minutes else pd.DatetimeIndex([])
    return {
        "first": str(day[0].date()),
        "last": str(day[-1].date()),
        "n_bars": len(df),
        "n_sessions": len(sessions),
        "sessions_missing": len(missing_sessions),
        "missing_session_dates": " ".join(str(d.date()) for d in missing_sessions),
        "sessions_missing_last_bar": len(no_last),
        "missing_last_bar_dates": " ".join(str(d.date()) for d in no_last),
        "early_close_sessions": int((expected < expected.max()).sum()),
        "expected_bars": int(expected.sum()),
        "missing_bars": int((expected - per_session).clip(lower=0).sum()),
        "extra_bars": int((per_session - expected).clip(lower=0).sum()),
        "bars_off_calendar": int((~day.isin(calendar.index)).sum()),
        "duplicate_stamps": int(df.index.duplicated().sum()),
        "nan_rows": int(df[["open", "high", "low", "close", "volume"]].isna().any(axis=1).sum()),
        "zero_volume": int((df["volume"] <= 0).sum()),
        "nonpositive_price": int((df[["open", "high", "low", "close"]] <= 0).any(axis=1).sum()),
        "ohlc_violations": int(ohlc_bad.sum()),
    }


def _auction_report(df: pd.DataFrame, calendar: pd.DataFrame) -> dict:
    """1DayAuction: sessions without a cross, crosses found only in the wide window, crosses reported at more than
    one price, and the sessions the frame lacks against the calendar."""
    day = _naive_days(df.index)
    sessions = calendar.loc[day[0] : day[-1]]
    out = {"first": str(day[0].date()), "last": str(day[-1].date()), "n_bars": len(df), "n_sessions": len(sessions),
           "sessions_missing": int((~sessions.index.isin(day)).sum()),
           "duplicate_stamps": int(df.index.duplicated().sum()), "bars_off_calendar": int((~day.isin(calendar.index)).sum())}  # fmt: skip
    for side in ("open", "close"):
        miss = df.index[df[f"{side}_raw" if f"{side}_raw" in df else side].isna()]
        out[f"no_{side}_cross"] = len(miss)
        out[f"no_{side}_cross_dates"] = " ".join(str(d.date()) for d in miss)
        out[f"{side}_wide_window"] = int((df[f"{side}_wide"] == 1).sum())
        out[f"{side}_multi_price"] = int((df[f"{side}_nprice"] > 1).sum())
    return out
