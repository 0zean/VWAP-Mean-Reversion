"""Point-in-time exogenous series (U1; ported from 0zean/wavelet-meta-model `data/exo.py::load_series`,
`available_at` and `features/exo_align.py::Exo`).

    load_series(name, start, end, *, vintage) -> DataFrame indexed by observation date (tz-naive NY midnight) with
    `value` (float) and `available_at` (tz-aware UTC instant from which the row may be used)

Series and their `available_at` rules (PLAN.md §4):
- `VIX`: CBOE's VIX_History.csv, value = close, available at the observation date 16:20 ET (the index closes at
  16:15). A `close` decision (16:25) sees the same day's close; a `t1545` decision sees the previous session's.
  On an early close the `close` decision is 13:25 and still sees the previous close (conservative).
- `DTB3`: the FRED API (key `FRED_API_KEY`), the 3-month T-bill secondary-market rate in percent; H.15 rule: the
  next federal business day 16:30 ET. Holiday rows ('.') are dropped.

Cache: `<vintage>/exo/<name>.parquet` + sidecar (store.py). A fetch downloads the whole published history and
replaces the cache; a frozen vintage is never re-fetched. Rows dated on or after HOLDOUT_START are cut before
anything is cached, except in the `live` cache (forward reads are audited by the caller, as in bars.py)."""

import io
from collections.abc import Callable
from datetime import datetime
from pathlib import Path

import numpy as np
import pandas as pd

from vwap_mr.config import CACHE_DIR, HOLDOUT_START
from vwap_mr.data.alpaca import NY_TZ
from vwap_mr.data.bars import FORWARD_ACCESS, HOLDOUT, HoldoutError, _day, log_forward_access
from vwap_mr.data.calendar import today_ny
from vwap_mr.data.credentials import credential
from vwap_mr.data.store import LIVE, MANIFEST, FrozenVintageError, load_frame, save_frame, vintage_dir

HTTP_TIMEOUT = 60
USER_AGENT = "Mozilla/5.0 (vwap-mean-reversion research)"
CBOE_URL = "https://cdn.cboe.com/api/global/us_indices/daily_prices/{name}_History.csv"
FRED_API_URL = "https://api.stlouisfed.org/fred/series/observations"
SERIES = {"VIX": "cboe", "DTB3": "fred"}
CBOE_SETTLE = "16:20"
H15_RELEASE = "16:30"


def http_get(url: str, params: dict | None = None) -> str:
    """GET `url` as text. On failure raises with the status code and the URL **without** its query string, so an
    API key passed in `params` never reaches a log or traceback."""
    import requests

    try:
        r = requests.get(url, params=params, headers={"User-Agent": USER_AGENT}, timeout=HTTP_TIMEOUT)
    except requests.RequestException as e:
        raise ConnectionError(f"GET {url} failed: {type(e).__name__}") from None
    if r.status_code != 200:
        raise ConnectionError(f"GET {url} returned HTTP {r.status_code}")
    return r.text


# ── available_at rules ──────────────────────────────────────────────────────


def at_time(dates: pd.DatetimeIndex, hhmm: str) -> pd.DatetimeIndex:
    """NY wall-clock `hhmm` on each tz-naive date, as UTC instants."""
    h, m = (int(x) for x in hhmm.split(":"))
    return (pd.DatetimeIndex(dates) + pd.Timedelta(hours=h, minutes=m)).tz_localize(NY_TZ).tz_convert("UTC")


def next_business_day(dates: pd.DatetimeIndex) -> pd.DatetimeIndex:
    """The next US federal business day after each date (Federal Reserve release days)."""
    from pandas.tseries.holiday import USFederalHolidayCalendar

    d = pd.DatetimeIndex(dates).to_numpy().astype("datetime64[D]")
    hol = USFederalHolidayCalendar().holidays("1950-01-01", "2100-12-31").to_numpy().astype("datetime64[D]")
    return pd.DatetimeIndex(np.busday_offset(d, 1, roll="backward", holidays=hol).astype("datetime64[ns]"))


def available_at(name: str, dates: pd.DatetimeIndex) -> pd.DatetimeIndex:
    if name == "VIX":
        return at_time(dates, CBOE_SETTLE)
    if name == "DTB3":
        return at_time(next_business_day(dates), H15_RELEASE)
    raise ValueError(f"no available_at rule for {name!r}")


# ── Parsers ─────────────────────────────────────────────────────────────────


def _clean(df: pd.DataFrame, what: str) -> pd.DataFrame:
    if df.index.has_duplicates:
        raise ValueError(f"{what}: duplicate observation dates")
    if df.empty:
        raise ValueError(f"{what}: no observations")
    return df.sort_index().astype("float64")


def parse_cboe(text: str) -> pd.DataFrame:
    """CBOE index history CSV (DATE MM/DD/YYYY, OPEN, HIGH, LOW, CLOSE) -> value = close."""
    raw = pd.read_csv(io.StringIO(text))
    raw.columns = [c.strip().upper() for c in raw.columns]
    if not {"DATE", "CLOSE"} <= set(raw.columns):
        raise ValueError(f"unexpected CBOE columns {list(raw.columns)}")
    idx = pd.DatetimeIndex(pd.to_datetime(raw["DATE"], format="%m/%d/%Y"), name="date")
    return _clean(pd.DataFrame({"value": pd.to_numeric(raw["CLOSE"], errors="raise").to_numpy(float)}, index=idx),
                  "CBOE VIX")  # fmt: skip


def parse_fred_json(text: str) -> pd.DataFrame:
    """FRED API observations JSON -> value; '.' (no observation, e.g. a holiday) rows dropped."""
    import json

    obs = json.loads(text).get("observations")
    if not obs:
        raise ValueError("FRED response has no observations")
    dates = pd.DatetimeIndex(pd.to_datetime([o["date"] for o in obs]), name="date")
    vals = pd.to_numeric(pd.Series([o["value"] for o in obs]).replace(".", None), errors="raise")
    return _clean(pd.DataFrame({"value": vals.to_numpy(float)}, index=dates).dropna(), "FRED DTB3")


def fetch(name: str, http: Callable[..., str] = http_get) -> pd.DataFrame:
    """The whole published history of `name` (no `available_at` yet)."""
    if name == "VIX":
        return parse_cboe(http(CBOE_URL.format(name=name)))
    if name == "DTB3":
        (key,) = credential("FRED_API_KEY")
        return parse_fred_json(http(FRED_API_URL, {"series_id": name, "api_key": key, "file_type": "json"}))
    raise ValueError(f"unknown exogenous series {name!r}; expected one of {list(SERIES)}")


# ── Cache + load ────────────────────────────────────────────────────────────


def load_series(
    name: str,
    start,
    end,
    *,
    vintage: str,
    allow_forward: bool = False,
    cache_root: Path = CACHE_DIR,
    refresh: bool = False,
    http: Callable[..., str] = http_get,
    forward_log: Path = FORWARD_ACCESS,
) -> pd.DataFrame:
    """Observations of `name` dated in NY days [start, end), with `available_at` (module docstring)."""
    if name not in SERIES:
        raise ValueError(f"unknown exogenous series {name!r}; expected one of {list(SERIES)}")
    start, end = _day(start), _day(end)
    if end > HOLDOUT and not allow_forward:
        raise HoldoutError(f"{name}: requested data through {end.date()}, past HOLDOUT_START {HOLDOUT_START}")
    if allow_forward and vintage != LIVE:
        raise HoldoutError(f"allow_forward is for the live cache only, not vintage {vintage!r}")
    if start >= end:
        raise ValueError(f"empty range [{start.date()}, {end.date()})")
    if end > HOLDOUT:
        log_forward_access({"event": "load_series", "name": name, "start": start.date(), "end": end.date(),
                            "vintage": vintage}, forward_log)  # fmt: skip
    vdir = vintage_dir(vintage, cache_root)
    base = vdir / "exo" / name
    ident = {"name": name, "source": SERIES[name]}
    frozen = (vdir / MANIFEST).exists()
    need = min(end, today_ny())
    df = None
    if base.with_suffix(".json").exists():
        df, meta = load_frame(base, expect=ident)
        if frozen and _day(meta["coverage_end"]) < need:
            raise FrozenVintageError(
                f"{name}: {vdir.name} covers to {meta['coverage_end']}, requested {need.date()}"
            )
        if not frozen and (refresh or _day(meta["coverage_end"]) < need):
            df = None
    elif frozen:
        raise FrozenVintageError(f"{vdir.name} has no exo/{name}")
    if df is None:
        df = _fetch_and_cache(name, base, ident, vintage, http)
    return df[(df.index >= start) & (df.index < end)]


def _fetch_and_cache(name, base, ident, vintage, http) -> pd.DataFrame:
    df = fetch(name, http)
    cut = today_ny() if vintage == LIVE else min(today_ny(), HOLDOUT)
    df = df[df.index < cut].copy()  # research caches never hold holdout rows
    df.index = pd.DatetimeIndex(df.index, name=None).as_unit("ns")
    df["available_at"] = available_at(name, df.index).as_unit("ns")
    meta = {**ident, "coverage_start": str(df.index[0].date()), "coverage_end": str(cut.date()),
            "fetched_at": datetime.now().astimezone().isoformat()}  # fmt: skip
    save_frame(base, df, meta)
    print(f"[EXO]   {name}: {len(df):,} observations {df.index[0].date()} -> {df.index[-1].date()}")
    return df


# ── Point-in-time view ──────────────────────────────────────────────────────


def _ns_utc(when) -> np.ndarray:
    idx = pd.DatetimeIndex([when]) if isinstance(when, datetime) else pd.DatetimeIndex(when)
    if idx.tz is None:
        raise ValueError("decision instants must be tz-aware")
    return idx.tz_convert("UTC").as_unit("ns").asi8


class Exo:
    """Point-in-time view: a row is used at instant `when` only if `available_at <= when`."""

    def __init__(self, frames: dict[str, pd.DataFrame]):
        for n, f in frames.items():
            if "available_at" not in f or pd.DatetimeIndex(f["available_at"]).tz is None:
                raise ValueError(f"exo series {n!r} needs a tz-aware `available_at` column")
            if f["available_at"].isna().any():
                raise ValueError(f"exo series {n!r} has rows without `available_at`")
        self._frames = {n: f.sort_index() for n, f in frames.items()}

    def asof(self, name: str, when, *, max_age: pd.Timedelta | None = None) -> np.ndarray:
        """Value of the visible row with the greatest available_at (ties: the latest date) at each instant in
        `when` (tz-aware scalar or index); NaN where nothing is visible yet. With `max_age`, a visible row whose
        observation date (NY midnight) is older than `max_age` before the instant raises (a missing day must not
        silently serve an old value; U1 review)."""
        if name not in self._frames:
            raise KeyError(f"exo series {name!r} not loaded (have {sorted(self._frames)})")
        f = self._frames[name]
        avail = pd.DatetimeIndex(f["available_at"]).tz_convert("UTC").as_unit("ns").asi8
        dates = pd.DatetimeIndex(f.index).as_unit("ns").asi8
        order = np.lexsort((dates, avail))
        at = _ns_utc(when)
        pos = np.searchsorted(avail[order], at, side="right") - 1
        out = np.full(len(pos), np.nan)
        ok = pos >= 0
        out[ok] = f["value"].to_numpy(float)[order][pos[ok]]
        if max_age is not None and ok.any():
            obs = (
                pd.DatetimeIndex(dates[order][pos[ok]])
                .tz_localize(NY_TZ)
                .tz_convert("UTC")
                .as_unit("ns")
                .asi8
            )
            stale = at[ok] - obs > max_age.value
            if stale.any():
                first = pd.Timestamp(at[ok][stale][0], tz="UTC").tz_convert(NY_TZ)
                raise ValueError(
                    f"{name}: {int(stale.sum())} as-of value(s) older than {max_age}, first at {first}"
                )
        return out
