"""Data layer (U1). No network: Alpaca is FakeSource, HTTP is a fake callable."""

import json

import numpy as np
import pandas as pd
import pytest

from vwap_mr import config
from vwap_mr.backtest.costs import check_costable
from vwap_mr.config import DecisionTime
from vwap_mr.data import bars, calendar, credentials, exo, secondary
from vwap_mr.data.alpaca import BAR_COLUMNS, bars_from_raw, calendar_from_raw, midnight
from vwap_mr.data.alpaca import ts as ts_
from vwap_mr.data.bars import HoldoutError, load_bars, resample_daily, rth_filter, snapshot
from vwap_mr.data.calendar import decision_times
from vwap_mr.data.quality import quality_report
from vwap_mr.data.store import (
    CacheError,
    FrozenVintageError,
    freeze,
    load_frame,
    save_frame,
    verify_vintage,
    vintage_dir,
)
from vwap_mr.data.synthetic import gbm_5min, gbm_daily

NY = "America/New_York"
HALF_DAY, HOLIDAY = pd.Timestamp("2024-11-29"), pd.Timestamp("2024-11-28")
CAL_ROWS = [
    {"date": d.strftime("%Y-%m-%d"), "open": "09:30", "close": "13:00" if d == HALF_DAY else "16:00"}
    for d in pd.bdate_range("2015-01-01", "2027-12-31")
    if d != HOLIDAY
]


def _price(minutes: np.ndarray, factor: float) -> np.ndarray:
    return (100 + 5 * np.sin(minutes / 97.0) + (minutes % 7) * 0.01) * factor


class FakeSource:
    """Deterministic 5Min bars 04:00-20:00 NY on every weekday (extended hours and the holiday included) and a daily
    bar per session stamped at NY midnight whose open/close are the 09:30 open and the session-close print and whose
    volume includes extended hours (as Alpaca's daily bar)."""

    def __init__(self, factor: float = 1.0):
        self.factor = factor
        self.bar_calls: list[tuple] = []
        self.calendar_calls = 0

    def calendar(self, start, end):
        self.calendar_calls += 1
        return calendar_from_raw([r for r in CAL_ROWS if str(start) <= r["date"] <= str(end)])

    def _five(self, start, end):
        days = pd.bdate_range(start.tz_localize(None).normalize(), end.tz_localize(None), inclusive="left")
        offs = pd.timedelta_range("04:00:00", "19:55:00", freq="5min")
        idx = pd.DatetimeIndex([d + o for d in days for o in offs]).tz_localize(NY)
        idx = idx[(idx >= start) & (idx < end)]
        t = (idx.as_unit("ns").asi8 // 60_000_000_000).astype(float)
        close = _price(t, self.factor)
        return pd.DataFrame(
            {"open": close - 0.02, "high": close + 0.05, "low": close - 0.05, "close": close,
             "volume": 1000 + t % 13, "vwap": close - 0.01, "trade_count": 10.0},
            index=idx.tz_convert("UTC"),
        )[BAR_COLUMNS]  # fmt: skip

    def bars(self, symbol, timeframe, start, end, adjustment):
        self.bar_calls.append((symbol, timeframe, start, end))
        if timeframe == "5Min":
            return self._five(start, end)
        assert timeframe == "1DayPrint"
        cal = calendar_from_raw(CAL_ROWS)
        days = cal.index[(cal.index >= start.tz_localize(None)) & (cal.index < end.tz_localize(None))]
        rows = []
        for d in days:
            five = self._five(
                pd.Timestamp(d, tz=NY), pd.Timestamp(d, tz=NY) + pd.Timedelta(days=1)
            ).tz_convert(NY)
            close_at = cal.loc[d, "session_close"]
            m_open = pd.Timestamp(d, tz=NY) + pd.Timedelta(hours=9, minutes=30)
            px = lambda ts: float(
                _price(np.array([ts.as_unit("ns").value // 60_000_000_000], float), self.factor)[0]
            )
            rows.append({"open": px(m_open) + 0.003, "high": five["high"].max(), "low": five["low"].min(),
                         "close": px(close_at) + 0.007, "volume": five["volume"].sum(), "vwap": five["vwap"].mean(),
                         "trade_count": 100.0})  # fmt: skip
        idx = pd.DatetimeIndex(days).tz_localize(NY).tz_convert("UTC")
        return pd.DataFrame(rows, index=idx, columns=BAR_COLUMNS).astype("float64")


@pytest.fixture
def cal():
    return calendar_from_raw(CAL_ROWS)


@pytest.fixture
def root(tmp_path, monkeypatch):
    monkeypatch.setattr(bars, "today_ny", lambda: pd.Timestamp("2025-01-10"))
    monkeypatch.setattr(calendar, "today_ny", lambda: pd.Timestamp("2025-01-10"))
    monkeypatch.setattr(exo, "today_ny", lambda: pd.Timestamp("2026-10-20"))
    return tmp_path


def _load(root, tf, start, end, src=None, vintage="t", **kw):
    return load_bars(
        "SPY", tf, start, end, vintage=vintage, cache_root=root, source=src or FakeSource(), **kw
    )


# ── Source parsing, time zones, credentials ─────────────────────────────────


def test_bars_from_raw_is_utc_and_bar_open(root):
    raw = [{"t": "2024-11-27T14:30:00Z", "o": 1, "h": 2, "l": 0.5, "c": 1.5, "v": 100, "n": 3, "vw": 1.2}]
    df = bars_from_raw(raw)
    assert (
        str(pd.DatetimeIndex(df.index).tz) == "UTC"
        and list(df.columns) == BAR_COLUMNS
        and (df.dtypes == "float64").all()
    )
    assert df.index[0] == pd.Timestamp("2024-11-27 09:30", tz=NY)
    five = _load(root, "5Min", "2024-07-08", "2024-07-09")  # EDT: 09:30 NY = 13:30Z
    assert str(five.index.tz) == NY and five.index[0] == pd.Timestamp("2024-07-08 13:30", tz="UTC")
    daily = _load(root, "1DayPrint", "2024-11-25", "2024-12-03")
    assert (daily.index == midnight(daily.index)).all() and str(daily.index.tz) == NY


def test_missing_credentials_name_only(tmp_path, monkeypatch):
    env = tmp_path / ".env"
    env.write_text("ALPACA_API_KEY=supersecretkey123\n")
    monkeypatch.setattr(credentials, "ENV_PATH", env)
    for n in ("ALPACA_API_KEY", "ALPACA_API_SECRET"):
        monkeypatch.delenv(n, raising=False)
    with pytest.raises(credentials.MissingCredentialsError) as e:
        credentials.credential("ALPACA_API_KEY", "ALPACA_API_SECRET")
    assert "ALPACA_API_SECRET" in str(e.value) and "supersecretkey123" not in str(e.value)
    import os

    assert "ALPACA_API_KEY" not in os.environ


def test_http_get_never_echoes_query_string(monkeypatch):
    import requests

    class R:
        status_code = 403

    monkeypatch.setattr(requests, "get", lambda *a, **k: R())
    with pytest.raises(ConnectionError) as e:
        exo.http_get(exo.FRED_API_URL, {"api_key": "SECRETFREDKEY"})
    assert "SECRETFREDKEY" not in str(e.value) and "403" in str(e.value)

    def boom(*a, **k):
        raise requests.ConnectionError(f"failed for {a[0]}?api_key=SECRETFREDKEY")

    monkeypatch.setattr(requests, "get", boom)
    with pytest.raises(ConnectionError) as e:
        exo.http_get(exo.FRED_API_URL, {"api_key": "SECRETFREDKEY"})
    assert "SECRETFREDKEY" not in str(e.value) and e.value.__cause__ is None and e.value.__suppress_context__


# ── Sessions: RTH, half-day, DST, resample parity, snapshot ─────────────────


def _day5(day):
    d = pd.Timestamp(day, tz=NY)
    return FakeSource()._five(d, d + pd.Timedelta(days=1))


def test_rth_filter_regular_half_day_holiday_dst(cal):
    reg = rth_filter(_day5("2024-11-27"), cal, 5)
    assert (
        len(reg) == 78
        and reg.index[0].strftime("%H:%M") == "09:30"
        and reg.index[-1].strftime("%H:%M") == "15:55"
    )
    half = rth_filter(_day5("2024-11-29"), cal, 5)
    assert len(half) == 42 and half.index[-1].strftime("%H:%M") == "12:55"
    assert rth_filter(_day5("2024-11-28"), cal, 5).empty
    for day in ("2024-03-08", "2024-03-11", "2024-11-01", "2024-11-04"):  # both DST transitions
        r = rth_filter(_day5(day), cal, 5)
        assert len(r) == 78 and r.index[0].strftime("%H:%M") == "09:30", day


def test_1dayrth_matches_hand_computed_bar(root):
    five = _load(root, "5Min", "2024-11-25", "2024-12-03")
    daily = _load(root, "1DayRTH", "2024-11-25", "2024-12-03")
    assert len(daily) == 5 and pd.Timestamp("2024-11-28", tz=NY) not in daily.index
    for day in ("2024-11-27", "2024-11-29"):
        d = five.loc[day]
        row = daily.loc[pd.Timestamp(day, tz=NY)]
        assert row["open"] == d["open"].iloc[0] and row["close"] == d["close"].iloc[-1]
        assert row["high"] == d["high"].max() and row["low"] == d["low"].min()
        assert row["volume"] == d["volume"].sum()
        assert row["vwap"] == pytest.approx((d["vwap"] * d["volume"]).sum() / d["volume"].sum(), rel=1e-15)
    assert (
        daily.loc[pd.Timestamp("2024-11-29", tz=NY), "close"]
        == five.loc[pd.Timestamp("2024-11-29 12:55", tz=NY), "close"]
    )


def test_1dayprint_volume_includes_extended_hours_in_fake(root):
    """The fake mirrors what U1 measures for real: the print bar's volume vs the RTH sum."""
    p = _load(root, "1DayPrint", "2024-11-25", "2024-12-03")
    r = _load(root, "1DayRTH", "2024-11-25", "2024-12-03")
    assert (p["volume"] / r["volume"] > 1).all()


def test_snapshot_is_cut_at_close_minus_15(root, cal):
    five = _load(root, "5Min", "2024-11-27", "2024-12-03")
    snap = _load(root, "snap1545", "2024-11-27", "2024-12-03")
    d = five.loc["2024-11-27"]
    seen = d[d.index <= pd.Timestamp("2024-11-27 15:40", tz=NY)]
    row = snap.loc[pd.Timestamp("2024-11-27", tz=NY)]
    assert (
        row["n_bars"] == 75
        and row["last"] == seen["close"].iloc[-1]
        and row["volume"] == seen["volume"].sum()
    )
    assert row["vwap"] == pytest.approx(
        (seen["vwap"] * seen["volume"]).sum() / seen["volume"].sum(), rel=1e-15
    )
    half = snap.loc[pd.Timestamp("2024-11-29", tz=NY)]  # early close 13:00 -> cut at 12:45
    assert half["n_bars"] == 39 and half["last"] == five.loc[pd.Timestamp("2024-11-29 12:40", tz=NY), "close"]


def test_snapshot_causal_and_mutant_caught(cal):
    """Perturbing every bar that ends after the 15:45 cut leaves the snapshot unchanged; a snapshot that reads one
    bar further (the planted peek) is caught by the same check."""
    five = rth_filter(pd.concat([_day5(d) for d in ("2024-11-25", "2024-11-26", "2024-11-27")]), cal, 5)
    cut = decision_times(five.index, cal, DecisionTime.T1545)
    after = (five.index + pd.Timedelta(minutes=5)) > cut
    rng = np.random.default_rng(7)
    noisy = five.copy()
    noisy.loc[after, ["close", "vwap", "volume"]] *= rng.uniform(0.5, 1.5, (int(after.sum()), 3))
    pd.testing.assert_frame_equal(snapshot(five, cal), snapshot(noisy, cal))

    def peek(df, c):  # the mutant: cut 5 minutes late
        late = c.copy()
        late["session_close"] = late["session_close"] + pd.Timedelta(minutes=5)
        return snapshot(df, late)

    assert not peek(five, cal).equals(peek(noisy, cal))


# ── Cache, top-ups, re-adjustment ───────────────────────────────────────────


def test_cache_round_trip_and_hash_mismatch(tmp_path, cal):
    df = rth_filter(_day5("2024-11-27"), cal, 5)
    base = tmp_path / "v" / "SPY"
    save_frame(base, df, {"symbol": "SPY"})
    back, meta = load_frame(base, expect={"symbol": "SPY"})
    pd.testing.assert_frame_equal(back, df, check_freq=False)
    assert meta["n_rows"] == len(df)
    with pytest.raises(CacheError, match="symbol"):
        load_frame(base, expect={"symbol": "QQQ"})
    import pyarrow.parquet as pq

    t = pq.read_table(base.with_suffix(".parquet")).to_pandas()
    t.loc[3, "close"] += 0.01
    t.to_parquet(base.with_suffix(".parquet"), index=False)
    with pytest.raises(CacheError, match="hash"):
        load_frame(base)
    base.with_suffix(".json").unlink()
    with pytest.raises(CacheError, match="sidecar"):
        load_frame(base)


def test_cache_round_trip_with_datetime_column(tmp_path):
    idx = pd.DatetimeIndex(["2024-01-02", "2024-01-03"]).as_unit("ns")
    df = pd.DataFrame(
        {"value": [1.0, 2.0], "available_at": exo.available_at("VIX", idx).as_unit("ns")}, index=idx
    )
    save_frame(tmp_path / "x", df, {})
    pd.testing.assert_frame_equal(load_frame(tmp_path / "x")[0], df)


def test_top_up_without_duplicates(root):
    src = FakeSource()
    a = _load(root, "5Min", "2024-11-25", "2024-12-03", src)
    assert len(src.bar_calls) == 1
    again = _load(root, "5Min", "2024-11-26", "2024-12-02", src)
    assert len(src.bar_calls) == 1  # served from the cache
    pd.testing.assert_frame_equal(again, a.loc["2024-11-26":"2024-12-01"], check_freq=False)
    ext = _load(root, "5Min", "2024-11-18", "2024-12-10", src)
    assert len(src.bar_calls) == 3  # one prefix and one suffix fetch, each with the 7-day overlap
    assert not ext.index.has_duplicates and ext.index.is_monotonic_increasing
    full = _load(root / "fresh", "5Min", "2024-11-18", "2024-12-10")
    pd.testing.assert_frame_equal(ext, full, check_freq=False)
    meta = json.loads((vintage_dir("t", root) / "sip/all/5Min/SPY.json").read_text())
    assert (meta["coverage_start"], meta["coverage_end"]) == ("2024-11-18", "2024-12-10")


def test_readjustment_triggers_full_refetch(root, capsys):
    src = FakeSource()
    _load(root, "1DayPrint", "2024-11-01", "2024-11-15", src)
    src.factor = 0.9998  # a ~2 bp dividend re-adjustment of all history
    out = _load(root, "1DayPrint", "2024-11-01", "2024-11-22", src)
    assert "re-fetching the full range" in capsys.readouterr().out
    ref = _load(root / "r", "1DayPrint", "2024-11-01", "2024-11-22", FakeSource(0.9998))
    pd.testing.assert_frame_equal(out, ref, check_freq=False)


def test_end_clamped_to_today(root):
    df = _load(root, "1DayPrint", "2025-01-02", "2025-02-01")
    assert df.index[-1] == pd.Timestamp("2025-01-09", tz=NY)  # today (2025-01-10) is not a completed session
    meta = json.loads((vintage_dir("t", root) / "sip/all/1DayPrint/SPY.json").read_text())
    assert meta["coverage_end"] == "2025-01-10"


def test_errors_propagate_and_empty_raises(root):
    class Unauthorized(FakeSource):
        def bars(self, *a, **k):
            raise PermissionError("subscription does not permit querying recent SIP data")

    with pytest.raises(PermissionError):
        _load(root, "5Min", "2024-11-25", "2024-12-03", Unauthorized())

    class Empty(FakeSource):
        def bars(self, *a, **k):
            return super().bars(*a, **k).iloc[:0]

    with pytest.raises(ValueError, match="no 1DayPrint bars"):
        _load(root, "1DayPrint", "2024-11-25", "2024-12-03", Empty())
    with pytest.raises(ValueError, match="whole days"):
        _load(root, "5Min", "2024-11-25 10:00", "2024-12-03")


# ── Frozen vintage ──────────────────────────────────────────────────────────


def _snapshot_files(d):
    return {p: p.read_bytes() for p in sorted(d.rglob("*")) if p.is_file()}


def test_frozen_vintage_is_read_only(root):
    src = FakeSource()
    _load(root, "1DayPrint", "2024-11-01", "2024-12-01", src)
    vdir = vintage_dir("t", root)
    man = freeze(vdir)
    assert verify_vintage(vdir) == man["data_hash"]
    before, n_calls = _snapshot_files(vdir), len(src.bar_calls)
    inside = _load(root, "1DayPrint", "2024-11-05", "2024-11-20", src)
    assert len(inside) == 11 and len(src.bar_calls) == n_calls
    with pytest.raises(FrozenVintageError, match="covers"):
        _load(root, "1DayPrint", "2024-11-01", "2024-12-15", src)  # a top-up would write
    with pytest.raises(FrozenVintageError, match="has no"):
        _load(root, "5Min", "2024-11-05", "2024-11-20", src)
    with pytest.raises(FrozenVintageError):
        save_frame(vdir / "sip/all/1DayPrint/QQQ", inside, {})
    with pytest.raises(FrozenVintageError, match="already frozen"):
        freeze(vdir)
    assert _snapshot_files(vdir) == before and len(src.bar_calls) == n_calls
    with pytest.raises(ValueError, match="never frozen"):
        freeze(vintage_dir("live", root))


def test_frozen_vintage_detects_a_replaced_file(root):
    _load(root, "1DayPrint", "2024-11-01", "2024-12-01")
    vdir = vintage_dir("t", root)
    freeze(vdir)
    # a consistent parquet + sidecar pair written behind the manifest's back is still refused
    other = root / "other"
    _load(other, "1DayPrint", "2024-11-01", "2024-12-01", FakeSource(1.01))
    for ext in (".parquet", ".json"):
        src_p = vintage_dir("t", other) / f"sip/all/1DayPrint/SPY{ext}"
        (vdir / f"sip/all/1DayPrint/SPY{ext}").write_bytes(src_p.read_bytes())
    with pytest.raises(CacheError, match="MANIFEST"):
        _load(root, "1DayPrint", "2024-11-05", "2024-11-20")
    with pytest.raises(CacheError):
        verify_vintage(vdir)


# ── Holdout ─────────────────────────────────────────────────────────────────


def test_holdout_refusal_and_forward_audit(tmp_path, monkeypatch):
    monkeypatch.setattr(bars, "today_ny", lambda: pd.Timestamp("2026-03-02"))
    monkeypatch.setattr(calendar, "today_ny", lambda: pd.Timestamp("2026-03-02"))
    log = tmp_path / "forward_access.jsonl"
    src = FakeSource()

    def load(start, end, vintage, allow_forward=False):
        return load_bars("SPY", "1DayPrint", start, end, vintage=vintage, allow_forward=allow_forward,
                         cache_root=tmp_path, source=src, forward_log=log)  # fmt: skip

    with pytest.raises(HoldoutError):
        load("2025-12-01", "2026-01-02", "t")
    with pytest.raises(HoldoutError, match="live cache only"):
        load("2025-12-01", "2026-01-02", "t", allow_forward=True)
    ok = load("2025-12-01", config.HOLDOUT_START, "t")
    assert ok.index[-1] < pd.Timestamp(config.HOLDOUT_START, tz=NY) and not log.exists()
    fwd = load("2025-12-01", "2026-02-01", "live", allow_forward=True)
    assert fwd.index[-1] >= pd.Timestamp(config.HOLDOUT_START, tz=NY)
    rows = [json.loads(x) for x in log.read_text().splitlines()]
    assert len(rows) == 1 and rows[0]["event"] == "load_bars" and rows[0]["vintage"] == "live"
    with pytest.raises(HoldoutError):
        exo.load_series("VIX", "2025-01-01", "2026-01-02", vintage="t", cache_root=tmp_path, http=_fake_http)


# ── Exogenous: parsers, available_at at both decision times ─────────────────

VIX_CSV = "DATE,OPEN,HIGH,LOW,CLOSE\n" + "\n".join(
    f"{d:%m/%d/%Y},{20 + i},{21 + i},{19 + i},{20.5 + i}"
    for i, d in enumerate(pd.bdate_range("2025-12-22", "2026-01-09"))
)
FRED_JSON = json.dumps({"observations": [
    {"date": "2024-11-26", "value": "4.40"}, {"date": "2024-11-27", "value": "4.41"},
    {"date": "2024-11-28", "value": "."}, {"date": "2024-11-29", "value": "4.42"},
    {"date": "2026-01-02", "value": "3.90"},
]})  # fmt: skip


def _fake_http(url, params=None):
    params = params or {}
    if "cboe" in url:
        return VIX_CSV
    if "stlouisfed" in url:
        assert params["api_key"] == "k" and params["series_id"] == "DTB3"
        return FRED_JSON
    raise AssertionError(url)


def test_exo_cache_cuts_holdout_rows(root, monkeypatch):
    monkeypatch.setenv("FRED_API_KEY", "k")
    vix = exo.load_series(
        "VIX", "2025-12-01", config.HOLDOUT_START, vintage="t", cache_root=root, http=_fake_http
    )
    assert vix.index[-1] == pd.Timestamp("2025-12-31")
    cached, meta = load_frame(vintage_dir("t", root) / "exo/VIX")
    assert cached.index[-1] < pd.Timestamp(config.HOLDOUT_START) and meta["coverage_end"] == "2026-01-01"
    dtb3 = exo.load_series(
        "DTB3", "2024-01-01", config.HOLDOUT_START, vintage="t", cache_root=root, http=_fake_http
    )
    assert list(dtb3["value"]) == [4.40, 4.41, 4.42]  # '.' dropped, 2026 row cut


def test_available_at_vix_close_vs_t1545(cal):
    idx = pd.DatetimeIndex(["2024-11-26", "2024-11-27", "2024-11-29"])
    frame = pd.DataFrame(
        {"value": [15.0, 16.0, 17.0], "available_at": exo.available_at("VIX", idx)}, index=idx
    )
    e = exo.Exo({"VIX": frame})
    sessions = pd.DatetimeIndex(["2024-11-27", "2024-11-29"])
    at_close = decision_times(sessions, cal, DecisionTime.CLOSE)
    at_1545 = decision_times(sessions, cal, DecisionTime.T1545)
    assert at_close[0] == pd.Timestamp("2024-11-27 16:25", tz=NY) and at_1545[0] == pd.Timestamp(
        "2024-11-27 15:45", tz=NY
    )
    assert at_close[1] == pd.Timestamp("2024-11-29 13:25", tz=NY) and at_1545[1] == pd.Timestamp(
        "2024-11-29 12:45", tz=NY
    )
    np.testing.assert_array_equal(e.asof("VIX", at_close), [16.0, 16.0])  # 11-29 early close: 13:25 < 16:20
    np.testing.assert_array_equal(
        e.asof("VIX", at_1545), [15.0, 16.0]
    )  # t1545 sees the previous session only
    assert e.asof("VIX", pd.Timestamp("2024-11-27 16:20", tz=NY))[0] == 16.0  # boundary: available_at <= when
    assert e.asof("VIX", pd.Timestamp("2024-11-27 16:19:59", tz=NY))[0] == 15.0
    assert np.isnan(e.asof("VIX", pd.Timestamp("2024-11-26 16:00", tz=NY))[0])
    with pytest.raises(ValueError, match="tz-aware"):
        e.asof("VIX", pd.Timestamp("2024-11-27 16:25"))


def test_available_at_dtb3_h15_rule():
    idx = pd.DatetimeIndex(["2024-11-27", "2024-11-29", "2024-12-31"])
    got = exo.available_at("DTB3", idx).tz_convert(NY)
    # Wed -> Thu is Thanksgiving -> Fri 16:30; Fri -> Mon 16:30; 12-31 -> 01-02 (New Year's Day)
    assert list(got) == [
        pd.Timestamp(x, tz=NY) for x in ("2024-11-29 16:30", "2024-12-02 16:30", "2025-01-02 16:30")
    ]
    sat = exo.next_business_day(
        pd.DatetimeIndex(["2024-11-30", "2024-11-28"])
    )  # never strictly-after-skipping
    assert list(sat) == [pd.Timestamp("2024-12-02"), pd.Timestamp("2024-11-29")]
    frame = pd.DataFrame({"value": [4.4, 4.5, 4.6], "available_at": exo.available_at("DTB3", idx)}, index=idx)
    e = exo.Exo({"DTB3": frame})
    np.testing.assert_array_equal(e.asof("DTB3", pd.DatetimeIndex(["2024-11-29 16:25", "2024-11-29 16:30"], tz=NY)),
                                  [np.nan, 4.4])  # fmt: skip


# ── diag: Yahoo parsing, splice, cost-model refusal ─────────────────────────


def _yahoo_json(days, close, adj, volume):
    ts = [int(ts_(ts_(d).tz_localize(NY) + pd.Timedelta(hours=9, minutes=30)).timestamp()) for d in days]
    q = {"open": list(close * 0.999), "high": list(close * 1.01), "low": list(close * 0.99), "close": list(close),
         "volume": list(volume)}  # fmt: skip
    return json.dumps({"chart": {"error": None, "result": [{"timestamp": ts, "indicators": {"quote": [q],
                       "adjclose": [{"adjclose": list(adj)}]}}]}})  # fmt: skip


def _diag_inputs(step_at_exdates: bool):
    days = pd.bdate_range("2015-01-02", "2016-12-30")
    rng = np.random.default_rng(3)
    close = 200 * np.exp(np.cumsum(rng.normal(0, 0.01, len(days))))
    factor = np.ones(len(days))
    for ex in pd.DatetimeIndex(["2015-03-20", "2015-06-19", "2015-09-18", "2015-12-18", "2016-03-18", "2016-06-17",
                                "2016-09-16", "2016-12-16"]):  # fmt: skip
        factor[days < ex] *= 0.995
    adj = close * factor
    raw = secondary.parse_yahoo_chart(_yahoo_json(days, close, adj, np.full(len(days), 1e8)))
    tr = pd.Series(adj * 1.2345, index=raw.index)  # Alpaca: total-return adjusted, another level
    if step_at_exdates:
        tr = pd.Series(close * 1.2345, index=raw.index)  # Alpaca split-only: ratio steps at every ex-date
    alp = pd.DataFrame({"close": tr, "volume": 9e7}, index=raw.index)
    return raw, alp[alp.index >= pd.Timestamp(config.DEV_START, tz=NY)]


def test_splice_constant_ratio_accepted():
    raw, alp = _diag_inputs(step_at_exdates=False)
    out, rep = secondary.splice(raw, alp)
    assert (
        rep["ratio_max_rel_dev"] < 1e-12
        and rep["close_corr"] > 0.9999
        and rep["ratio_median"] == pytest.approx(1.2345)
    )
    assert (
        out.index[-1] < pd.Timestamp(config.DEV_START, tz=NY)
        and out.attrs["source"] == "diag"
        and out["is_diag"].all()
    )
    first = raw.index[0]
    assert out.loc[first, "close"] == pytest.approx(raw.loc[first, "adjclose"] * 1.2345)
    assert out.loc[first, "close_split"] == raw.loc[first, "close"]


def test_splice_refuses_non_constant_ratio():
    raw, alp = _diag_inputs(step_at_exdates=True)
    with pytest.raises(secondary.SpliceError, match="not constant"):
        secondary.splice(raw, alp)


def test_cost_model_refuses_diag():
    raw, alp = _diag_inputs(step_at_exdates=False)
    out, _ = secondary.splice(raw, alp)
    with pytest.raises(secondary.DiagDataError):
        check_costable(out)
    stripped = out.copy()
    stripped.attrs = {}
    with pytest.raises(secondary.DiagDataError):  # the row flag survives a lost attrs
        check_costable(stripped)
    check_costable(alp)  # Alpaca bars pass


def test_yahoo_parser_refuses_missing_fields():
    days = pd.bdate_range("2015-01-02", periods=3)
    text = _yahoo_json(days, np.array([1.0, 2.0, 3.0]), np.array([1.0, 2.0, 3.0]), [1, 2, 3])
    blob = json.loads(text)
    blob["chart"]["result"][0]["indicators"]["quote"][0]["close"][1] = None
    with pytest.raises(ValueError, match="missing"):
        secondary.parse_yahoo_chart(json.dumps(blob))
    df = secondary.parse_yahoo_chart(text)
    assert list(df.index) == [pd.Timestamp(d, tz=NY) for d in days]


# ── Quality, synthetic ──────────────────────────────────────────────────────


def test_quality_report(root, cal):
    five = _load(root, "5Min", "2024-11-25", "2024-12-03")
    rep = quality_report(five, cal, "5Min")
    assert rep["missing_bars"] == 0 and rep["early_close_sessions"] == 1 and rep["ohlc_violations"] == 0
    assert rep["duplicate_stamps"] == 0 and rep["bars_off_calendar"] == 0
    holey = five.drop(five.loc["2024-11-26"].index)
    rep2 = quality_report(holey.drop(holey.index[[5]]), cal, "5Min")
    assert (
        rep2["sessions_missing"] == 1
        and rep2["missing_session_dates"] == "2024-11-26"
        and rep2["missing_bars"] == 79
    )
    bad = five.copy()
    bad.iloc[0, bad.columns.get_loc("low")] = bad.iloc[0]["high"] + 1
    assert quality_report(bad, cal, "5Min")["ohlc_violations"] == 1
    p = _load(root, "1DayPrint", "2024-11-25", "2024-12-03")
    assert quality_report(p, cal, "1DayPrint")["sessions_missing"] == 0


def test_synthetic_is_seeded_and_regime_vols_differ():
    a, _, reg = gbm_5min(40, seed=11)
    b, _, _ = gbm_5min(40, seed=11)
    pd.testing.assert_frame_equal(a, b)
    assert not a.equals(gbm_5min(40, seed=12)[0])
    assert (a["low"] <= a[["open", "close"]].min(axis=1)).all() and (
        a["high"] >= a[["open", "close"]].max(axis=1)
    ).all()
    daily, reg = gbm_daily(2000, seed=5, vols_ann=(0.10, 0.40), stay=0.99)
    r = np.log(daily["close"]).diff().to_numpy()[1:]
    lo, hi = r[reg[1:] == 0].std() * np.sqrt(252), r[reg[1:] == 1].std() * np.sqrt(252)
    assert 0.08 < lo < 0.12 and 0.33 < hi < 0.47
    assert len(daily) == 2000 and (daily.index == midnight(daily.index)).all()
    assert resample_daily(a, gbm_5min(40, seed=11)[1]).equals(gbm_daily(40, seed=11)[0])
