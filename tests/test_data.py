"""Data layer (U1). No network: Alpaca is FakeSource, HTTP is a fake callable."""

import json

import numpy as np
import pandas as pd
import pytest

from vwap_mr import config
from vwap_mr.backtest.costs import check_costable
from vwap_mr.config import DecisionTime
from vwap_mr.data import bars, calendar, credentials, dividends, exo, secondary
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

    def __init__(self, factor: float = 1.0, divs: dict | None = None):
        self.factor = factor
        self.bar_calls: list[tuple] = []
        self.trade_calls = 0
        self.calendar_calls = 0
        self.delay: dict = {}  # "YYYY-MM-DD open|close" -> cross printed late
        self.missing: set[str] = set()  # "YYYY-MM-DD open|close" -> no cross at all
        self.divs = divs if divs is not None else {"2024-11-27": 0.5}

    @staticmethod
    def cross(day, side: str) -> float:
        """The fake listing-exchange cross price of a session (distinct from every bar price)."""
        d = pd.Timestamp(day)
        return round(200 + d.dayofyear * 0.37 + (0.11 if side == "open" else 0.23), 2)

    def trades(self, symbol, start, end):
        """SIP-like trades around each session bound in [start, end): the primary cross (reported twice), a decoy
        cross at another venue at another price, and a larger unflagged trade at the primary."""
        self.trade_calls += 1
        cal = calendar_from_raw(CAL_ROWS)
        out = []
        for d, row in cal[(cal.index >= start.tz_localize(None).normalize() - pd.Timedelta(days=1))
                          & (cal.index <= end.tz_localize(None))].iterrows():  # fmt: skip
            for side, cond in (("open", "O"), ("close", "6")):
                key = f"{d.date()} {side}"
                at = row[f"session_{side}"] + self.delay.get(key, pd.Timedelta(milliseconds=80))
                if key in self.missing or not (start <= at < end):
                    continue
                px = self.cross(d, side)
                stamp = at.tz_convert("UTC").isoformat()
                out += [{"t": stamp, "p": px, "s": 5000, "x": "P", "c": [" ", cond]},
                        {"t": stamp, "p": px, "s": 5000, "x": "P", "c": [" ", cond]},
                        {"t": stamp, "p": px + 0.5, "s": 100, "x": "T", "c": [" ", cond, "X"]},
                        {"t": stamp, "p": px + 1.0, "s": 9000, "x": "P", "c": [" "]}]  # fmt: skip
        return out

    def corporate_actions(self, symbol, start, end):
        rows = [{"symbol": symbol, "ex_date": d, "rate": r} for d, r in self.divs.items()
                if str(start) <= d <= str(end)]  # fmt: skip
        return {"cash_dividends": rows}

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


YAHOO_DIVS = {"2024-11-27": 0.5}  # what the fake Yahoo reports; tests edit a copy through monkeypatch


def _yahoo_div_json(divs: dict) -> str:
    ev = {str(int(ts_(f"{d} 09:30").tz_localize(NY).timestamp())): {"amount": a,
          "date": int(ts_(f"{d} 09:30").tz_localize(NY).timestamp())} for d, a in divs.items()}  # fmt: skip
    return json.dumps({"chart": {"error": None, "result": [{"events": {"dividends": ev}}]}})


@pytest.fixture
def root(tmp_path, monkeypatch):
    monkeypatch.setattr(exo, "http_get", lambda url, params=None: _yahoo_div_json(YAHOO_DIVS))
    monkeypatch.setattr(dividends, "OVERRIDES_PATH", tmp_path / "no_overrides.csv")
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
    meta = json.loads((vintage_dir("t", root) / "sip/raw/5Min/SPY.json").read_text())
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


# ── 1DayAuction: listing-exchange crosses ───────────────────────────────────


def test_auction_picks_listing_exchange_cross(root):
    src = FakeSource()
    a = _load(root, "1DayAuction", "2024-11-25", "2024-12-03", src, adjustment="raw")
    assert len(a) == 5 and pd.Timestamp("2024-11-28", tz=NY) not in a.index  # holiday
    for d in a.index:
        assert a.loc[d, "open"] == FakeSource.cross(
            d.date(), "open"
        )  # not the decoy, not the unflagged trade
        assert a.loc[d, "close"] == FakeSource.cross(d.date(), "close")
    assert (a["open_size"] == 5000).all() and (a["close_nprice"] == 1).all() and (a["close_wide"] == 0).all()
    # the half-day's close cross is searched at 13:00, not 16:00
    assert a.loc[pd.Timestamp("2024-11-29", tz=NY), "close"] == FakeSource.cross("2024-11-29", "close")
    with pytest.raises(ValueError, match="'all'"):
        _load(root, "1DayAuction", "2024-11-25", "2024-12-03", src, adjustment="split")


def test_auction_wide_window_and_missing_cross(root, capsys, cal):
    src = FakeSource()
    src.delay["2024-11-26 open"] = pd.Timedelta(seconds=120)  # a delayed opening auction
    src.missing.add("2024-11-27 close")
    a = _load(root, "1DayAuction", "2024-11-25", "2024-12-03", src, adjustment="raw")
    d26, d27 = pd.Timestamp("2024-11-26", tz=NY), pd.Timestamp("2024-11-27", tz=NY)
    assert a.loc[d26, "open"] == FakeSource.cross("2024-11-26", "open") and a.loc[d26, "open_wide"] == 1
    assert np.isnan(a.loc[d27, "close"]) and a.loc[d27, "close_nprice"] == 0  # never filled
    assert "no close cross in 1 session(s): ['2024-11-27']" in capsys.readouterr().out
    rep = quality_report(a, cal, "1DayAuction")
    assert (
        rep["no_close_cross"] == 1
        and rep["no_close_cross_dates"] == "2024-11-27"
        and rep["open_wide_window"] == 1
    )


# ── Dividends: reconciliation, factors, adjusted auction prints ─────────────


def _divs(d: dict) -> pd.DataFrame:
    return dividends._frame(d.keys(), d.values())


def test_reconcile_tolerance_mismatch_and_overrides(tmp_path):
    no_ov = dividends.read_overrides(tmp_path / "none.csv")
    ok = dividends.reconcile("SPY", _divs({"2024-03-15": 1.5952}), _divs({"2024-03-15": 1.595}), no_ov)
    assert ok["amount"].iloc[0] == 1.5952 and ok["resolution"].iloc[0] == 0  # Yahoo's 3-decimal rounding
    alp, yah = (
        _divs({"2016-03-18": 1.05, "2018-06-15": 1.2}),
        _divs({"2016-03-18": 1.05, "2018-06-15": 1.246}),
    )
    with pytest.raises(dividends.DividendMismatchError, match="2018-06-15  alpaca 1.2  yahoo 1.246"):
        dividends.reconcile("SPY", alp, yah, no_ov)
    with pytest.raises(dividends.DividendMismatchError, match="2016-03-18  alpaca nan"):
        dividends.reconcile("SPY", _divs({}), _divs({"2016-03-18": 1.05}), no_ov)
    path = tmp_path / "ov.csv"
    path.write_text('symbol,ex_date,amount,note\nSPY,2018-06-15,1.24557,"issuer page, checked"\n')
    t = dividends.reconcile("SPY", alp, yah, dividends.read_overrides(path))
    assert t.loc["2018-06-15", "amount"] == 1.24557 and t["resolution"].sum() == 1
    with pytest.raises(dividends.DividendMismatchError, match="match no disagreement"):
        dividends.reconcile("SPY", yah, yah, dividends.read_overrides(path))  # a stale override is refused
    path.write_text("symbol,ex_date,amount,note\nSPY,2018-06-15,1.24557,\n")
    with pytest.raises(ValueError, match="note"):
        dividends.read_overrides(path)
    with pytest.raises(dividends.DividendMismatchError, match="unhandled"):
        dividends.parse_alpaca({"cash_dividends": [], "forward_splits": [{"symbol": "SPY"}]})


def test_adjustment_factor_by_hand():
    idx = pd.DatetimeIndex(["2024-01-02", "2024-01-03", "2024-01-04", "2024-01-05"], tz=NY)
    close = pd.Series([100.0, 102.0, 101.0, 103.0], index=idx)
    f = dividends.adjustment_factor(close, _divs({"2024-01-04": 1.0}))
    np.testing.assert_allclose(f, [101 / 102, 101 / 102, 1, 1], rtol=0, atol=1e-15)
    adj = close * f
    assert adj.iloc[2] / adj.iloc[1] - 1 == pytest.approx((101 + 1) / 102 - 1, abs=1e-14)  # total return
    with pytest.raises(ValueError, match="not sessions"):
        dividends.adjustment_factor(close, _divs({"2024-01-06": 1.0}))
    with pytest.raises(ValueError, match="missing close"):
        dividends.adjustment_factor(close.where(close.index != idx[2]), _divs({"2024-01-04": 1.0}))


def test_auction_adjusted_by_reconciled_dividends(root):
    src = FakeSource()
    a = _load(root, "1DayAuction", "2024-11-18", "2024-12-10", src)
    ex = pd.Timestamp("2024-11-27", tz=NY)
    assert a.loc[ex, "dividend"] == 0.5 and (a["dividend"] > 0).sum() == 1
    r = a["close"].pct_change()
    tr = (a["close_raw"] + a["dividend"]) / a["close_raw"].shift() - 1
    np.testing.assert_allclose(r.iloc[1:], tr.iloc[1:], rtol=0, atol=1e-13)
    assert (a.loc[a.index >= ex, "factor"] == 1).all() and (a.loc[a.index < ex, "factor"] < 1).all()
    part = _load(root, "1DayAuction", "2024-11-20", "2024-11-27", src)
    pd.testing.assert_frame_equal(part, a.loc[part.index])  # a slice gets the full frame's factors
    assert json.loads((vintage_dir("t", root) / "dividends/SPY.json").read_text())["overrides_used"] == 0


def test_auction_refuses_unreconciled_dividends(root, monkeypatch):
    monkeypatch.setitem(YAHOO_DIVS, "2024-11-27", 0.52)
    with pytest.raises(dividends.DividendMismatchError, match="2024-11-27"):
        _load(root, "1DayAuction", "2024-11-18", "2024-12-10")


# ── diag: Yahoo parsing, splice, cost-model refusal ─────────────────────────


def _yahoo_json(days, close, adj, volume):
    ts = [int(ts_(ts_(d).tz_localize(NY) + pd.Timedelta(hours=9, minutes=30)).timestamp()) for d in days]
    q = {"open": list(close * 0.999), "high": list(close * 1.01), "low": list(close * 0.99), "close": list(close),
         "volume": list(volume)}  # fmt: skip
    return json.dumps({"chart": {"error": None, "result": [{"timestamp": ts, "indicators": {"quote": [q],
                       "adjclose": [{"adjclose": list(adj)}]}}]}})  # fmt: skip


EX_DATES = ["2015-03-20", "2015-06-19", "2015-09-18", "2015-12-18", "2016-03-18", "2016-06-17", "2016-09-16",
            "2016-12-16", "2017-03-17", "2017-06-16", "2017-09-15", "2017-12-15", "2018-03-16", "2018-06-15"]  # fmt: skip


def _diag_inputs(auction_close=lambda close, factor: close):
    """Yahoo raw/adjclose 2015-2018 with quarterly 0.5 % dividends, and a 1DayAuction frame from 2016 whose raw close
    is `auction_close(yahoo close, yahoo factor)` plus a cent of noise, adjusted with the same dividends."""
    days = pd.bdate_range("2015-01-02", "2018-12-31")
    rng = np.random.default_rng(3)
    close = np.round(200 * np.exp(np.cumsum(rng.normal(0, 0.01, len(days)))), 2)
    step = np.ones(len(days))
    for ex in EX_DATES:
        step[days.get_loc(pd.Timestamp(ex))] = 0.995
    yf = np.append(np.cumprod(step[::-1])[::-1][1:], 1.0)
    raw = secondary.parse_yahoo_chart(_yahoo_json(days, close, close * yf, np.full(len(days), 1e8)))
    dev = raw.index >= pd.Timestamp(config.DEV_START, tz=NY)
    craw = pd.Series(auction_close(close, yf), index=raw.index)[dev] + rng.choice([-0.01, 0, 0.01], dev.sum())
    divs = _divs(
        {ex: 0.005 * float(raw["close"].shift().loc[pd.Timestamp(ex, tz=NY)]) for ex in EX_DATES[4:]}
    )
    factor = dividends.adjustment_factor(craw, divs)
    auction = pd.DataFrame({"open": craw * factor, "close": craw * factor, "open_raw": craw, "close_raw": craw,
                            "dividend": divs["amount"].reindex(craw.index.tz_localize(None)).fillna(0).to_numpy(),
                            "factor": factor})  # fmt: skip
    return raw, auction


def test_splice_raw_vs_raw_accepted_and_continuous():
    raw, auction = _diag_inputs()
    out, rep = secondary.splice(raw, auction)
    assert abs(rep["raw_ratio_median"] - 1) < 1e-4 and rep["adjusted_close_corr"] > 0.9999
    assert out.index[-1] < pd.Timestamp(config.DEV_START, tz=NY) and out.attrs["source"] == "diag"
    assert out["is_diag"].all()
    last, first = out.index[-1], auction.index[0]
    # total return across the boundary equals Yahoo's raw return (no dividend on 2016-01-04)
    k = rep["raw_ratio_median"]
    assert auction.loc[first, "close"] / out.loc[last, "close"] == pytest.approx(
        auction.loc[first, "close_raw"] / (raw.loc[last, "close"] * k), rel=1e-12)  # fmt: skip
    # a 2015 ex-date is a total-return step inside diag
    e = pd.Timestamp("2015-06-19", tz=NY)
    prev = out.index[out.index.get_loc(e) - 1]
    assert out.loc[e, "close"] / out.loc[prev, "close"] == pytest.approx(
        raw.loc[e, "close"] / (0.995 * raw.loc[prev, "close"])
    )
    assert out.loc[out.index[0], "close_split"] == raw.loc[raw.index[0], "close"]


def test_splice_refuses_drifting_or_rescaled_source():
    with pytest.raises(secondary.SpliceError, match="not constant"):  # a dividend-adjusted 'raw' close drifts
        secondary.splice(*_diag_inputs(lambda close, yf: close * yf))
    with pytest.raises(secondary.SpliceError, match="levels differ"):  # a split or a re-scaling
        secondary.splice(*_diag_inputs(lambda close, yf: close * 1.01))


def test_cost_model_refuses_diag():
    raw, auction = _diag_inputs()
    out, _ = secondary.splice(raw, auction)
    with pytest.raises(secondary.DiagDataError):
        check_costable(out)
    stripped = out.copy()
    stripped.attrs = {}
    with pytest.raises(secondary.DiagDataError):  # the row flag survives a lost attrs
        check_costable(stripped)
    check_costable(auction)  # Alpaca prints pass


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


def test_intraday_family_shares_the_auction_dividend_factor(root):
    """5Min / 1DayRTH / snap1545 are cached raw and scaled by the session's 1DayAuction factor (review U1 SEVERE)."""
    src = FakeSource()
    raw = _load(root, "5Min", "2024-11-18", "2024-12-10", src, adjustment="raw")
    adj = _load(root, "5Min", "2024-11-18", "2024-12-10", src)
    auc = _load(root, "1DayAuction", "2024-11-18", "2024-12-10", src)
    f = auc["factor"].reindex(midnight(raw.index)).to_numpy()
    np.testing.assert_allclose(adj["close"], raw["close"] * f, rtol=1e-15)
    assert (adj["volume"] == raw["volume"]).all()
    before = pd.Timestamp("2024-11-26", tz=NY)
    assert (
        auc.loc[before, "factor"] < 1
        and adj.loc["2024-11-26", "vwap"].iloc[0] < raw.loc["2024-11-26", "vwap"].iloc[0]
    )
    rth_raw = _load(root, "1DayRTH", "2024-11-18", "2024-12-10", src, adjustment="raw")
    rth = _load(root, "1DayRTH", "2024-11-18", "2024-12-10", src)
    assert rth.loc[before, "close"] == pytest.approx(
        rth_raw.loc[before, "close"] * auc.loc[before, "factor"], rel=1e-15
    )
    snap = _load(root, "snap1545", "2024-11-18", "2024-12-10", src)
    assert (
        snap.loc[before, "vwap"]
        < _load(root, "snap1545", "2024-11-18", "2024-12-10", src, adjustment="raw").loc[before, "vwap"]
    )
    with pytest.raises(ValueError, match="reconciled"):
        _load(root, "5Min", "2024-11-18", "2024-12-10", src, adjustment="split")
