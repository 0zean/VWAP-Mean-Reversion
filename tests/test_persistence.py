"""U7: persistence tables, the historical cost schedule, causal states, the edge monitor, the script's dry run."""

import hashlib
import importlib.util
import json

import numpy as np
import pandas as pd
import pytest

from vwap_mr.backtest.costs import CostModel
from vwap_mr.backtest.engine import reprice, run_diagnostic
from vwap_mr.config import ROOT
from vwap_mr.data.synthetic import daily_market
from vwap_mr.experiments import persistence as ps
from vwap_mr.experiments.diagnose import diag_market
from vwap_mr.experiments.looks import LOOKS_PATH
from vwap_mr.live.monitor import EdgeMonitor
from vwap_mr.stats.breaks import local_level_track
from vwap_mr.strategies.vwap_mr import r0

NY = "America/New_York"


def diag_frame(start="1999-01-04", end="2003-12-31"):
    s = daily_market("SPY", start, end, 5).sessions
    f = pd.DataFrame({"open": s["open"], "close": s["close"], "close_split": s["close_raw"], "volume": s["volume"],
                      "is_diag": True})  # fmt: skip
    f.attrs["source"] = "diag"
    return f


def test_historical_schedule_by_hand():
    idx = pd.DatetimeIndex(["2001-04-06", "2001-04-09", "2010-01-04"]).tz_localize(NY)
    rt = ps.historical_rt_bp(pd.Series([50.0, 50.0, 200.0], index=idx))
    np.testing.assert_allclose(rt, [12.5, 2.0, 1.0])  # 1/16 / 50; 0.01 / 50; 0.01 / 200 = 0.5 -> floor 1.0


def test_at_cost_equals_the_engine_at_a_constant_cost():
    m = diag_market(diag_frame())
    res = run_diagnostic(m, r0(), fill="next_open", costs=CostModel(0.0, fees=False), cash_yield=0.02)
    lg = res.ledger
    for bp in (1.0, 4.7):
        want = reprice(res, bp).ledger["net"]
        got = ps.at_cost(lg["net"], lg["traded"], pd.Series(bp, index=lg.index))
        np.testing.assert_allclose(got, want, rtol=0, atol=1e-15)


def test_period_tables():
    idx = pd.bdate_range("2016-01-04", "2020-12-31").tz_localize(NY)
    x = pd.Series(np.random.default_rng(0).normal(1e-4, 0.01, len(idx)), index=idx)
    years = ps.by_year(x)
    assert [r["period"] for r in years] == ["2016", "2017", "2018", "2019", "2020"]
    assert sum(r["n"] for r in years) == len(x)
    roll = ps.rolling_years(x)
    assert [r["period"] for r in roll] == ["2016–2018", "2017–2019", "2018–2020"]
    assert roll[0]["mean_bp_day"] == pytest.approx(x["2016":"2018"].mean() * 1e4)
    lo, hi = roll[0]["ci90"]
    assert lo < roll[0]["mean_bp_day"] < hi
    short = ps.period_row(x.iloc[:30], "short")
    assert np.isnan(short["ci90"][0]) and np.isfinite(short["t_nw"])


def test_trailing_quantile_is_causal_and_by_hand():
    s = pd.Series([3.0, 1.0, 2.0, 5.0, 4.0])
    np.testing.assert_allclose(ps.trailing_quantile(s, 3), [np.nan, np.nan, 2 / 3, 1.0, 2 / 3])
    rng = np.random.default_rng(1)
    a = pd.Series(rng.normal(size=600))
    b = a.copy()
    b.iloc[400:] = rng.normal(size=200) * 10
    np.testing.assert_array_equal(ps.trailing_quantile(a, 100)[:400], ps.trailing_quantile(b, 100)[:400])
    np.testing.assert_array_equal(ps.bins(pd.Series([0.2, 0.21, 1.0, np.nan]), 5), [1, 2, 5, np.nan])


def test_state_is_aligned_to_the_previous_decision():
    idx = pd.bdate_range("2016-01-04", periods=5).tz_localize(NY)
    st = pd.Series([10.0, 20, 30, 40, 50], index=idx)
    np.testing.assert_array_equal(ps.known_at_previous_decision(st, idx[1:]), [10, 20, 30, 40])


def test_cond_table_counts_missing_states():
    idx = pd.bdate_range("2016-01-04", periods=200).tz_localize(NY)
    x = pd.Series(np.arange(200) * 1e-5, index=idx)
    key = pd.Series(np.where(np.arange(200) < 100, 1.0, 2.0), index=idx)
    key.iloc[:10] = np.nan
    rows = ps.cond_table(x, key, {1.0: "low", 2.0: "high"})
    assert [r["period"] for r in rows] == ["low", "high", "no state (warm-up)"]
    assert [r["n"] for r in rows] == [90, 100, 10]


def test_coherence_by_year():
    idx = pd.bdate_range("2016-01-04", "2017-12-29").tz_localize(NY)
    y16 = idx.year == 2016  # ty: ignore[unresolved-attribute]
    s = {"SPY": pd.Series(np.where(y16, 1e-4, -1e-4), index=idx), "QQQ": pd.Series(1e-4, index=idx),
         "IWM": pd.Series(np.where(y16, -1e-4, -2e-4), index=idx)}  # fmt: skip
    rows = ps.coherence_by_year(s)
    assert [r["same_sign"] for r in rows] == [2, 2]


def test_edge_monitor_equals_the_causal_track():
    y = np.random.default_rng(2).normal(1e-4, 0.01, 330)
    tr = local_level_track(y, min_obs=60, refit_every=21)
    mon = EdgeMonitor(min_obs=60, refit_every=21)
    out = [mon.update(v) for v in y]
    assert all(o is None for o in out[:60])
    live = [o for o in out[60:] if o is not None]
    assert len(live) == len(y) - 60
    np.testing.assert_allclose([o["p_pos"] for o in live], tr["p_pos"][60:], rtol=0, atol=1e-12)
    np.testing.assert_allclose([o["mu"] for o in live], tr["mu"][60:], rtol=0, atol=1e-15)
    nxt = EdgeMonitor(y[:200], min_obs=60, refit_every=21).update(y[200])
    assert nxt is not None and nxt["p_pos"] == pytest.approx(tr["p_pos"][200], abs=1e-12)
    with pytest.raises(ValueError):
        mon.update(float("nan"))


def test_monitor_ignores_the_future():
    y = np.random.default_rng(3).normal(0, 0.01, 200)
    a = EdgeMonitor(min_obs=50).update
    out_a = [a(v) for v in y]
    z = y.copy()
    z[150:] += 0.05
    b = EdgeMonitor(min_obs=50).update
    out_b = [b(v) for v in z]
    assert all(o1 == o2 for o1, o2 in zip(out_a[:150], out_b[:150], strict=True))


def test_script_dry_run_writes_only_to_scratch(tmp_path):
    spec = importlib.util.spec_from_file_location("u07", ROOT / "scripts" / "u07_persistence.py")
    assert spec is not None and spec.loader is not None
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    before = hashlib.md5(LOOKS_PATH.read_bytes()).hexdigest()
    assert mod.main(["--synthetic", "--scratch", str(tmp_path)]) == 0
    assert hashlib.md5(LOOKS_PATH.read_bytes()).hexdigest() == before
    md = (tmp_path / "U07_persistence.md").read_text(encoding="utf-8")
    for head in (
        "## 1. Edge by period",
        "## 2. Structural breaks",
        "## 3. Kalman",
        "## 4. Conditional",
        "## 5.",
    ):
        assert head in md
    rows = [json.loads(x) for x in (tmp_path / "looks.jsonl").read_text().splitlines()]
    new = [r for r in rows if r["label"].startswith("U7 ")]
    assert sum(r["n"] for r in new if r["window"] == "dev") == 12
    assert sum(r["n"] for r in new if r["window"] == "diag") == 2
