"""U5: the VWAP-MR cell and R₀, the diagnostics of `experiments/diagnose.py`, and the look gate."""

import json
from dataclasses import dataclass

import numpy as np
import pandas as pd
import pytest
from causality import assert_causal
from test_backtest import synth

from vwap_mr.backtest.costs import ZERO
from vwap_mr.backtest.engine import run, run_diagnostic
from vwap_mr.config import DecisionTime
from vwap_mr.data.secondary import DiagDataError
from vwap_mr.experiments import diagnose as dg
from vwap_mr.experiments.looks import looked, looks_total, record_look
from vwap_mr.features.deviation import trading_deviation
from vwap_mr.strategies.base import target_series
from vwap_mr.strategies.vwap_mr import R0_B, R0_N, VWAP_R, VwapCell, cell_target, r0, r0_cells

NY = "America/New_York"


def diag_frame(n: int = 300, seed: int = 0) -> pd.DataFrame:
    m = synth(n, seed).sessions
    f = pd.DataFrame({"open": m["open"], "close": m["close"], "close_split": m["close_raw"], "volume": m["volume"],
                      "is_diag": True})  # fmt: skip
    f.attrs["source"] = "diag"
    return f


# ── cell and region ─────────────────────────────────────────────────────────


def test_cell_thresholds_are_strict_and_nan_is_flat():
    np.testing.assert_array_equal(cell_target([0.002, -0.002, 0.001, -0.001, 0.0, np.nan], 0.001),
                                  [-1, 1, 0, 0, 0, 0])  # fmt: skip
    np.testing.assert_array_equal(cell_target([1e-9, -1e-9, 0.0], 0.0), [-1, 1, 0])


def test_r0_is_the_registered_grid():
    cells = r0_cells()
    assert [(c.n, c.b) for c in cells] == [(n, b) for n in R0_N for b in R0_B]
    assert len(cells) == 16 and len({c.name for c in cells}) == 16
    assert (VWAP_R.n, VWAP_R.b) == (3, 0.001)
    with pytest.raises(ValueError):
        VwapCell(3, 0.001, decision=DecisionTime.T1545, fill="moc")


@pytest.mark.parametrize("strategy", [VWAP_R, VwapCell(10, 0.005), r0()], ids=lambda s: s.name)
def test_vectorized_targets_equal_the_view_loop(strategy):
    m = synth(120, 3)
    np.testing.assert_allclose(
        target_series(strategy, m), target_series(strategy, m, vectorized=False), atol=0
    )


def test_cell_matches_the_deviation_by_hand():
    m = synth(60, 1)
    s = m.sessions
    d = trading_deviation(s["close"], s["volume"], 3)
    t = VWAP_R.targets(m)
    assert (t.iloc[:2] == 0).all()
    np.testing.assert_array_equal(
        t.iloc[2:], np.where(d.iloc[2:] > 0.001, -1, np.where(d.iloc[2:] < -0.001, 1, 0))
    )


@pytest.mark.parametrize("strategy", [VWAP_R, r0()], ids=lambda s: s.name)
def test_cell_and_region_are_causal(strategy):
    m = synth(250, 4)
    assert_causal(lambda mk: target_series(strategy, mk), m, DecisionTime.CLOSE)
    assert_causal(lambda mk: run(mk, strategy, costs=ZERO, cash_yield=0.0).targets, m, DecisionTime.CLOSE)


@dataclass(frozen=True)
class PeekCell(VwapCell):
    """Mutant: the deviation of the next session (a one-bar peek)."""

    def targets(self, market):
        s = market.sessions
        d = trading_deviation(s["close"], s["volume"], self.n).shift(-1)
        return pd.Series(cell_target(d, self.b), index=s.index)


def test_harness_catches_a_one_bar_peek_in_the_cell():
    with pytest.raises(AssertionError):
        assert_causal(lambda mk: target_series(PeekCell(3, 0.001), mk), synth(250, 4), DecisionTime.CLOSE)


# ── diag market and the VWAP.R emulation ────────────────────────────────────


def test_diag_market_is_refused_by_run_and_needs_a_diag_frame():
    f = diag_frame()
    m = dg.diag_market(f)
    assert m.source == "diag"
    with pytest.raises(DiagDataError):
        run(m, VWAP_R, costs=ZERO, cash_yield=0.0)
    g = f.copy()
    g.attrs = {}
    with pytest.raises(ValueError, match="load_long"):
        dg.diag_market(g)


def test_close_split_basis_keeps_the_dividend_drop():
    f = diag_frame(130)
    tr, pr = dg.diag_market(f), dg.diag_market(f, price="close_split")
    r_tr = tr.sessions["close"].pct_change()
    r_pr = pr.sessions["close"].pct_change()
    ex = synth(130).sessions["dividend"] > 0
    assert (r_tr[ex] > r_pr[ex] + 1e-4).all()  # the price return falls by the dividend on the ex-date
    np.testing.assert_allclose(r_tr[~ex].iloc[1:], r_pr[~ex].iloc[1:], rtol=0, atol=1e-14)
    np.testing.assert_allclose(pr.sessions["open"] / pr.sessions["close"], f["open"] / f["close"], rtol=1e-12)


def test_vwap_r_streams_reproduce_the_script_by_hand():
    """Ten sessions computed the way VWAP.R does (signal NA → flat; trade × lag(ret, −1); na.omit of the cbind)."""
    close = np.array([100, 101, 99, 99.5, 102, 101, 100.5, 103, 102, 104.0])
    vol = np.array([5, 4, 6, 5, 7, 3, 4, 6, 5, 4.0])
    idx = pd.bdate_range("2007-01-03", periods=10).tz_localize(NY)
    f = pd.DataFrame({"open": close, "close": close, "close_split": close, "volume": vol}, index=idx)
    f.attrs["source"] = "diag"
    m = dg.diag_market(f, price="close_split")
    vwap = np.array([np.nan, np.nan] + [(close[i - 2 : i + 1] @ vol[i - 2 : i + 1]) / vol[i - 2 : i + 1].sum()
                                        for i in range(2, 10)])  # fmt: skip
    sig = np.where(np.isnan(vwap), 1.0, close / vwap)
    trade = np.where(sig < 0.999, 1.0, np.where(sig > 1.001, -1.0, 0.0))
    ret = np.r_[np.nan, close[1:] / close[:-1] - 1]
    strat = trade[:-1] * ret[1:]  # row t = trade_t · ret_{t+1}, t = 0 … 8
    st = dg.vwap_r_streams(m, VWAP_R.targets(m))
    assert list(st.index) == list(idx[1:-1])
    np.testing.assert_allclose(st["strategy"], strat[1:], rtol=1e-12)
    np.testing.assert_allclose(st["index"], ret[1:-1], rtol=1e-12)
    eng = run_diagnostic(m, VWAP_R, fill="same_close", costs=ZERO, cash_yield=0.0)
    np.testing.assert_allclose(eng.ledger["gross"].shift(-1).loc[st.index], st["strategy"], rtol=1e-12)


def test_r_sharpe_is_performanceanalytics_geometric():
    r = np.array([0.01, -0.02, 0.015, 0.003, -0.004])
    geo = np.prod(1 + r) ** (252 / 5) - 1
    assert dg.r_sharpe(r) == pytest.approx(geo / (np.std(r, ddof=1) * np.sqrt(252)), rel=1e-12)


def test_leg_contributions_decompose_next_open_gross():
    m = synth(200, 2)
    res = run(m, VWAP_R, costs=ZERO, cash_yield=0.0)
    c = dg.leg_contributions(res)
    g = res.ledger["gross"].iloc[2:]
    lg = res.legs.iloc[2:]
    cross = (lg["r_on"] * lg["r_id"]).abs()
    assert ((g - c["on_next_open"] - c["intraday"]).abs() <= cross + 1e-15).all()
    np.testing.assert_allclose(c["on_same_close"], res.targets.shift(1).iloc[2:] * lg["r_on"], rtol=0)
    with pytest.raises(ValueError, match="next_open"):
        dg.leg_contributions(run_diagnostic(m, VWAP_R, fill="same_close", costs=ZERO, cash_yield=0.0))


# ── reversal statistics ─────────────────────────────────────────────────────


def test_autocorr_and_robust_se_on_iid_and_ar1():
    rng = np.random.default_rng(0)
    x = rng.standard_normal(20_000)
    rho, se = dg.autocorr(x, 1)
    assert se == pytest.approx(1 / np.sqrt(len(x)), rel=0.05)
    assert abs(rho) < 4 * se
    e = rng.standard_normal(20_000)
    y = np.empty_like(e)
    y[0] = e[0]
    for t in range(1, len(e)):
        y[t] = -0.2 * y[t - 1] + e[t]
    assert dg.autocorr(y, 1)[0] == pytest.approx(-0.2, abs=0.02)
    assert dg.autocorr(y, 2)[0] == pytest.approx(0.04, abs=0.02)
    # GARCH-like scale changes widen the robust se above 1/√n
    z = x * np.repeat(rng.choice([0.5, 3.0], 200), 100)
    assert dg.autocorr(z, 1)[1] > 1.3 / np.sqrt(len(z))


def test_variance_ratio_is_the_autocorrelation_form():
    rng = np.random.default_rng(1)
    x = rng.standard_normal(5000)
    vr2, z2 = dg.variance_ratio(x, 2)
    rho, se = dg.autocorr(x, 1)
    assert vr2 == pytest.approx(1 + rho, rel=1e-12)
    assert z2 == pytest.approx(rho / se, rel=1e-12)
    vr5, _ = dg.variance_ratio(x, 5)
    s5 = pd.Series(x).rolling(5).sum().dropna()
    assert vr5 == pytest.approx(s5.var() / (5 * x.var()), abs=0.03)  # overlapping-sum form, same limit


def test_cross_corr_reads_the_next_value():
    rng = np.random.default_rng(2)
    x = rng.standard_normal(10_000)
    y = np.r_[0.0, -0.3 * x[:-1]] + rng.standard_normal(10_000)
    c, se = dg.cross_corr(x, y)
    assert c == pytest.approx(-0.3 / np.sqrt(1.09), abs=0.02)
    assert abs(dg.cross_corr(y, x)[0]) < 4 * se


# ── the look gate ───────────────────────────────────────────────────────────


def test_looked_records_before_computing(tmp_path):
    path = tmp_path / "looks.jsonl"
    seen = []

    def compute():
        seen.append([json.loads(line)["label"] for line in path.read_text().splitlines()])
        return 42

    assert looked("t1", 3, compute, window="diag", path=path) == 42
    assert seen == [["t1"]]
    with pytest.raises(ValueError, match="window"):
        looked("t2", 1, compute, window="holdout", path=path)
    assert len(seen) == 1  # nothing was computed for the refused look


def test_looks_total_counts_dev_by_default(tmp_path):
    path = tmp_path / "looks.jsonl"
    path.write_text(json.dumps({"at": "x", "label": "seed", "n": 1, "note": "", "git_sha": "0" * 40}) + "\n")
    record_look("a", 5, path=path, window="dev")
    record_look("b", 7, path=path, window="diag")
    assert looks_total(path) == 6
    assert looks_total(path, window="diag") == 7
    assert looks_total(path, window=None) == 13
