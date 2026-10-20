"""Backtest engine, costs, account, metrics, strategy protocol and features (U3). No network."""

from dataclasses import dataclass, replace

import numpy as np
import pandas as pd
import pytest
from causality import assert_causal, assert_fill_after

from vwap_mr.backtest import account, engine
from vwap_mr.backtest.costs import ZERO, CostModel, fee_rates, measured, tbill_yield
from vwap_mr.backtest.engine import legs, reprice, run, run_diagnostic, simulate
from vwap_mr.backtest.market import Market
from vwap_mr.backtest.metrics import by_year, drawdown, summary
from vwap_mr.config import ROOT, DecisionTime
from vwap_mr.data.dividends import _frame, adjustment_factor
from vwap_mr.data.exo import Exo, available_at
from vwap_mr.data.secondary import DiagDataError
from vwap_mr.features.deviation import state_deviation, trading_deviation
from vwap_mr.features.vwap import rolling_vwap
from vwap_mr.stats.sharpe import psr
from vwap_mr.strategies.base import target_series, views
from vwap_mr.strategies.region import Region

NY = "America/New_York"
CLOSE, T1545 = DecisionTime.CLOSE, DecisionTime.T1545
FIXTURE = ROOT / "tests" / "fixtures" / "spy_2024q1_daily.parquet"


# ── Synthetic market ────────────────────────────────────────────────────────


def synth(n: int = 400, seed: int = 0, *, div_every: int = 63, flat_gap: bool = False) -> Market:
    """Raw prints with overnight gaps and quarterly dividends, the adjusted close from the U1 factor, a snapshot,
    one state column and VIX / DTB3 with their available_at rules."""
    rng = np.random.default_rng(seed)
    days = pd.bdate_range("2018-01-02", periods=n)
    div = np.where((np.arange(n) % div_every == div_every - 1), 0.45, 0.0) if div_every else np.zeros(n)
    o, c = np.empty(n), np.empty(n)
    prev = 100.0
    for i in range(n):
        o[i] = prev if flat_gap else prev * np.exp(rng.normal(0, 0.004)) - div[i]
        c[i] = o[i] * np.exp(rng.normal(0, 0.009))
        prev = c[i]
    idx = days.tz_localize(NY)
    raw = pd.Series(c, index=idx)
    divs = _frame(days[div > 0], div[div > 0])
    factor = adjustment_factor(raw, divs).to_numpy()
    vol = rng.uniform(5e7, 1.5e8, n)
    s = pd.DataFrame({"open": o * factor, "close": c * factor, "open_raw": o, "close_raw": c, "dividend": div,
                      "factor": factor, "volume": vol}, index=idx)  # fmt: skip
    snap = pd.DataFrame({"last": c * factor * np.exp(rng.normal(0, 0.001, n)), "volume": vol * 0.8,
                         "vwap": c * factor, "n_bars": 75.0}, index=idx)  # fmt: skip
    state = pd.DataFrame({"s": rng.normal(size=n)}, index=idx)
    vix = pd.DataFrame({"value": rng.uniform(10, 40, n)}, index=days)
    vix["available_at"] = available_at("VIX", days)
    rate_days = pd.bdate_range(end=days[-1], periods=n + 10)  # H.15 history starts before the first session
    dtb3 = pd.DataFrame({"value": rng.uniform(0.5, 5, n + 10)}, index=rate_days)
    dtb3["available_at"] = available_at("DTB3", rate_days)
    return Market(s, "synthetic", snapshot=snap, state=state, exo=Exo({"VIX": vix, "DTB3": dtb3}))


def fixture_market() -> Market:
    s = pd.read_parquet(FIXTURE)
    s.attrs["source"] = "alpaca"  # the fixture is load_bars output (1DayAuction adjusted + 1DayPrint volume)
    return Market(s, "alpaca")


# ── Strategies used by the tests ────────────────────────────────────────────


def _cell(d: float, b: float) -> float:
    return 0.0 if not np.isfinite(d) else (-1.0 if d > b else (1.0 if d < -b else 0.0))


@dataclass(frozen=True)
class VwapCell:
    """§8.1 cell: −1 / 0 / +1 on the trading deviation of the close from VWAP_n (close decision)."""

    n: int
    b: float
    decision: DecisionTime = CLOSE
    fill: str = "next_open"

    @property
    def name(self) -> str:
        return f"vwap_{self.n}_{self.b}"

    def target(self, view) -> float:
        s = view.sessions.iloc[-self.n :]
        return (
            _cell(trading_deviation(s["close"], s["volume"], self.n).iloc[-1], self.b)
            if len(s) >= self.n
            else 0.0
        )

    def targets(self, market):
        d = trading_deviation(market.sessions["close"], market.sessions["volume"], self.n)
        return d.map(lambda x: _cell(x, self.b))


@dataclass(frozen=True)
class SnapCell:
    """t1545 cell: the 15:45 last trade against VWAP_n of the previous n sessions."""

    n: int = 3
    b: float = 0.001
    leak: bool = False  # mutant: reads session t's own close
    decision: DecisionTime = T1545
    fill: str = "moc"
    name: str = "snap"

    def target(self, view) -> float:
        s = view.sessions.iloc[-self.n :]
        if len(s) < self.n or view.snapshot is None:
            return 0.0
        anchor = rolling_vwap(s["close"], s["volume"], self.n).iloc[-1]
        return _cell(view.snapshot["last"] / anchor - 1, self.b)

    def targets(self, market):
        s = market.sessions
        anchor = rolling_vwap(s["close"], s["volume"], self.n).shift(1)
        px = s["close"] if self.leak else market.snapshot["last"].reindex(s.index)
        return (px / anchor - 1).map(lambda x: _cell(x, self.b))


@dataclass(frozen=True)
class ExoState:
    """Uses the view's exo and state: long if VIX as of the decision > 20 and the state is positive, else short."""

    decision: DecisionTime = CLOSE
    fill: str = "next_open"
    name: str = "exo_state"

    def target(self, view) -> float:
        return 1.0 if view.exo.asof("VIX") > 20 and view.state["s"].iloc[-1] > 0 else -1.0

    def targets(self, market):
        vix = market.exo.asof("VIX", market.decision_instants(self.decision))
        return pd.Series(
            np.where((vix > 20) & (market.state["s"].to_numpy() > 0), 1.0, -1.0), index=market.index
        )


@dataclass(frozen=True)
class Fixed:
    """A given target array (decided per session)."""

    f: tuple
    decision: DecisionTime = CLOSE
    fill: str = "next_open"
    name: str = "fixed"

    def target(self, view) -> float:
        return float(self.f[len(view.sessions) - 1 + (self.decision == T1545)])

    def targets(self, market):
        return pd.Series(self.f, index=market.index, dtype=float)


@dataclass(frozen=True)
class PeekNext:
    """Mutant: a close strategy whose vectorized path reads close_{t+1}."""

    decision: DecisionTime = CLOSE
    fill: str = "next_open"
    name: str = "peek"

    def target(self, view) -> float:
        return 0.0

    def targets(self, market):
        c = market.sessions["close"]
        return np.sign(c.shift(-1) - c).fillna(0.0)


R0 = Region("R0", tuple(VwapCell(n, b) for n in (2, 3, 5, 10) for b in (0.0, 0.001, 0.0025, 0.005)))


def _const(v: float, decision=CLOSE, n: int = 400):
    return Fixed(tuple([v] * n), decision=decision, fill="next_open" if decision == CLOSE else "moc")


# ── Invariants (PLAN.md §7 U3 done-when) ────────────────────────────────────


def _bh_equity(m: Market, fill: str, e0: float) -> np.ndarray:
    """Independent share ledger: buy at the first fill, reinvest each dividend at the fill print of its ex-date."""
    o, c, d = (m.sessions[k].to_numpy() for k in ("open_raw", "close_raw", "dividend"))
    eq = np.empty(len(c))
    eq[0] = e0
    shares = e0 / c[0] if fill == "moc" else e0 / o[1]
    for i in range(1, len(c)):
        if d[i]:
            shares *= (o[i] + d[i]) / o[i] if fill == "next_open" else (c[i] + d[i]) / c[i]
        eq[i] = shares * c[i]
    return eq


@pytest.mark.parametrize("decision", [CLOSE, T1545])
def test_always_long_is_buy_and_hold_at_zero_cost(decision):
    m = synth()
    r = run(m, _const(1.0, decision), costs=ZERO, cash_yield=0.0)
    np.testing.assert_allclose(r.ledger["equity"], _bh_equity(m, r.fill, 30_000.0), rtol=1e-12)
    if decision == T1545:  # moc buy-and-hold is the adjusted (total-return) close path exactly
        adj = m.sessions["close"].to_numpy()
        np.testing.assert_allclose(r.ledger["equity"] / 30_000.0, adj / adj[0], rtol=1e-12)


def test_always_long_on_fixture_is_the_adjusted_close():
    m = fixture_market()  # 2024Q1 has SPY's 2024-03-15 ex-date
    assert (m.sessions["dividend"] > 0).sum() == 1
    lg, _ = legs(m, "moc")
    eq = simulate(lg, pd.Series(1.0, index=m.index), "moc", ZERO, pd.Series(0.0, index=m.index), 1.0)[
        "equity"
    ]
    adj = m.sessions["close"].to_numpy()
    np.testing.assert_allclose(eq, adj / adj[0], rtol=1e-12)
    np.testing.assert_allclose(simulate(legs(m, "next_open")[0], pd.Series(1.0, index=m.index), "next_open", ZERO,
                                        pd.Series(0.0, index=m.index), 1.0)["equity"],
                               _bh_equity(m, "next_open", 1.0), rtol=1e-12)  # fmt: skip


def test_sign_flip_negates_gross():
    m = synth(seed=3)
    rng = np.random.default_rng(1)
    f = rng.choice([-1.0, -0.5, 0.0, 0.5, 1.0], size=len(m.index))
    y = pd.Series(0.02, index=m.index)
    for fill in ("moc", "next_open"):
        lg, _ = legs(m, fill)
        g = simulate(lg, pd.Series(f, index=m.index), fill, CostModel(), y, 3e4)["gross"].to_numpy()
        h = simulate(lg, pd.Series(-f, index=m.index), fill, CostModel(), y, 3e4)["gross"].to_numpy()
        if fill == "moc":
            np.testing.assert_array_equal(g, -h)
        else:  # (1 + a r_on)(1 + b r_id) − 1 + (1 − a r_on)(1 − b r_id) − 1 = 2ab·r_on·r_id
            a, b = np.r_[0, 0, f[:-2]], np.r_[0, f[:-1]]
            cross = lg["r_on"].to_numpy() * lg["r_id"].to_numpy()
            np.testing.assert_allclose(g + h, 2 * a * b * cross, rtol=0, atol=1e-15)
            assert np.abs(g + h).sum() <= 2 * np.abs(cross).sum()


def test_cost_booked_matches_independent_sum_to_the_cent():
    n = 1200
    m = synth(n, seed=5)
    rng = np.random.default_rng(7)
    f = np.round(rng.uniform(-1, 1, n) * 16) / 16
    f[rng.random(n) < 0.15] = np.nan
    f = pd.Series(f).ffill().fillna(0.0).to_numpy()
    assert (np.diff(f) != 0).sum() >= 1000
    costs = CostModel(round_trip_bp=2.3, borrow_rate=0.004)
    y = tbill_yield(m)
    for fill, decision in (("next_open", CLOSE), ("moc", T1545)):
        r = run(m, Fixed(tuple(f), decision=decision, fill=fill), costs=costs, cash_yield=y)
        lg, legs_ = r.ledger, r.legs
        e_prev = np.r_[30_000.0, lg["equity"].to_numpy()[:-1]]
        a, b = lg["exp_overnight"].to_numpy(), lg["exp_close"].to_numpy()
        move = legs_["r_on"].to_numpy() if fill == "next_open" else legs_["r_cc"].to_numpy()
        at_fill = e_prev * (1 + a * move) + lg["interest"].to_numpy() - lg["borrow"].to_numpy()
        px = legs_["open_px" if fill == "next_open" else "close_px"].to_numpy()
        fees = fee_rates(m.index)
        sold = np.maximum(a - b, 0) * at_fill
        expected = (np.abs(b - a) * at_fill * costs.one_way(engine.FILL_KIND[fill]) + sold * fees["sec"].to_numpy()
                    + sold / px * fees["taf"].to_numpy())  # fmt: skip
        assert abs(lg["cost"].sum() - expected.sum()) < 0.005
        np.testing.assert_allclose(lg["cost"], expected, rtol=1e-12, atol=1e-9)


def test_long_to_short_flip_trades_twice():
    m = synth(10, div_every=0)
    f = (0, 1, 1, -1, -1, 0, 0, 0, 0, 0)
    r = run(m, Fixed(f), costs=CostModel(round_trip_bp=1.0, fees=False, borrow_rate=0.0), cash_yield=0.0)
    lg = r.ledger
    flip = lg.iloc[4]  # decided at session 3, filled at the open of session 4
    assert flip["traded"] == 2.0
    at_fill = lg["equity"].iloc[3] * (1 + 1.0 * r.legs["r_on"].iloc[4])
    assert flip["notional"] == pytest.approx(2 * at_fill, rel=1e-12)
    assert flip["spread"] == pytest.approx(2 * at_fill * 0.5e-4, rel=1e-12)
    ok, reasons = account.check(r, "margin_30k")
    assert ok and not reasons and account.day_trades(account.fills(r)).empty


def test_random_target_has_no_edge_and_loses_exactly_the_booked_cost():
    n = 2500
    m = synth(n, seed=11, div_every=0)
    f = np.random.default_rng(2).choice([-1.0, 1.0], n)
    r = run(m, Fixed(tuple(f)), costs=CostModel(round_trip_bp=1.0, borrow_rate=0.0), cash_yield=0.0)
    g = r.ledger["gross"].iloc[1:]
    assert abs(g.mean()) < 2 * g.std(ddof=1) / np.sqrt(len(g))
    np.testing.assert_allclose(r.ledger["net"] - r.ledger["gross"], -r.ledger["cost_ret"], rtol=0, atol=1e-15)
    assert (r.ledger["cost_ret"] > 0).sum() > 1000


def test_moc_and_next_open_agree_when_open_equals_previous_close():
    m = synth(300, seed=4, div_every=0, flat_gap=True)
    f = pd.Series(np.random.default_rng(3).uniform(-1, 1, 300), index=m.index)
    y = pd.Series(0.0, index=m.index)  # carry differs by design: moc holds f_t overnight, next_open f_{t-1}
    a = simulate(legs(m, "moc")[0], f, "moc", ZERO, y, 3e4)
    b = simulate(legs(m, "next_open")[0], f, "next_open", ZERO, y, 3e4)
    np.testing.assert_allclose(a["gross"], b["gross"], rtol=0, atol=1e-15)
    np.testing.assert_allclose(a["net"], b["net"], rtol=0, atol=1e-15)


def test_carry_interest_and_borrow_over_weekends():
    m = synth(10, div_every=0)
    f = (0, -1, -1, -1, -1, -0.5, -0.5, 0.25, 0.25, 0)
    costs = CostModel(round_trip_bp=0.0, borrow_rate=0.004, fees=False)
    r = run(m, Fixed(f), costs=costs, cash_yield=0.05)
    lg = r.ledger
    e_prev = np.r_[30_000.0, lg["equity"].to_numpy()[:-1]]
    days = np.r_[0, np.diff(m.index.tz_localize(None)).astype("timedelta64[D]").astype(int)]
    a = lg["exp_overnight"].to_numpy()
    np.testing.assert_allclose(lg["borrow"], e_prev * days / 360 * 0.004 * np.maximum(-a, 0), rtol=1e-12)
    np.testing.assert_allclose(lg["interest"], e_prev * days / 360 * 0.05 * (1 - np.abs(a)), rtol=1e-12)
    assert days[4] == 3 and lg["borrow"].iloc[4] > 2.9 * lg["borrow"].iloc[3]  # Fri → Mon: three nights
    # a short opened at the open of session 2 and held to the open of 3 crosses one close: one night of borrow
    assert lg["borrow"].iloc[2] == 0 and lg["borrow"].iloc[3] > 0
    np.testing.assert_allclose(lg["excess"], lg["net"] - 0.05 * days / 360, rtol=0, atol=1e-16)


def test_vectorized_equals_loop_on_fixture_quarter():
    m = fixture_market()
    t_vec = target_series(R0, m, vectorized=True)
    t_loop = target_series(R0, m, vectorized=False)
    np.testing.assert_allclose(t_vec, t_loop, rtol=0, atol=1e-12)
    assert t_vec.abs().sum() > 0
    costs = CostModel(round_trip_bp=2.3, borrow_rate=0.004)
    a = run(m, R0, costs=costs, cash_yield=0.0525, vectorized=True)
    b = run(m, R0, costs=costs, cash_yield=0.0525, vectorized=False)
    np.testing.assert_allclose(a.ledger.to_numpy(), b.ledger.to_numpy(), rtol=1e-12, atol=1e-12)
    np.testing.assert_allclose(a.ledger["net"], b.ledger["net"], rtol=0, atol=1e-12)


@pytest.mark.parametrize("strategy", [VwapCell(3, 0.001), ExoState(), SnapCell()])
def test_views_and_vectorized_targets_agree(strategy):
    m = synth(200, seed=8)
    np.testing.assert_allclose(
        target_series(strategy, m), target_series(strategy, m, vectorized=False), atol=0
    )


def test_t1545_view_hides_session_close_and_close_view_includes_it():
    m = synth(30)
    for i, v in enumerate(views(m, T1545)):
        assert v.snapshot is not None and len(v.sessions) == i and v.snapshot.name == m.index[i]
        assert v.when.hour == 15 and v.when.minute == 45
    for i, v in enumerate(views(m, CLOSE)):
        assert v.sessions.index[-1] == m.index[i] and v.snapshot is None
        assert v.exo is not None and v.exo.when.hour == 16 and v.exo.when.minute == 25


# ── Causality (PLAN.md §8.6) ────────────────────────────────────────────────


@pytest.mark.parametrize("strategy", [VwapCell(5, 0.001), R0, ExoState(), SnapCell()])
def test_strategies_are_causal(strategy):
    m = synth(300, seed=12)
    assert_causal(lambda mk: target_series(strategy, mk), m, strategy.decision)


@pytest.mark.parametrize("strategy", [R0, SnapCell(), ExoState()])
def test_engine_is_causal_at_both_decision_times(strategy):
    m = synth(300, seed=13)
    # moc books its fill at the close of c, which the 15:45 decision precedes: compare targets <= c, net <= c - 1
    lag = 1 if strategy.decision == T1545 else 0

    def fn(mk):
        r = run(mk, strategy, costs=CostModel(round_trip_bp=1.0), cash_yield=tbill_yield(mk))
        return pd.concat([r.targets, r.ledger["net"].shift(lag)], axis=1)

    assert_causal(fn, m, strategy.decision, truncate=False)


def test_harness_catches_a_one_bar_peek():
    m = synth(300, seed=14)
    with pytest.raises(AssertionError):
        assert_causal(lambda mk: target_series(PeekNext(), mk), m, CLOSE)


def test_harness_catches_a_t1545_strategy_reading_the_close():
    m = synth(300, seed=15)
    with pytest.raises(AssertionError):
        assert_causal(lambda mk: target_series(SnapCell(leak=True), mk), m, T1545)
    with pytest.raises(AssertionError):  # and through the engine
        assert_causal(lambda mk: run(mk, SnapCell(leak=True), costs=ZERO, cash_yield=0.0).targets, m, T1545,
                      truncate=False)  # fmt: skip


def _gross_fn(lg, fill):
    y = pd.Series(0.0, index=lg.index)
    return lambda f: simulate(lg, pd.Series(f, index=lg.index), fill, ZERO, y, 1.0)["gross"].to_numpy()


def test_fills_after_decision_and_planted_same_bar_fill_caught():
    m = synth(60, seed=16, div_every=0)
    f = np.random.default_rng(4).choice([-1.0, 1.0], 60)
    c = 30
    lg, _ = legs(m, "next_open")
    # next_open: f_c cannot earn anything up to the opening print of c+1 (rows ≤ c, and c+1's overnight leg)
    assert_fill_after(_gross_fn(lg, "next_open"), f, c, slice(0, c + 1))
    gap_only = lg.copy()
    gap_only[["r_id", "r_cc"]] = 0.0
    gap_only["r_cc"] = gap_only["r_on"]  # a market that only moves overnight
    # the gap into c+1 is earned by f_{c-1} (held at close c); f_c is first exposed to the gap into c+2
    assert_fill_after(_gross_fn(gap_only, "next_open"), f, c, slice(0, c + 2))
    with pytest.raises(AssertionError):  # the same-close fill earns the gap with the decision it follows
        assert_fill_after(_gross_fn(gap_only, "same_close"), f, c, slice(0, c + 2))
    # moc: f_c earns nothing up to the close of c
    assert_fill_after(_gross_fn(legs(m, "moc")[0], "moc"), f, c, slice(0, c + 1))


# ── same_close (test-only / diagnostic) ─────────────────────────────────────


def test_same_close_is_unreachable_from_a_spec_fill():
    m = synth(20)
    with pytest.raises(ValueError, match="fills"):
        run(m, Fixed(tuple([1.0] * 20), fill="same_close"), costs=ZERO, cash_yield=0.0)
    with pytest.raises(ValueError, match="close decisions"):
        run_diagnostic(m, SnapCell(), fill="same_close", costs=ZERO, cash_yield=0.0)
    assert "same_close" not in engine.FILLS


def test_same_close_reproduces_hand_computed_example():
    closes = np.array([100, 102, 101, 103, 100, 100, 99, 101, 102, 100.0])
    idx = pd.bdate_range("2019-03-04", periods=10).tz_localize(NY)
    s = pd.DataFrame({"open_raw": np.nan, "close_raw": closes, "dividend": 0.0, "close": closes, "volume": 1.0},
                     index=idx)  # fmt: skip
    f = (1, 1, -1, 0, 1, -1, -1, 1, 0, 0)
    r = run_diagnostic(Market(s, "synthetic"), Fixed(f), fill="same_close", costs=ZERO, cash_yield=0.0)
    hand = [0, 2 / 100, -1 / 102, -2 / 101, 0, 0, 1 / 100, -2 / 99, 1 / 101, 0]
    np.testing.assert_allclose(r.ledger["net"], hand, rtol=0, atol=1e-15)
    assert r.diagnostic and np.prod(1 + np.array(hand)) * 30_000 == pytest.approx(r.ledger["equity"].iloc[-1])


# ── Market, fallbacks and the diag guard ────────────────────────────────────


def test_missing_cross_falls_back_counted_and_raises_without_fallback(capsys):
    m = synth(50, div_every=0)
    s = m.sessions.copy()
    s["open_fallback_raw"], s["close_fallback_raw"] = s["open_raw"] * 1.0001, s["close_raw"] * 0.9999
    s.iloc[10, s.columns.get_loc("close_raw")] = np.nan
    s.iloc[20, s.columns.get_loc("open_raw")] = np.nan
    r = run(replace(m, sessions=s), _const(1.0, n=50), costs=ZERO, cash_yield=0.0)
    assert r.fallbacks == {"open": 1, "close": 1} and "RTH 5Min" in capsys.readouterr().out
    assert r.legs["close_px"].iloc[10] == s["close_fallback_raw"].iloc[10]
    with pytest.raises(ValueError, match="no close cross"):
        run(
            replace(m, sessions=s.drop(columns=["close_fallback_raw"])),
            _const(1.0, n=50),
            costs=ZERO,
            cash_yield=0.0,
        )


def test_first_session_open_is_never_needed_under_next_open():
    m = synth(20, div_every=0)
    s = m.sessions.copy()
    s.iloc[0, s.columns.get_loc("open_raw")] = np.nan  # no fallback columns at all
    r = run(replace(m, sessions=s), _const(1.0, n=20), costs=CostModel(), cash_yield=0.0)
    assert np.isfinite(r.ledger.to_numpy()).all() and r.fallbacks == {"open": 0, "close": 0}


def test_diag_guard_is_sticky():
    m = synth(30)
    diag = m.sessions.copy()
    diag.attrs["source"] = "diag"
    with pytest.raises(DiagDataError):
        Market(diag, "synthetic")
    with pytest.raises(DiagDataError):
        run(Market(diag, "diag"), _const(1.0, n=30), costs=ZERO, cash_yield=0.0)
    assert run_diagnostic(Market(diag, "diag"), _const(1.0, n=30), fill="next_open", costs=ZERO,
                          cash_yield=0.0).diagnostic  # fmt: skip
    rebuilt = pd.DataFrame(m.sessions.to_dict("list"), index=m.index)  # columns copied, provenance lost
    with pytest.raises(DiagDataError, match="load_market"):
        Market(rebuilt, "alpaca")


def test_targets_out_of_range_or_nan_raise():
    m = synth(10)
    with pytest.raises(ValueError, match="finite"):
        run(m, Fixed((0.0,) * 9 + (np.nan,)), costs=ZERO, cash_yield=0.0)
    with pytest.raises(ValueError, match="finite"):
        run(m, Fixed((1.5,) * 10), costs=ZERO, cash_yield=0.0)
    with pytest.raises(ValueError, match="cash_yield"):
        run(m, _const(1.0, n=10), costs=ZERO, cash_yield=pd.Series(0.01, index=m.index[:5]))


# ── Costs ───────────────────────────────────────────────────────────────────


def test_fee_table_dates():
    d = pd.DatetimeIndex(["2016-03-01", "2021-03-01", "2024-05-21", "2024-05-22", "2025-06-02"]).tz_localize(
        NY
    )
    r = fee_rates(d)
    np.testing.assert_allclose(r["sec"] * 1e6, [21.80, 5.10, 8.00, 27.80, 0.00])
    np.testing.assert_allclose(r["taf"], [0.000119, 0.000119, 0.000166, 0.000166, 0.000166])
    with pytest.raises(ValueError, match="fee table starts"):
        fee_rates(pd.DatetimeIndex(["2014-01-02"]))


def test_cost_model_curve_measured_and_reprice():
    assert CostModel(round_trip_bp=1.0).one_way("open_auction") == pytest.approx(0.5e-4)
    with pytest.raises(ValueError):
        CostModel(round_trip_bp="cheap")
    with pytest.raises(FileNotFoundError, match="measured"):
        measured("open_auction", ROOT / "data" / "costs" / "nope.csv")
    m = synth(300, seed=21)
    y = tbill_yield(m)
    r = run(m, R0, costs=CostModel(round_trip_bp=1.0), cash_yield=y)
    for bp in (0.3, 2.3):
        np.testing.assert_allclose(reprice(r, bp).ledger, run(m, R0, costs=CostModel(round_trip_bp=bp),
                                                               cash_yield=y).ledger, rtol=1e-13)  # fmt: skip
    assert reprice(r, 2.3).net.sum() < r.net.sum() < reprice(r, 0.3).net.sum()


def test_tbill_yield_refuses_stale_values():
    m = synth(60)
    assert m.exo is not None
    f = m.exo._frames["DTB3"].iloc[:20]
    with pytest.raises(ValueError, match="older than"):
        tbill_yield(replace(m, exo=Exo({"DTB3": f})))
    y = tbill_yield(m)
    assert y.notna().all() and y.between(0.005, 0.05).all()


# ── Account ─────────────────────────────────────────────────────────────────


def test_account_profiles():
    m = synth(100)
    f = tuple(np.random.default_rng(5).choice([-1.0, 0.0, 1.0], 100))
    r = run(m, Fixed(f), costs=CostModel(), cash_yield=0.0)
    assert account.check(r, "margin_30k") == (True, [])
    ok, reasons = account.check(r, "cash_30k")
    assert not ok and "cannot short" in reasons[0]
    long_only = run(m, Fixed(tuple(max(x, 0.0) for x in f)), costs=CostModel(), cash_yield=0.0)
    assert account.check(long_only, "cash_30k") == (True, [])
    ok, reasons = account.check(
        run(m, Fixed(f), costs=CostModel(), cash_yield=0.0, equity0=10_000), "margin_30k"
    )
    assert not ok and "profile equity" in reasons[0]
    with pytest.raises(ValueError, match="unknown"):
        account.get_profile("margin_1m")
    trades = pd.DataFrame(
        {"side": [1, -1, 1]}, index=pd.DatetimeIndex(["2020-01-02", "2020-01-02", "2020-01-03"])
    )
    assert account.day_trades(trades).to_dict() == {pd.Timestamp("2020-01-02"): 1}


# ── Metrics and features ────────────────────────────────────────────────────


def test_drawdown_and_metrics():
    net = pd.Series([0.1, -0.5, 0.2, 0.0, 1.0])
    mdd, dur = drawdown(net)
    assert mdd == pytest.approx(0.5) and dur == 3
    m = synth(504, seed=31)
    r = run(m, R0, costs=CostModel(), cash_yield=tbill_yield(m))
    bench = run(m, _const(1.0, n=504), costs=ZERO, cash_yield=tbill_yield(m)).excess
    s = summary(r, bench)
    assert 0 < s["psr0"] < 1 and s["round_trips_yr"] == pytest.approx(s["turnover"] / 2)
    assert {"beta", "alpha_ann", "sortino", "max_dd_sessions", "flat_share", "worst_day"} <= set(s)
    assert list(by_year(r).index) == [2018, 2019]
    assert psr(0.0, 100) == pytest.approx(0.5)


def test_vwap_and_deviations_by_hand():
    p = pd.Series([10.0, 11.0, 12.0, 13.0])
    v = pd.Series([1.0, 3.0, 0.0, 2.0])
    w = rolling_vwap(p, v, 2)
    assert (
        np.isnan(w.iloc[0]) and w.iloc[1] == pytest.approx((10 + 33) / 4) and w.iloc[2] == pytest.approx(11.0)
    )
    assert np.isnan(rolling_vwap(pd.Series([1.0, 2.0]), pd.Series([0.0, 0.0]), 2).iloc[1])
    assert trading_deviation(p, v, 2).iloc[3] == pytest.approx(13 / ((12 * 0 + 13 * 2) / 2) - 1)
    assert state_deviation(p, v, 2).iloc[3] == pytest.approx(13 / 11.0 - 1)
