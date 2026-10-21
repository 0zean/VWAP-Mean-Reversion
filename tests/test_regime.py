"""U9: Hamilton filter, Markov-switching refits, BOCPD, the detection metrics. No network; synthetic series."""

import warnings

import numpy as np
import pandas as pd
import pytest
from causality import assert_causal
from scipy import stats

from vwap_mr.config import DecisionTime
from vwap_mr.data.synthetic import daily_market
from vwap_mr.features.deviation import state_deviation
from vwap_mr.regime import calibrate as cb
from vwap_mr.regime.bocpd import NIG, bocpd, bocpd_avg, prior_from
from vwap_mr.regime.hmm import (
    MSParams,
    fit_ms,
    from_statsmodels,
    hamilton_filter,
    rolling_regime,
    standardized,
)

CLOSE = DecisionTime.CLOSE


# ── Hamilton filter (done-when 1) ───────────────────────────────────────────


@pytest.mark.parametrize("with_exog", [False, True])
def test_hamilton_filter_equals_statsmodels(with_exog):
    from statsmodels.tsa.regime_switching.markov_regression import MarkovRegression

    rng = np.random.default_rng(0)
    y, x = rng.standard_normal(400), rng.standard_normal(400)
    exog = x if with_exog else None
    m = MarkovRegression(y, k_regimes=2, exog=exog, switching_variance=True)
    p = np.array([0.95, 0.1, 0.1, -0.2] + ([0.3, 0.8] if with_exog else []) + [1.0, 2.5])
    ref = m.filter(p)
    P = from_statsmodels(m, p)
    out = hamilton_filter(P.log_density(y, exog), P.A, P.ergodic())
    np.testing.assert_allclose(out["filtered"], ref.filtered_marginal_probabilities, rtol=0, atol=1e-10)
    np.testing.assert_allclose(out["predicted"], ref.predicted_marginal_probabilities, rtol=0, atol=1e-10)
    assert out["loglik"] == pytest.approx(ref.llf, abs=1e-8)
    np.testing.assert_allclose(out["one_step"][:-1], out["predicted"][1:], atol=1e-15)


def test_params_reorder_and_ergodic():
    P = MSParams(
        np.array([0.0, 1.0]), np.array([0.9, 0.2]), np.array([1.0, 4.0]), np.array([[0.9, 0.1], [0.3, 0.7]])
    )
    np.testing.assert_allclose(P.ergodic(), [0.75, 0.25])
    Q = P.reorder(np.array([1, 0]))
    np.testing.assert_allclose(Q.A, [[0.7, 0.3], [0.1, 0.9]])
    assert Q.beta is not None and Q.beta.tolist() == [0.2, 0.9]


def test_fit_recovers_two_volatility_regimes():
    x, _ = cb.ms_series(1500, 100, np.random.default_rng(1), kind="sigma")
    params, conv = fit_ms(x)
    assert conv
    srt = np.sort(np.sqrt(params.sigma2))
    assert srt[0] == pytest.approx(1.0, abs=0.15) and srt[1] == pytest.approx(2.0, abs=0.3)


# ── causality (done-when 1) ─────────────────────────────────────────────────


def _raw_returns(mk) -> pd.Series:
    """Returns of the raw closes: the harness leaves them bit-identical before the cut (adjusted closes there move by
    a constant factor, i.e. 1e-16 in returns, which an MLE's stopping rule amplifies to ~1e-5)."""
    return mk.sessions["close_raw"].pct_change()


def _r1(mk, fit=120, every=10):
    z = standardized(_raw_returns(mk), 60)
    return rolling_regime(z, kind="sigma", fit_window=fit, refit_every=every)[0]["p_next"]


def test_rolling_regime_is_causal():
    m = daily_market("SPY", "2016-01-04", "2017-03-31", 2)
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        assert_causal(_r1, m, CLOSE)


def test_r2_is_causal():
    def r2(mk, fit=120, every=10):
        d = state_deviation(mk.sessions["close_raw"], mk.sessions["volume"], 5)
        return rolling_regime(d, pd.Series(d.shift(1)), kind="phi", fit_window=fit, refit_every=every)[0][
            "p_next"
        ]

    m = daily_market("SPY", "2016-01-04", "2017-03-31", 3)
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        assert_causal(r2, m, CLOSE)


def test_a_nan_after_t_never_removes_the_gate_at_t():
    x, _ = cb.ms_series(600, 50, np.random.default_rng(12), kind="sigma")
    y = pd.Series(x)
    base, _ = rolling_regime(y, kind="sigma", fit_window=200, refit_every=20)
    holed = y.copy()
    holed.iloc[215] = np.nan
    out, log = rolling_regime(holed, kind="sigma", fit_window=200, refit_every=20)
    np.testing.assert_array_equal(out["p_next"].iloc[:215], base["p_next"].iloc[:215])
    assert out["p_next"].iloc[215:220].isna().all() and any(r.get("skipped") for r in log)


def test_harness_catches_a_refit_that_sees_its_own_block():
    from vwap_mr.regime.hmm import fit_ms as fit

    def leaky(mk, fit_window=120, every=10):
        z = standardized(_raw_returns(mk), 60).to_numpy()
        out = np.full(len(z), np.nan)
        first = int(np.argmax(np.isfinite(z))) + fit_window
        for r in range(first, len(z), every):
            lo, stop = r - fit_window, min(len(z), r + every)
            params, _ = fit(z[lo:stop])  # the mutant: fitted on rows the block's decisions have not seen yet
            params = params.reorder(np.argsort(params.sigma2))
            o = hamilton_filter(params.log_density(z[lo:stop]), params.A, params.ergodic())
            out[r:stop] = o["one_step"][r - lo :, 1]
        return pd.Series(out, index=mk.index)

    m = daily_market("SPY", "2016-01-04", "2017-03-31", 2)
    with warnings.catch_warnings(), pytest.raises(AssertionError):
        warnings.simplefilter("ignore")
        assert_causal(leaky, m, CLOSE, k=3)


def test_standardization_is_trailing_and_a_full_sample_sigma_is_caught():
    m = daily_market("SPY", "2016-01-04", "2017-06-30", 3)
    assert_causal(lambda mk: standardized(mk.sessions["close"].pct_change(), 60), m, CLOSE)
    with pytest.raises(AssertionError):
        assert_causal(lambda mk: (r := mk.sessions["close"].pct_change()) / r.std(), m, CLOSE)
    r = pd.Series(np.arange(1.0, 11.0))
    assert standardized(r, 3).iloc[5] == pytest.approx(6.0 / np.std([3.0, 4.0, 5.0], ddof=1))


def test_bocpd_and_baselines_are_causal():
    m = daily_market("SPY", "2016-01-04", "2017-06-30", 4)

    def r3(mk):
        z = standardized(mk.sessions["close"].pct_change(), 60)
        v = z.to_numpy()
        s = int(np.argmax(np.isfinite(v)))
        out = np.full(len(v), np.nan)
        out[s:] = bocpd_avg(v[s:], NIG())["p_recent"]
        return pd.Series(out, index=mk.index)

    assert_causal(r3, m, CLOSE)
    assert_causal(lambda mk: cb.baseline_prob(mk.sessions["close"].pct_change(), "sigma"), m, CLOSE)


# ── BOCPD (done-when 1) ─────────────────────────────────────────────────────


def _reference_bocpd(x, prior: NIG, lam: float) -> np.ndarray:
    """Adams & MacKay (2007) Algorithm 1, directly: probability space, scipy's Student-t, no pruning."""
    T, H = len(x), 1 / lam
    R = np.zeros((T + 1, T + 1))
    R[0, 0] = 1.0
    mu, ka, al, be = [prior.mu0], [prior.kappa0], [prior.alpha0], [prior.beta0]
    out = np.zeros((T, T + 1))
    for t in range(T):
        pred = np.array([stats.t.pdf(x[t], 2 * al[i], mu[i], np.sqrt(be[i] * (ka[i] + 1) / (al[i] * ka[i])))
                         for i in range(t + 1)])  # fmt: skip
        R[t + 1, 1 : t + 2] = R[t, : t + 1] * pred * (1 - H)
        R[t + 1, 0] = (R[t, : t + 1] * pred * H).sum()
        R[t + 1] /= R[t + 1].sum()
        new_mu = [prior.mu0] + [(ka[i] * mu[i] + x[t]) / (ka[i] + 1) for i in range(t + 1)]
        new_be = [prior.beta0] + [
            be[i] + ka[i] * (x[t] - mu[i]) ** 2 / (2 * (ka[i] + 1)) for i in range(t + 1)
        ]
        mu, be = new_mu, new_be
        ka, al = [prior.kappa0] + [k + 1 for k in ka], [prior.alpha0] + [a + 0.5 for a in al]
        out[t] = R[t + 1]
    return out


def test_bocpd_matches_the_reference_and_p_r0_is_the_hazard():
    rng = np.random.default_rng(5)
    z = np.r_[rng.normal(0, 1, 100), rng.normal(1.5, 2, 100)]
    prior = NIG(0.0, 1.0, 3.0, 2.0)
    full = bocpd(z, 100.0, prior, prune=None, keep_matrix=True)
    np.testing.assert_allclose(full["R"], _reference_bocpd(z, prior, 100.0), rtol=0, atol=1e-8)
    np.testing.assert_allclose(full["p_r0"], 0.01, atol=1e-12)
    assert full["p_recent"][100:115].max() > 0.5 and full["p_recent"][62:95].max() < 0.3
    pruned = bocpd(z, 100.0, prior)
    np.testing.assert_allclose(pruned["p_recent"], full["p_recent"], atol=1e-3)


def test_prior_from_sample():
    z = np.random.default_rng(6).normal(0.1, 1.5, 5000)
    p = prior_from(z)
    assert p.mu0 == pytest.approx(0.1, abs=0.05) and p.beta0 / (p.alpha0 - 1) == pytest.approx(2.25, rel=0.05)


# ── detection metrics ───────────────────────────────────────────────────────


def test_detection_metrics_by_hand():
    target = np.array([0, 0, 1, 1, 1, 0, 0, 1, 1, 0], bool)
    p = np.array([0.1, 0.2, 0.6, 0.9, 0.2, 0.7, 0.1, 0.1, 0.2, 0.1])  # p[t] predicts target[t + 1]
    m = cb.detection_metrics(p, target)
    # signal at rows 1..9 = p[0..8] > 0.5 → [F, F, T, T, F, T, F, F, F]; target spells start at rows 2 and 7
    assert (
        m["switches"] == 2 and m["delay_median"] == 1 and m["missed"] == 0
    )  # row 3 signals the spell of row 2
    assert m["already_on"] == 1  # the signal at row 6 was on when the spell of row 7 began
    assert m["false_alarms_per_1000"] == pytest.approx(1 / 9 * 1000)  # the onset at row 6 (target off)
    on = target[1:]
    sig = p[:-1] > 0.5
    assert m["tpr"] == pytest.approx(sig[on].mean()) and m["fpr"] == pytest.approx(sig[~on].mean())
    assert m["brier"] == pytest.approx(np.mean((p[:-1] - target[1:]) ** 2))


def test_stuck_and_random_signals_score_zero_youden():
    _, target = cb.ms_series(5000, 63, np.random.default_rng(8), kind="sigma")
    stuck = cb.detection_metrics(np.ones(5000), target)
    assert stuck["youden"] == pytest.approx(0.0) and stuck["already_on"] == stuck["switches"]
    coin = cb.detection_metrics(np.random.default_rng(9).random(5000), target)
    assert abs(coin["youden"]) < 0.05


def test_change_metrics_by_hand():
    sw = np.zeros(60, bool)
    sw[10] = True
    p = np.zeros(60)
    p[13:15] = 0.9
    p[40] = 0.8
    m = cb.change_metrics(p, sw, horizon=21)
    assert m["delay_median"] == 3 and m["missed"] == 0
    assert m["false_alarms_per_1000"] == pytest.approx(1 / 60 * 1000)


def test_ms_series_durations_and_regimes():
    x, fast = cb.ms_series(20000, 63, np.random.default_rng(7), kind="phi")
    runs = np.diff(np.flatnonzero(np.r_[True, fast[1:] != fast[:-1], True]))
    assert runs.mean() == pytest.approx(63, rel=0.15)
    xs = pd.Series(x)
    lag = xs.shift(1)
    fs = pd.Series(fast)
    phi_fast = np.polyfit(lag[fs & lag.notna()], xs[fs & lag.notna()], 1)[0]
    assert phi_fast == pytest.approx(0.3, abs=0.1)
