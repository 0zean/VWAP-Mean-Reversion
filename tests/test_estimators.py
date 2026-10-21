"""U8: Hurst-type estimators, the causal MODWT, state variables and the synthetic generators. No network."""

import warnings

import numpy as np
import pandas as pd
import pytest
from causality import assert_causal

from vwap_mr.config import DecisionTime
from vwap_mr.data.synthetic import daily_market
from vwap_mr.features.state import (
    SMALL,
    StateConfig,
    lag1_autocorr,
    market_state,
    realized_vol,
    variance_ratio,
)
from vwap_mr.regime import calibrate as cb
from vwap_mr.regime import hurst
from vwap_mr.regime.wavelet import abry_veitch, boundary_len, causal_modwt, modwt_filters, rolling_av

CLOSE, T1545 = DecisionTime.CLOSE, DecisionTime.T1545

with warnings.catch_warnings():
    warnings.simplefilter("ignore", SyntaxWarning)
    import nolds
    from MFDFA import MFDFA


# ── cross-checks (done-when 1) ──────────────────────────────────────────────


@pytest.mark.parametrize("seed", [0, 1, 2])
def test_dfa_h2_matches_the_mfdfa_package(seed):
    """MFDFA cuts segments from both ends, as we do (PLAN §7 U8 expected one end; the package's code does both)."""
    x = cb.fgn(1500, 0.45 + 0.05 * seed, np.random.default_rng(seed))
    sc = hurst.scales("standard")
    lag, f = MFDFA(x, sc, q=np.array([2.0]), order=1)
    pkg = np.polyfit(np.log(lag), np.log(f[:, 0]), 1)[0]
    assert abs(hurst.mfdfa(x)["h2"] - pkg) < 0.01
    assert hurst.mfdfa(x)["h2"] == pytest.approx(hurst.dfa(x)["alpha"], abs=1e-12)
    one_end = hurst.dfa(x, both_ends=False)["alpha"]
    assert abs(one_end - pkg) < 0.05  # the conventions differ, but not much


@pytest.mark.parametrize("seed", [0, 1, 2])
def test_rs_matches_nolds(seed):
    z = np.random.default_rng(seed).standard_normal(2000)
    nv = np.unique(np.logspace(1, np.log10(500), 12).astype(int))
    assert hurst.rs_hurst(z, nv) == pytest.approx(
        nolds.hurst_rs(z, nvals=nv, fit="poly", corrected=False), abs=1e-10
    )


@pytest.mark.parametrize("H", [0.40, 0.50, 0.60])
def test_abry_veitch_recovers_h_on_fgn(H):
    rng = np.random.default_rng(int(H * 100))
    est = [abry_veitch(cb.fgn(1024, H, rng)) for _ in range(150)]
    assert abs(np.mean(est) - H) < 0.03


def test_lo_modified_rs_is_in_its_short_memory_range_on_noise():
    rng = np.random.default_rng(4)
    v = [hurst.lo_modified_rs(rng.standard_normal(1000)) for _ in range(200)]
    assert 0.9 < np.mean((np.array(v) > 0.809) & (np.array(v) < 1.862)) <= 1.0


# ── the estimators ──────────────────────────────────────────────────────────


def test_dfa_on_returns_not_the_profile():
    x = np.random.default_rng(5).standard_normal(2000)
    assert abs(hurst.dfa(x)["alpha"] - 0.5) < 0.08
    assert abs(hurst.dfa(np.cumsum(x))["alpha"] - 1.5) < 0.1  # a price-like input reads H + 1


def test_rolling_equals_single_window():
    r = pd.Series(cb.fgn(2500, 0.55, np.random.default_rng(6)))
    i = 2000
    assert hurst.rolling_dfa(r, 504, "short").iloc[i] == pytest.approx(
        hurst.dfa(r.iloc[i - 503 : i + 1], "short")["alpha"], abs=1e-12)  # fmt: skip
    mf = hurst.rolling_mfdfa(r, 1008).iloc[i]
    one = hurst.mfdfa(r.iloc[i - 1007 : i + 1])
    assert mf["mfdfa_h2"] == pytest.approx(one["h2"], abs=1e-12)
    assert mf["mfdfa_dalpha"] == pytest.approx(one["delta_alpha"], abs=1e-10)
    s = hurst.rolling_dfa(r, 504)
    assert s.iloc[:503].isna().all() and s.iloc[503:].notna().all()


def test_rolling_estimates_skip_windows_holding_a_nan():
    r = pd.Series(np.random.default_rng(7).standard_normal(800))
    r.iloc[300] = np.nan
    s = hurst.rolling_dfa(r, 200, "short")
    assert s.iloc[300:500].isna().all() and s.iloc[500:].notna().all()


def test_modwt_filters_and_white_noise_variances():
    g, h = modwt_filters()
    assert g.sum() == pytest.approx(1.0) and h.sum() == pytest.approx(0.0, abs=1e-12)
    assert (h**2).sum() == pytest.approx(0.5)
    W = causal_modwt(np.random.default_rng(8).standard_normal(40000), 4)
    for j in range(4):
        assert np.isnan(W[j, : boundary_len(j + 1)]).all() and not np.isnan(W[j, boundary_len(j + 1)])
        assert np.nanmean(W[j] ** 2) == pytest.approx(0.5 ** (j + 1), rel=0.06)


def test_causal_modwt_ignores_the_future():
    x = np.random.default_rng(9).standard_normal(600)
    y = x.copy()
    y[400:] = 10 * np.random.default_rng(10).standard_normal(200)
    np.testing.assert_array_equal(causal_modwt(x, 4)[:, :400], causal_modwt(y, 4)[:, :400])


# ── state variables ─────────────────────────────────────────────────────────


def test_state_features_by_hand():
    r = pd.Series([0.01, -0.02, 0.03, 0.0, 0.01, -0.01, 0.02])
    assert realized_vol(r, 3).iloc[4] == pytest.approx(np.std([0.03, 0.0, 0.01], ddof=1))
    assert lag1_autocorr(r, 4).iloc[5] == pytest.approx(np.corrcoef([0.03, 0.0, 0.01, -0.01], [-0.02, 0.03, 0.0,
                                                                                                0.01])[0, 1])  # fmt: skip
    rng = np.random.default_rng(11)
    e = rng.standard_normal(20000)
    assert variance_ratio(pd.Series(e), 5, 20000).iloc[-1] == pytest.approx(1.0, abs=0.05)
    x = np.zeros(20000)
    for t in range(1, 20000):
        x[t] = 0.6 * x[t - 1] + e[t]
    assert cb.ar1_phi(pd.Series(x), 20000 - 1).iloc[-1] == pytest.approx(0.6, abs=0.02)


def test_state_frame_columns_and_momentum():
    m = daily_market("SPY", "2016-01-04", "2017-12-29", 3)
    net = pd.Series(np.random.default_rng(12).normal(0, 0.01, len(m.index)), index=m.index)
    st = market_state(m, net=net, cfg=SMALL)
    assert {"B1_vol", "B2_vix", "B3_ac1", "B4_phi", "B5_vr5", "B6_mom", "E1_dfa_std", "E2_dfa_short",
            "E3_av_short", "E4_h2", "E4_dalpha"} <= set(st.columns)  # fmt: skip
    assert st["B6_mom"].iloc[30] == pytest.approx(net.iloc[11:31].sum())
    assert st.iloc[290:].notna().all().all()


def test_registered_windows_are_section_6_4():
    c = StateConfig()
    assert (c.vol, c.ac, c.phi, c.vr, c.mom, c.dfa, c.av, c.mf) == (21, 63, 126, 252, 63, 504, 512, 1008)


@pytest.mark.parametrize("decision", [CLOSE, T1545])
def test_state_is_causal_at_both_decision_times(decision):
    """Row t is as of t's close decision; a t1545 view sees rows ≤ t−1, i.e. the frame shifted by one."""
    m = daily_market("SPY", "2016-01-04", "2017-12-29", 4)
    lag = 0 if decision == CLOSE else 1
    assert_causal(lambda mk: market_state(mk, cfg=SMALL).shift(lag), m, decision)


def test_harness_catches_a_peeking_estimator_and_a_symmetric_boundary():
    m = daily_market("SPY", "2016-01-04", "2017-06-30", 5)

    def peek(mk):
        return hurst.rolling_dfa(mk.sessions["close"].pct_change().shift(-1), 120, "short")

    def symmetric(mk):  # a centred (non-causal) level-1 detail filter: the DWT boundary / centring mutant
        r = mk.sessions["close"].pct_change().fillna(0.0).to_numpy()
        _, h = modwt_filters()
        w = np.convolve(r, h, mode="same")
        return pd.Series(w**2, index=mk.index).rolling(64).mean()

    for fn in (peek, symmetric):
        with pytest.raises(AssertionError):
            assert_causal(fn, m, CLOSE)
    assert_causal(lambda mk: rolling_av(mk.sessions["close"].pct_change(), 128, "short"), m, CLOSE)


# ── generators and the study ────────────────────────────────────────────────


@pytest.mark.parametrize("H", [0.4, 0.6])
def test_fgn_autocorrelation_matches_theory(H):
    rng = np.random.default_rng(13)
    x = np.concatenate([cb.fgn(4096, H, rng) for _ in range(20)])
    assert np.corrcoef(x[1:], x[:-1])[0, 1] == pytest.approx(0.5 * (2 ** (2 * H) - 2), abs=0.015)


def test_arfima_t_is_unit_variance_and_fat_tailed():
    x = cb.arfima_t(20000, 0.5, np.random.default_rng(14))
    assert x.std() == pytest.approx(1.0, abs=1e-12)
    assert ((x - x.mean()) ** 4).mean() / x.var() ** 2 > 4


def test_min_window_rule():
    tab = pd.DataFrame({"estimator": ["e"] * 4, "innovations": ["g"] * 4, "H": [0.4, 0.6, 0.4, 0.6],
                        "n": [252, 252, 504, 504], "sd": [0.03, 0.02, 0.024, 0.025]})  # fmt: skip
    assert cb.min_window(tab).iloc[0]["min_n"] == 504
    tab.loc[3, "sd"] = 0.026
    assert np.isnan(cb.min_window(tab).iloc[0]["min_n"])


def test_noise_study_small():
    tab = cb.noise_table(Hs=(0.5,), ns=(252,), draws=10)
    assert set(tab["estimator"]) == set(cb.ESTIMATORS) - {"Abry–Veitch standard j 2–6"}
    assert (tab["sd"] > 0).all()
    assert cb.rw_phi_null(n_sessions=1500, draws=3)["mean"] > 0.5
    da = cb.delta_alpha_tails(n=1008, draws=10)
    assert da.set_index("series").loc["iid t4", "delta_alpha_mean"] > da.set_index("series").loc["iid Gaussian",
                                                                                                "delta_alpha_mean"]  # fmt: skip
