"""stats/: Sharpe, PSR/MinTRL, Holm, block bootstrap tests, power, Bai–Perron, local-level tracker (U4, SPEC §6).

Simulations here run at reduced scale with tolerances wide enough for their Monte Carlo error; the done-when
numbers at full scale (1,000 simulations, n 2,500) are produced by `scripts/u04_checks.py`."""

import math
import warnings
from itertools import combinations

import numpy as np
import pytest
from scipy.stats import chi2, norm

from vwap_mr.stats.bootstrap import (
    andrews_bandwidth,
    circular_block_indices,
    cond_mean_test,
    hac_bartlett,
    mean_test,
    newey_west_ols,
    sharpe_test,
)
from vwap_mr.stats.breaks import (
    _cdf_from_table,
    _date_quantile,
    _local_level_filter,
    _resid,
    bai_perron,
    bai_perron_dates_cdf,
    bp_null,
    local_level,
    local_level_track,
    segment_dp,
    sup_f1,
)
from vwap_mr.stats.multiplicity import bonferroni, holm
from vwap_mr.stats.power import garch_t, mde_cond_diff, mde_delta_sharpe, mde_mean, simulate_rejection
from vwap_mr.stats.sharpe import min_track_record_length, psr, psr_of, return_moments, sharpe_ratio

EULER_GAMMA = 0.5772156649015329


# ── Sharpe, PSR, MinTRL ───────────────────────────────────────────────────────────────────────────────────────────


def _expected_max_sharpe(n_trials: int, var_trials: float) -> float:
    """Bailey & López de Prado (2014) SR₀ — test-only: DSR is not used by the program (PLAN §3.2)."""
    z = (1 - EULER_GAMMA) * norm.ppf(1 - 1 / n_trials) + EULER_GAMMA * norm.ppf(1 - 1 / (n_trials * np.e))
    return float(np.sqrt(var_trials) * z)


def test_dsr_example_reproduced_through_psr():
    # Bailey & López de Prado (2014), "A numerical example": annualized SR 2.5 over T = 1,250 daily returns,
    # N = 100 trials with annualized V[SR_n] = 1/2, skew −3, raw kurtosis 10, 250 observations a year.
    # Paper: SR₀ = 0.1132, DSR = PSR(SR₀) = 0.9004.
    sr, var = 2.5 / np.sqrt(250), 0.5 / 250
    sr0 = _expected_max_sharpe(100, var)
    assert round(sr0, 4) == 0.1132
    assert round(psr(sr, 1250, skew=-3, kurt=10, sr_star=sr0), 4) == 0.9004


def test_psr_uses_per_period_sharpe_and_raw_kurtosis():
    sr, T = 0.1, 500
    normal = norm.cdf(sr * np.sqrt(T - 1) / np.sqrt(1 + sr**2 / 2))
    assert psr(sr, T, skew=0, kurt=3) == pytest.approx(normal)
    assert psr(sr, T) == pytest.approx(normal)
    assert psr(sr, T, kurt=0) != pytest.approx(normal)  # excess kurtosis 0 would be a different answer
    assert psr(sr, T, skew=-1, kurt=8) < normal
    # an annualized Sharpe passed by mistake gives a visibly different (≈ 1) answer
    assert psr(sr * np.sqrt(252), T) > 0.999999 > psr(sr, T)


def test_min_track_record_length_and_psr_invert_each_other():
    for sr, sr_star, sk, ku, alpha in [
        (0.1, 0.0, 0.0, 3.0, 0.05),
        (0.08, 0.02, -1.0, 6.0, 0.01),
        (0.2, 0.1, 0.5, 4, 0.1),
    ]:
        n = min_track_record_length(sr, sr_star, sk, ku, alpha)
        z = (sr - sr_star) * np.sqrt(n - 1) / np.sqrt(1 - sk * sr + (ku - 1) / 4 * sr**2)
        assert norm.cdf(z) == pytest.approx(1 - alpha, abs=1e-12)  # PSR at the (continuous) MinTRL is 1 − α
        assert psr(sr, math.ceil(n), sk, ku, sr_star) >= 1 - alpha - 1e-12
        assert psr(sr, math.floor(n) - 1, sk, ku, sr_star) < 1 - alpha
    assert min_track_record_length(0.05, 0.05) == float("inf")
    assert min_track_record_length(-0.1) == float("inf")
    # PLAN §6.6: MinTRL for PSR(0) ≥ 0.95 is ≈ 10.8 years at annualized Sharpe 0.5, ≈ 2.7 at 1.0 (normal returns)
    assert min_track_record_length(0.5 / np.sqrt(252)) / 252 == pytest.approx(10.8, abs=0.05)
    assert min_track_record_length(1.0 / np.sqrt(252)) / 252 == pytest.approx(2.7, abs=0.05)


def test_return_moments_sharpe_and_psr_of():
    rng = np.random.default_rng(3)
    r = rng.normal(0.05, 1.0, 200_000)
    m = return_moments(r)
    assert m.sr == pytest.approx(r.mean() / r.std(ddof=1)) and m.n_obs == r.size
    assert m.skew == pytest.approx(0, abs=0.02) and m.kurt == pytest.approx(3, abs=0.05)
    assert return_moments(rng.standard_t(6, 400_000)).kurt == pytest.approx(3 + 6 / (6 - 4), rel=0.15)
    q = return_moments(r[:500])
    assert psr_of(r[:500]) == psr(q.sr, q.n_obs, q.skew, q.kurt)
    with pytest.raises(ValueError, match="zero variance"):
        sharpe_ratio(np.ones(10))
    with pytest.raises(ValueError, match="NaN"):
        sharpe_ratio(np.array([0.1, np.nan, 0.2]))


def test_psr_is_calibrated_under_the_null():
    rng = np.random.default_rng(7)
    hits = [psr_of(x) > 0.95 for x in rng.normal(0, 0.01, (4000, 250))]
    assert np.mean(hits) == pytest.approx(0.05, abs=0.012)


# ── Holm, Bonferroni ──────────────────────────────────────────────────────────────────────────────────────────────


def test_holm_hand_example():
    # m = 4. Sorted: 0.005·4 = 0.020; 0.010·3 = 0.030; 0.030·2 = 0.060; 0.040·1 = 0.040 → max-accumulated 0.060.
    p = [0.01, 0.04, 0.03, 0.005]
    assert holm(p) == pytest.approx([0.03, 0.06, 0.06, 0.02])
    # at α 0.05 Holm rejects H(0.005) and H(0.01) and stops at 0.03 > 0.05/2
    assert (holm(p) < 0.05).tolist() == [True, False, False, True]
    assert holm([0.3, 0.6]) == pytest.approx([0.6, 0.6])  # capped and monotone
    assert holm([0.02]) == pytest.approx([0.02])
    assert holm([0.01, 0.01]) == pytest.approx([0.02, 0.02])
    with pytest.raises(ValueError):
        holm([0.01, np.nan])
    with pytest.raises(ValueError):
        holm([1.2])
    assert bonferroni(0.02, 5) == pytest.approx(0.10) and bonferroni(0.5, 5) == 1.0


# ── HAC and the bootstrap tests ───────────────────────────────────────────────────────────────────────────────────


def _ar1(n, phi, rng, sigma=0.01):
    e = rng.normal(0, sigma, n + 200)
    x = np.empty_like(e)
    x[0] = e[0]
    for t in range(1, len(e)):
        x[t] = phi * x[t - 1] + e[t]
    return x[200:]


def test_hac_bartlett_is_newey_west_with_b_minus_1_lags():
    rng = np.random.default_rng(1)
    x = _ar1(800, 0.3, rng)
    T = len(x)
    se_hac = np.sqrt(hac_bartlett(x[:, None], 21)[0, 0] / T)
    assert newey_west_ols(x, np.empty((T, 0)), 20)["se"][0] == pytest.approx(se_hac, rel=1e-12)
    assert mean_test(x, n_boot=10)["se"] == pytest.approx(se_hac, rel=1e-12)
    # bandwidth 1 = the iid variance (population)
    assert hac_bartlett(x[:, None], 1)[0, 0] == pytest.approx(x.var(), rel=1e-12)


def test_cond_mean_test_statistic_and_se_are_the_regression_slope():
    rng = np.random.default_rng(2)
    x = _ar1(1500, 0.1, rng)
    g = rng.random(1500) < 0.4
    out = cond_mean_test(x, g, n_boot=50)
    assert out["delta"] == pytest.approx(x[g].mean() - x[~g].mean(), rel=1e-12)
    assert out["pi"] == pytest.approx(g.mean())
    nw = newey_west_ols(x, g.astype(float), 20)  # bandwidth 21 = 20 lags
    assert nw["beta"][1] == pytest.approx(out["delta"], rel=1e-10)
    assert nw["se"][1] == pytest.approx(out["se"], rel=1e-10)
    with pytest.raises(ValueError, match="boolean"):
        cond_mean_test(x, g.astype(float))
    with pytest.raises(ValueError, match="on and off"):
        cond_mean_test(x, np.zeros(1500, dtype=bool))


def test_cond_mean_test_counts_episodes():
    g = np.zeros(100, dtype=bool)
    g[[3, 4, 5, 20, 50, 51]] = True
    x = np.random.default_rng(0).normal(0, 0.01, 100)
    out = cond_mean_test(x, g, block=5, n_boot=50)
    assert out["episodes"] == 3 and out["episode_median"] == 2.0


def test_tests_raise_on_bad_input_and_are_seeded():
    x = np.random.default_rng(4).normal(0.0005, 0.01, 600)
    a, b = mean_test(x, n_boot=300, seed=5), mean_test(x, n_boot=300, seed=5)
    assert a == b
    with pytest.raises(ValueError, match="NaN"):
        mean_test(np.r_[x, np.nan])
    with pytest.raises(ValueError, match="2 blocks"):
        mean_test(x[:30])
    with pytest.raises(ValueError, match="sided"):
        mean_test(x, sided="both")
    with pytest.raises(ValueError, match="zero variance"):
        mean_test(np.zeros(100))


def test_one_sided_conventions():
    rng = np.random.default_rng(8)
    up = rng.normal(0.002, 0.01, 1000)
    one, two = mean_test(up, n_boot=999), mean_test(up, n_boot=999, sided="two")
    assert one["p"] < 0.01 and one["ci"][1] == float("inf") and one["ci"][0] > 0
    assert mean_test(-up, n_boot=999)["p"] > 0.99  # H1 is "mean > 0": a negative mean is not evidence
    assert two["ci"][0] < one["mean"] < two["ci"][1]
    assert cond_mean_test(up, up > 0, n_boot=200)["p"] < 0.01
    assert cond_mean_test(up, up < 0, n_boot=200)["p"] > 0.99


def test_one_sided_lower_bound_has_its_coverage():
    # P(true mean ≥ lower bound) ≈ 0.95; a bound built with +q·se or the α-quantile would cover ≈ 1.0 or ≈ 0.5
    mu, hits = 0.0003, 0
    for i in range(300):
        x = np.random.default_rng(3000 + i).normal(mu, 0.01, 600)
        out = mean_test(x, n_boot=299, seed=i)
        assert out["ci"][0] < out["mean"]
        hits += out["ci"][0] <= mu
    assert 0.91 <= hits / 300 <= 0.98


def test_invalid_resamples_are_counted_and_excluded():
    # a gate on 3 of 120 sessions: many resamples never see it on; they are counted, not silently used
    g = np.zeros(120, dtype=bool)
    g[[10, 60, 61]] = True
    x = np.random.default_rng(5).normal(0, 0.01, 120)
    out = cond_mean_test(x, g, block=5, n_boot=500, seed=1)
    assert out["n_invalid"] > 0
    valid = 500 - out["n_invalid"]
    assert out["p"] * (valid + 1) == pytest.approx(round(out["p"] * (valid + 1)))  # p's denominator is M′ + 1


def test_andrews_bandwidth():
    rng = np.random.default_rng(6)
    assert andrews_bandwidth(rng.normal(0, 1, 2500)) < 4  # iid: ≈ 1
    b = andrews_bandwidth(_ar1(2500, 0.2, rng))
    rho = 0.2
    expected = 1.1447 * (4 * rho**2 / ((1 - rho) ** 2 * (1 + rho) ** 2) * 2500) ** (1 / 3)
    assert b == pytest.approx(expected, rel=0.25)
    x = _ar1(500, 0.3, rng)
    z = x - x.mean()
    gam = [z @ z / 500, z[1:] @ z[:-1] / 500, z[2:] @ z[:-2] / 500]  # Γ_j = Σ z_t z_{t−j} / T
    expected = gam[0] + 2 * (1 - 1 / 2.5) * gam[1] + 2 * (1 - 2 / 2.5) * gam[2]  # fractional b: lags j < 2.5
    assert hac_bartlett(x[:, None], 2.5)[0, 0] == pytest.approx(expected, rel=1e-12)


def test_block_bootstrap_preserves_lag1_autocorrelation_within_10pct():
    rng = np.random.default_rng(9)
    x = _ar1(2500, 0.3, rng)

    def ac1(z):
        z = z - z.mean(axis=-1, keepdims=True)
        return (z[..., 1:] * z[..., :-1]).sum(-1) / (z * z).sum(-1)

    boot = ac1(x[circular_block_indices(len(x), 21, 2000, np.random.default_rng(0))]).mean()
    assert abs(boot / ac1(x) - 1) < 0.10
    # iid resampling (block 1) destroys it: the harness would notice a block-length bug
    assert abs(ac1(x[circular_block_indices(len(x), 1, 500, np.random.default_rng(0))]).mean()) < 0.05


def test_mean_test_size_and_power_small_scale():
    # 300 iid simulations at n 1,000: size within 0.05 ± 0.03 (MC se 0.013); power at the analytic MDE ≥ 0.70
    size = simulate_rejection("mean", 1000, 0.01, n_sims=300, n_boot=299, seed=11)["rejection_rate"]
    assert 0.02 <= size <= 0.08
    mde = mde_mean(1000, 0.01)["mde"]
    pw = simulate_rejection("mean", 1000, 0.01, mde, n_sims=300, n_boot=299, seed=12)["rejection_rate"]
    assert pw >= 0.70


def test_cond_mean_test_size_independent_coin_small_scale():
    out = simulate_rejection("cond_mean_diff", 1000, 0.01, n_sims=300, pi=0.4, n_boot=299, seed=13)
    assert out["rejection_rate"] <= 0.08


def test_sharpe_test_one_sample_and_paired():
    rng = np.random.default_rng(14)
    a = rng.normal(0.0006, 0.01, 2500)
    out = sharpe_test(a, n_boot=200)
    assert out["delta"] == pytest.approx(a.mean() / a.std(), rel=1e-12)  # population std (Ledoit–Wolf)
    sr = out["delta"]
    assert out["se"] == pytest.approx(np.sqrt((1 + sr**2 / 2) / 2500), rel=0.15)  # iid normal asymptotic se
    b = 0.9 * a + np.sqrt(1 - 0.81) * rng.normal(0.0006, 0.01, 2500)
    pr = sharpe_test(a, b, n_boot=200)
    assert pr["delta"] == pytest.approx(a.mean() / a.std() - b.mean() / b.std(), rel=1e-12)
    assert pr["delta_ann"] == pytest.approx(pr["delta"] * np.sqrt(252))
    # Memmel's iid se at ρ ≈ 0.9 against the HAC delta-method se
    rho = np.corrcoef(a, b)[0, 1]
    assert pr["se"] * np.sqrt(252) == pytest.approx(mde_delta_sharpe(2500, rho, 0.95)["se_ann"], rel=0.2)
    with pytest.raises(ValueError, match="same length"):
        sharpe_test(a, b[:-1])


def test_sharpe_test_size_paired_small_scale():
    rej = 0
    for i in range(150):
        rng = np.random.default_rng(1000 + i)
        common = rng.normal(0, 0.01, 1000)
        a = 0.9 * common + np.sqrt(0.19) * rng.normal(0, 0.01, 1000)
        b = 0.9 * common + np.sqrt(0.19) * rng.normal(0, 0.01, 1000)
        rej += sharpe_test(a, b, n_boot=199, seed=i)["p"] < 0.05
    assert rej / 150 <= 0.10


# ── Power ─────────────────────────────────────────────────────────────────────────────────────────────────────────


def test_mde_reproduces_plan_power_table():
    # PLAN §6.6, n 2,500, σ_d ≈ 1.05 %
    m = mde_mean(2500, 0.0105)
    assert m["se_bp_day"] == pytest.approx(2.1, abs=0.05) and m["mde_bp_day"] == pytest.approx(5.2, abs=0.1)
    assert m["mde_sharpe_ann"] == pytest.approx(0.79, abs=0.02)
    c = mde_cond_diff(2500, 0.0105, 0.40)
    assert c["se_bp_day"] == pytest.approx(4.3, abs=0.05) and c["mde_bp_day"] == pytest.approx(10.7, abs=0.2)
    assert mde_cond_diff(2500, 0.0105, 0.40, k=4)["mde_bp_day"] == pytest.approx(13.2, abs=0.2)
    assert mde_cond_diff(2500, 0.0105, 0.50)["se_bp_day"] == pytest.approx(4.2, abs=0.05)
    s = mde_delta_sharpe(2500, 0.9)
    assert s["se_ann"] == pytest.approx(0.14, abs=0.005) and s["mde_delta_sharpe"] == pytest.approx(
        0.35, abs=0.01
    )
    assert mde_delta_sharpe(2500, 0.7)["mde_delta_sharpe"] == pytest.approx(0.61, abs=0.01)
    # k = 1, two-sided uses α/2
    assert mde_mean(2500, 0.01, sided="two")["mde"] > m["mde"] * 0.01 / 0.0105
    with pytest.raises(ValueError):
        mde_cond_diff(2500, 0.01, 1.0)


def test_garch_t_has_target_variance_and_fat_tails():
    r = garch_t(20_000, 0.01, np.random.default_rng(0), size=10)
    assert r.shape == (10, 20_000)
    assert r.std() == pytest.approx(0.01, rel=0.05)
    assert return_moments(r.ravel()).kurt > 5
    sq = r[0] ** 2  # volatility clustering: |r| autocorrelated
    assert np.corrcoef(sq[1:], sq[:-1])[0, 1] > 0.05
    with pytest.raises(ValueError):
        garch_t(10, 0.01, np.random.default_rng(0), a=0.5, b=0.6)


# ── Bai–Perron ────────────────────────────────────────────────────────────────────────────────────────────────────


def test_segment_dp_matches_brute_force():
    rng = np.random.default_rng(15)
    y = rng.normal(0, 1, 30) + np.r_[np.zeros(10), np.full(12, 2.0), np.zeros(8)]
    h = 4
    fits = segment_dp(y, 3, h)

    def ssr(ends):
        return float((_resid(y, list(ends)) ** 2).sum())

    assert fits[0][0] == pytest.approx(((y - y.mean()) ** 2).sum())
    for m in (1, 2, 3):
        cands = [c for c in combinations(range(h, 31 - h), m) if all(np.diff([0, *c, 30]) >= h)]
        best = min(cands, key=ssr)
        assert fits[m][1] == list(best) and fits[m][0] == pytest.approx(ssr(best))


def test_break_date_cdf_matches_bai_1997_and_simulation():
    assert bai_perron_dates_cdf(0.0) == pytest.approx(0.5)
    assert _date_quantile(0.975) == pytest.approx(11.03, abs=0.01)  # Bai (1997): 95 % interval ±11.03
    assert _date_quantile(0.95) == pytest.approx(7.69, abs=0.01)
    rng = np.random.default_rng(0)
    dt, n = 0.02, 2500  # two-sided W(s) − |s|/2 on [−50, 50]
    drift = np.arange(1, n + 1) * dt / 2
    v = []
    for _ in range(4):
        w1 = np.cumsum(rng.normal(0, np.sqrt(dt), (500, n)), 1) - drift
        w2 = np.cumsum(rng.normal(0, np.sqrt(dt), (500, n)), 1) - drift
        m1, m2 = w1.max(1), w2.max(1)
        v.append(np.where(m1 > np.maximum(m2, 0), (w1.argmax(1) + 1) * dt,
                          np.where(m2 > np.maximum(m1, 0), -(w2.argmax(1) + 1) * dt, 0.0)))  # fmt: skip
    v = np.concatenate(v)
    for x in (1.0, 4.0, 11.03):
        assert np.mean(v <= x) == pytest.approx(float(bai_perron_dates_cdf(x)), abs=0.025)


def test_bp_null_table_is_monotone_and_agrees_with_an_independent_supf1():
    tab = bp_null()
    for c in ("supF1", "supF2", "supF3", "supF4", "supF5", "UDmax"):
        assert np.all(np.diff(tab[c]) >= 0)
    for m in range(1, 5):  # supF(m) falls with m (the statistic is divided by m); UDmax ≥ supF(1)
        assert np.interp(0.95, tab["prob"], tab[f"supF{m + 1}"]) < np.interp(
            0.95, tab["prob"], tab[f"supF{m}"]
        )
    assert np.all(tab["UDmax"] >= tab["supF1"] - 1e-9)
    # the DP-simulated supF(1) against the O(N) single-break formula on fresh draws at the same N
    rng = np.random.default_rng(99)
    f1 = np.concatenate([sup_f1(rng.standard_normal((1000, 2000)), 300) for _ in range(8)])
    for p in (0.90, 0.95):
        assert np.quantile(f1, p) == pytest.approx(np.interp(p, tab["prob"], tab["supF1"]), rel=0.04)


def test_bai_perron_finds_a_planted_break_and_rejects_none_on_noise():
    rng = np.random.default_rng(16)
    y = rng.normal(0, 0.01, 1500) + np.r_[np.zeros(900), np.full(600, 0.004)]
    out = bai_perron(y)
    assert out["n_breaks"] == 1
    lo, hi = out["intervals"][0]
    assert lo <= 900 <= hi and abs(out["breaks"][0] - 900) < 60
    assert out["segment_means"][1] - out["segment_means"][0] == pytest.approx(0.004, abs=0.0015)
    assert out["supF_p"][0] < 0.01
    flat = bai_perron(rng.normal(0, 0.01, 1500))
    assert flat["n_breaks"] == 0 and flat["breaks"] == [] and flat["sequential"][0]["p"] >= 0.05
    with pytest.raises(ValueError, match="null table"):
        bai_perron(y, eps=0.10)


def test_bai_perron_statistics_by_definition():
    rng = np.random.default_rng(17)
    y = rng.normal(0, 0.01, 1200) + np.r_[np.zeros(400), np.full(400, 0.004), np.full(400, -0.002)]
    out = bai_perron(y, bandwidth=21)
    fits = segment_dp(y, 5, out["h"])
    w2 = hac_bartlett((y - y.mean())[:, None], 21)[0, 0]  # ω̂² of the null (0-break) residuals
    for m in range(1, 6):  # supF(m) = (SSR₀ − SSR_m)/(m·ω̂²₀)
        assert out["supF"][m - 1] == pytest.approx((fits[0][0] - fits[m][0]) / (m * w2), rel=1e-12)
    assert out["sequential"][0]["supF"] == pytest.approx(out["supF"][0], rel=1e-12)  # one supF(1), not two
    tab = bp_null()
    for st in out["sequential"]:
        assert st["p"] == pytest.approx(
            max(out["p_floor"], 1 - _cdf_from_table(tab, "supF1", st["supF"]) ** (st["l"] + 1))
        )
    assert out["n_breaks"] == 2
    c = _date_quantile(0.975)
    assert out["date_c"] == pytest.approx(c)
    for (lo, hi), k, n in zip(out["intervals"], out["breaks"], range(2), strict=True):
        half = math.ceil(c * out["omega2"] / (out["segment_means"][n + 1] - out["segment_means"][n]) ** 2)
        assert (lo, hi) == (max(0, k - half - 1), min(1200, k + half + 1))
    with pytest.raises(ValueError, match="constant"):
        bai_perron(np.full(500, 3e-4))


def test_bai_perron_size_on_ar1_noise_small_scale():
    rej = 0
    for i in range(120):
        y = _ar1(600, 0.2, np.random.default_rng(2000 + i))
        rej += bai_perron(y)["n_breaks"] > 0
    assert rej / 120 <= 0.10


# ── Local level ───────────────────────────────────────────────────────────────────────────────────────────────────


def test_local_level_likelihood_and_filter_match_statsmodels():
    sm = pytest.importorskip("statsmodels.api")
    rng = np.random.default_rng(0)
    y = np.cumsum(rng.normal(0, 0.1, 500)) + rng.normal(0, 1, 500)
    r = local_level(y)
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        mod = sm.tsa.UnobservedComponents(y, "llevel")
        fit = mod.fit(disp=False)
        params = [r["sigma2_eps"], r["q"] * r["sigma2_eps"]]
        assert mod.loglike(params) == pytest.approx(r["loglik"], abs=1e-5)
        filt = mod.smooth(params).filtered_state[0]
    assert r["q"] == pytest.approx(fit.params[1] / fit.params[0], rel=1e-3)
    assert np.abs(filt - r["mu"]).max() < 1e-5
    assert r["q_zero"] is False and r["p_q0"] < 1e-6


def test_local_level_q_zero_is_the_recursive_mean_and_is_flagged():
    rng = np.random.default_rng(1)
    y = rng.normal(0.0003, 0.01, 2000)
    _, _, af, _ = _local_level_filter(y, np.array([0.0]))
    assert np.allclose(af[:, 0], np.cumsum(y) / np.arange(1, 2001), rtol=0, atol=1e-15)
    r = local_level(y)
    assert r["q_zero"] is True and r["p_q0"] >= 0.05
    assert r["profile_q"][0] == 0.0 and len(r["profile_ll"]) == len(r["profile_q"])
    # the q = 0 answer is the full-sample mean's posterior; it is returned beside the q̂ answer, which on this
    # seed differs by ≈ 0.17 although q̂ is not significant (why a report must print both)
    se = y.std() / np.sqrt(len(y))
    assert r["P_mu_T_pos_q0"] == pytest.approx(norm.cdf(y.mean() / se), abs=1e-3)
    assert abs(r["P_mu_T_pos"] - r["P_mu_T_pos_q0"]) > 0.1


def test_local_level_tracks_a_large_planted_shift():
    # mechanics: a −25 → +25 bp/day shift at σ 1 % is tracked well within 120 sessions (the 10 bp case is in the
    # checks script and in U04_status.md: its median lag exceeds 120)
    rng = np.random.default_rng(2)
    y = np.r_[np.full(1000, -0.0025), np.full(1000, 0.0025)] + rng.normal(0, 0.01, 2000)
    r = local_level(y)
    assert r["p_pos"][900:1000].max() < 0.5
    first = int(np.flatnonzero(r["p_pos"][1000:] > 0.5)[0])
    assert first < 120 and r["P_mu_T_pos"] > 0.95
    with pytest.raises(ValueError):
        local_level(np.ones(50))


def test_local_level_lr_mixture_and_grid_edge():
    rng = np.random.default_rng(3)
    y = np.cumsum(rng.normal(0, 0.05, 400)) + rng.normal(0, 1, 400)
    r = local_level(y)
    assert r["lr_q0"] > 0 and r["p_q0"] == pytest.approx(0.5 * chi2.sf(r["lr_q0"], 1), rel=1e-12)
    assert r["q_at_grid_edge"] is False
    capped = local_level(y, q_grid=np.logspace(-9, -6, 10))  # the MLE wants more than the grid allows
    assert capped["q_at_grid_edge"] is True and capped["q"] == pytest.approx(1e-6)


def test_local_level_track_is_causal_and_local_level_is_not():
    rng = np.random.default_rng(4)
    y = np.r_[np.full(600, -0.0005), np.full(600, 0.0005)] + rng.normal(0, 0.01, 1200)
    c = 700
    alt = y.copy()
    alt[c + 1 :] = np.cumsum(np.random.default_rng(5).normal(0, 0.02, 1200 - c - 1))  # random walk after c
    a, b = local_level_track(y, refit_every=63), local_level_track(alt, refit_every=63)
    trunc = local_level_track(y[: c + 1], refit_every=63)
    np.testing.assert_allclose(a["p_pos"][: c + 1], b["p_pos"][: c + 1], rtol=0, atol=1e-12)
    np.testing.assert_allclose(a["p_pos"][: c + 1], trunc["p_pos"], rtol=0, atol=1e-12)
    assert np.isnan(a["p_pos"][:252]).all() and np.isfinite(a["p_pos"][252:]).all()
    # mutant: the whole-sample fit is caught by the same perturbation
    assert np.abs(local_level(y)["p_pos"][: c + 1] - local_level(alt)["p_pos"][: c + 1]).max() > 1e-6
