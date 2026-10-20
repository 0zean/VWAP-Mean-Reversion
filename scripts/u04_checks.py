"""U4 done-when demonstrations (PLAN.md §7 U4). **Synthetic data only**: no bar, no strategy, no market statistic is
read. Every seed is fixed below, before the first run.

    uv run python scripts/u04_checks.py [--sims 1000]

1. Test vectors: the Bailey–López de Prado DSR example through `psr`, PSR ↔ MinTRL, Holm by hand.
2. Mean test size at n 2,500 over iid normal and GARCH(1,1)-t streams (σ_d 1 %, n_boot 999).
3. Mean test power at the analytic MDE, iid and GARCH-t.
4. Conditional-mean-difference size with an independent coin gate π 0.4 (iid, GARCH-t), and with a persistent
   Markov gate (mean episode 21 sessions) on GARCH-t; power at its analytic MDE (iid).
5. Block bootstrap (block 21): mean lag-1 autocorrelation of resamples vs the sample's, AR(1) φ 0.1 / 0.3 / 0.5.
6. Ledoit–Wolf paired Sharpe test size at ρ 0.9.
7. Local-level tracker: q → 0 detection on iid noise; the planted 10 bp/day shift (−5 → +5 bp at σ 1 %, n 2,500,
   shift at 1,250): first session after the shift with P(μ_t > 0) > 0.5, at the whole-sample MLE q (not causal:
   a lower bound), at fixed q, and on the causal track (`local_level_track`, refit every 63).
8. Bai–Perron size (any break selected at 5 %) on AR(1) noise, φ 0 and 0.2, n 2,500.
"""

import argparse
import time

import numpy as np
from scipy.stats import norm

from vwap_mr.stats.bootstrap import circular_block_indices, cond_mean_test, sharpe_test
from vwap_mr.stats.breaks import _local_level_filter, bai_perron, local_level, local_level_track
from vwap_mr.stats.multiplicity import holm
from vwap_mr.stats.power import garch_t, mde_cond_diff, mde_mean, simulate_rejection
from vwap_mr.stats.sharpe import min_track_record_length, psr

N, SIGMA = 2500, 0.01


def line(label, out):
    print(
        f"  {label:<44} rejection {out['rejection_rate']:.3f} (MC se {out['mc_se']:.3f}, {out['n_sims']} sims)"
    )


def ar1(n, phi, rng):
    e = rng.normal(0, SIGMA, n + 200)
    x = np.empty_like(e)
    x[0] = e[0]
    for t in range(1, len(e)):
        x[t] = phi * x[t - 1] + e[t]
    return x[200:]


def markov_gate(n, pi, mean_on, rng):
    p_off = 1 / mean_on
    p_on = pi / (1 - pi) * p_off
    g = np.empty(n, dtype=bool)
    g[0] = rng.random() < pi
    u = rng.random(n)
    for t in range(1, n):
        g[t] = (u[t] >= p_off) if g[t - 1] else (u[t] < p_on)
    return g


def main(argv=None) -> None:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--sims", type=int, default=1000)
    a = ap.parse_args(argv)
    S = a.sims
    t0 = time.time()

    print("1. test vectors")
    sr, var = 2.5 / np.sqrt(250), 0.5 / 250
    g = 0.5772156649015329
    sr0 = np.sqrt(var) * ((1 - g) * norm.ppf(1 - 1 / 100) + g * norm.ppf(1 - 1 / (100 * np.e)))
    print(
        f"  DSR example: SR0 {sr0:.4f} (paper 0.1132), PSR(SR0) {psr(sr, 1250, -3, 10, sr0):.4f} (paper 0.9004)"
    )
    n = min_track_record_length(0.1, 0.0, -1.0, 6.0, 0.05)
    print(f"  MinTRL(0.1, skew -1, kurt 6) {n:.3f}; PSR at it (continuous) {_psr_cont(n):.12f}")
    print(f"  Holm([0.01, 0.04, 0.03, 0.005]) = {np.round(holm([0.01, 0.04, 0.03, 0.005]), 6).tolist()}")

    print(f"2. mean test size, n {N}, σ_d {SIGMA}, block 21, n_boot 999")
    for stream, seed in (("iid", 101), ("garch_t", 102)):
        line(
            f"size {stream}",
            simulate_rejection("mean", N, SIGMA, n_sims=S, stream=stream, seed=seed * 10_000),
        )

    m = mde_mean(N, SIGMA)
    print(f"3. mean test power at the analytic MDE {m['mde_bp_day']:.3f} bp/day")
    for stream, seed in (("iid", 103), ("garch_t", 104)):
        out = simulate_rejection("mean", N, SIGMA, m["mde"], n_sims=S, stream=stream, seed=seed * 10_000)
        line(f"power {stream}", out)

    print("4. conditional mean difference, π 0.4")
    for stream, seed in (("iid", 105), ("garch_t", 106)):
        out = simulate_rejection(
            "cond_mean_diff", N, SIGMA, n_sims=S, stream=stream, pi=0.4, seed=seed * 10_000
        )
        line(f"size, independent coin, {stream}", out)
    rng = np.random.default_rng(107)
    r = garch_t(N, SIGMA, rng, size=S)
    p = np.array(
        [cond_mean_test(r[i], markov_gate(N, 0.4, 21, rng), n_boot=999, seed=i)["p"] for i in range(S)]
    )
    rate = float((p < 0.05).mean())
    print(f"  {'size, Markov gate (episodes 21), garch_t':<44} rejection {rate:.3f} "
          f"(MC se {np.sqrt(rate * (1 - rate) / S):.3f}, {S} sims)")  # fmt: skip
    c = mde_cond_diff(N, SIGMA, 0.4)
    out = simulate_rejection("cond_mean_diff", N, SIGMA, c["mde"], n_sims=S, pi=0.4, seed=108 * 10_000)
    line(f"power at MDE {c['mde_bp_day']:.2f} bp/day, iid", out)

    print("5. block bootstrap lag-1 autocorrelation (block 21, 2,000 resamples)")
    for phi, seed in ((0.1, 109), (0.3, 110), (0.5, 111)):
        x = ar1(N, phi, np.random.default_rng(seed))
        xs = x[circular_block_indices(N, 21, 2000, np.random.default_rng(seed + 1000))]

        def ac1(z):
            z = z - z.mean(axis=-1, keepdims=True)
            return (z[..., 1:] * z[..., :-1]).sum(-1) / (z * z).sum(-1)

        print(
            f"  φ {phi}: sample {ac1(x):.4f}, resamples {ac1(xs).mean():.4f}, ratio {ac1(xs).mean() / ac1(x):.4f}"
        )

    print("6. Ledoit–Wolf paired Sharpe test size, ρ 0.9, n 2,500, n_boot 999")
    S6 = min(S, 300)
    rej = 0
    for i in range(S6):
        rng = np.random.default_rng(112_000 + i)
        common = rng.normal(0, SIGMA, N)
        x, y = (0.9 * common + np.sqrt(0.19) * rng.normal(0, SIGMA, N) for _ in range(2))
        rej += sharpe_test(x, y, n_boot=999, seed=i)["p"] < 0.05
    print(f"  size {rej / S6:.3f} (MC se {np.sqrt(0.05 * 0.95 / S6):.3f}, {S6} sims)")

    print("7. local-level tracker")
    S7 = min(S, 200)
    qz = [local_level(np.random.default_rng(113_000 + i).normal(0, SIGMA, N))["q_zero"] for i in range(S7)]
    print(f"  iid noise: q_zero flagged in {np.mean(qz):.3f} of {S7} (nominal 0.95)")
    mu = np.r_[np.full(N // 2, -5e-4), np.full(N - N // 2, 5e-4)]
    lags, qs, qzs, pre = [], [], [], []
    fixed = np.array([1e-6, 1e-5, 3e-5, 1e-4, 3e-4, 1e-3])
    flags, fpre = [], []
    for i in range(S7):
        y = mu + np.random.default_rng(114_000 + i).normal(0, SIGMA, N)
        o = local_level(y)
        after = np.flatnonzero(o["p_pos"][N // 2 :] > 0.5)
        lags.append(after[0] if len(after) else np.inf)
        qs.append(o["q"])
        qzs.append(o["q_zero"])
        pre.append(np.mean(o["p_pos"][N // 2 - 250 : N // 2] > 0.5))
        _, s2, af, pf = _local_level_filter(y, fixed)
        pp = norm.cdf(af / np.sqrt(s2 * pf))
        flags.append(
            [(np.flatnonzero(pp[N // 2 :, j] > 0.5)[:1].tolist() or [np.inf])[0] for j in range(len(fixed))]
        )
        fpre.append((pp[N // 2 - 250 : N // 2] > 0.5).mean(axis=0))
    lags = np.array(lags, dtype=float)
    print(
        f"  shift −5 → +5 bp/day, σ 1 %, {S7} sims, MLE q: median q̂ {np.median(qs):.2e}, q_zero {np.mean(qzs):.3f}"
    )
    print(f"    crossing lag: median {np.median(lags):.0f}, quartiles {np.percentile(lags, 25):.0f} / "
          f"{np.percentile(lags, 75):.0f}, share ≤ 120: {np.mean(lags <= 120):.3f}; "
          f"pre-shift sessions with P > 0.5 (last 250): {np.mean(pre):.3f}")  # fmt: skip
    flags, fpre = np.array(flags, dtype=float), np.array(fpre)
    for j, q in enumerate(fixed):
        print(f"    fixed q {q:.0e}: memory ≈ {1 / np.sqrt(q):5.0f} sessions; lag median {np.median(flags[:, j]):4.0f}, "
              f"share ≤ 120 {np.mean(flags[:, j] <= 120):.3f}; pre-shift P > 0.5 {fpre[:, j].mean():.3f}")  # fmt: skip
    S7c = min(S, 100)
    lags_c, pre_c = [], []
    for i in range(S7c):  # the causal track: q̂ refitted on the past every 63 sessions
        y = mu + np.random.default_rng(114_000 + i).normal(0, SIGMA, N)
        tr = local_level_track(y, refit_every=63)
        after = np.flatnonzero(tr["p_pos"][N // 2 :] > 0.5)
        lags_c.append(after[0] if len(after) else np.inf)
        pre_c.append(np.mean(tr["p_pos"][N // 2 - 250 : N // 2] > 0.5))
    lags_c = np.array(lags_c, dtype=float)
    print(f"    causal track (refit every 63, first {S7c} of the same series): lag median {np.median(lags_c):.0f}, "
          f"quartiles {np.percentile(lags_c, 25):.0f} / {np.percentile(lags_c, 75):.0f}, share ≤ 120 "
          f"{np.mean(lags_c <= 120):.3f}; pre-shift P > 0.5 {np.mean(pre_c):.3f}")  # fmt: skip

    print(f"8. Bai–Perron size, n {N}, ε 0.15, m_max 5, prewhitened Andrews HAC, sequential 5 %")
    for phi, seed in ((0.0, 115), (0.2, 116)):
        sel, f1, ud = 0, 0, 0
        for i in range(S):
            out = bai_perron(ar1(N, phi, np.random.default_rng(seed * 10_000 + i)))
            sel += out["n_breaks"] > 0
            f1 += out["supF_p"][0] < 0.05
            ud += out["UDmax_p"] < 0.05
        print(f"  AR(1) φ {phi}: any break selected {sel / S:.3f}; supF(1) p < 0.05 {f1 / S:.3f}; UDmax p < 0.05 {ud / S:.3f} "
              f"(MC se {np.sqrt(0.05 * 0.95 / S):.3f}, {S} sims)")  # fmt: skip
    print(f"done in {time.time() - t0:.0f}s")


def _psr_cont(n: float) -> float:
    sr, sk, ku = 0.1, -1.0, 6.0
    return float(norm.cdf(sr * np.sqrt(n - 1) / np.sqrt(1 - sk * sr + (ku - 1) / 4 * sr**2)))


if __name__ == "__main__":
    main()
