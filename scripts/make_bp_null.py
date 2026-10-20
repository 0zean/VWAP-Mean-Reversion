"""Simulate the Bai–Perron null table `vwap_mr/stats/bp_null.csv` (SPEC.md §6, `vwap_mr/stats/breaks.py`).

    uv run python scripts/make_bp_null.py [--n 2000] [--reps 10000] [--seed 0]   (≈ 7 minutes)

Under H0 (no break, long-run variance 1) supF(m) → the functional (SSR₀ − SSR_m)/m of Brownian motion, minimised
over partitions with segments ≥ ε. The script evaluates it on iid N(0, 1) series of length N with known variance by
the same DP as `breaks.segment_dp`, writes 999 quantiles (0.001 … 0.999) of supF(1 … 5) and UDmax, and prints
the discretisation check: supF(1) quantiles at N vs at 5N by the O(N) single-break formula."""

import argparse
import time

import numpy as np

from vwap_mr.stats.breaks import BP_EPS, BP_MMAX, BP_NULL_PATH, _dp, sup_f1


def main(argv=None) -> None:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--n", type=int, default=2000)
    ap.add_argument("--reps", type=int, default=10_000)
    ap.add_argument("--seed", type=int, default=0)
    a = ap.parse_args(argv)
    rng = np.random.default_rng(a.seed)
    h = int(np.ceil(BP_EPS * a.n))
    t0 = time.time()
    stats = []
    for start in range(0, a.reps, 1000):
        Y = rng.standard_normal((min(1000, a.reps - start), a.n))
        ssr, _ = _dp(Y, BP_MMAX, h)
        stats.append((ssr[:, :1] - ssr[:, 1:]) / np.arange(1, BP_MMAX + 1))
    F = np.concatenate(stats)
    F = np.column_stack([F, F.max(axis=1)])
    prob = np.round(np.arange(1, 1000) / 1000, 3)
    q = np.quantile(F, prob, axis=0)
    cols = ["prob", *[f"supF{m}" for m in range(1, BP_MMAX + 1)], "UDmax"]
    header = ",".join(cols)
    np.savetxt(
        BP_NULL_PATH, np.column_stack([prob, q]), delimiter=",", header=header, comments="", fmt="%.6f"
    )
    print(
        f"wrote {BP_NULL_PATH} (N {a.n}, reps {a.reps}, seed {a.seed}, eps {BP_EPS}) in {time.time() - t0:.0f}s"
    )
    for p in (0.90, 0.95, 0.99):
        i = round(p * 1000) - 1
        print(f"  {p:.2f}: " + "  ".join(f"{c} {v:.3f}" for c, v in zip(cols[1:], q[i], strict=True)))
    big = sup_f1(
        np.random.default_rng(a.seed + 1).standard_normal((a.reps, 5 * a.n)), int(np.ceil(BP_EPS * 5 * a.n))
    )
    small = sup_f1(np.random.default_rng(a.seed + 2).standard_normal((a.reps, a.n)), h)
    for p in (0.90, 0.95, 0.99):
        print(f"  discretisation supF1 q{p:.2f}: N {np.quantile(small, p):.3f}  5N {np.quantile(big, p):.3f}")


if __name__ == "__main__":
    main()
