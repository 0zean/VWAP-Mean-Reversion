# U4 checks — raw output

Synthetic data only; no bar, strategy or market statistic is read. Seeds are fixed in the scripts before each run.

## 1. `PYTHONIOENCODING=utf-8 uv run python scripts/u04_checks.py` on `unit/04-stats` @ 9db9873 (2026-10-20, 774 s)

Sections 1–6 are byte-identical to the first run on f8a1639; sections 7–8 reflect the review fixes. Section 8's header said "HAC bandwidth Andrews AR(1)"; the bandwidth was the AR(1)-prewhitened Andrews default (label corrected in c043a47).

```
1. test vectors
  DSR example: SR0 0.1132 (paper 0.1132), PSR(SR0) 0.9004 (paper 0.9004)
  MinTRL(0.1, skew -1, kurt 6) 301.992; PSR at it (continuous) 0.950000000000
  Holm([0.01, 0.04, 0.03, 0.005]) = [0.03, 0.06, 0.06, 0.02]
2. mean test size, n 2500, σ_d 0.01, block 21, n_boot 999
  size iid                                     rejection 0.064 (MC se 0.008, 1000 sims)
  size garch_t                                 rejection 0.056 (MC se 0.007, 1000 sims)
3. mean test power at the analytic MDE 4.973 bp/day
  power iid                                    rejection 0.795 (MC se 0.013, 1000 sims)
  power garch_t                                rejection 0.771 (MC se 0.013, 1000 sims)
4. conditional mean difference, π 0.4
  size, independent coin, iid                  rejection 0.050 (MC se 0.007, 1000 sims)
  size, independent coin, garch_t              rejection 0.049 (MC se 0.007, 1000 sims)
  size, Markov gate (episodes 21), garch_t     rejection 0.049 (MC se 0.007, 1000 sims)
  power at MDE 10.15 bp/day, iid               rejection 0.806 (MC se 0.013, 1000 sims)
5. block bootstrap lag-1 autocorrelation (block 21, 2,000 resamples)
  φ 0.1: sample 0.1164, resamples 0.1106, ratio 0.9503
  φ 0.3: sample 0.2716, resamples 0.2581, ratio 0.9504
  φ 0.5: sample 0.5115, resamples 0.4868, ratio 0.9517
6. Ledoit–Wolf paired Sharpe test size, ρ 0.9, n 2,500, n_boot 999
  size 0.060 (MC se 0.013, 300 sims)
7. local-level tracker
  iid noise: q_zero flagged in 0.970 of 200 (nominal 0.95)
  shift −5 → +5 bp/day, σ 1 %, 200 sims, MLE q: median q̂ 5.00e-06, q_zero 0.520
    crossing lag: median 226, quartiles 101 / 460, share ≤ 120: 0.305; pre-shift sessions with P > 0.5 (last 250): 0.076
    fixed q 1e-06: memory ≈  1000 sessions; lag median  516, share ≤ 120 0.095; pre-shift P > 0.5 0.051
    fixed q 1e-05: memory ≈   316 sessions; lag median  129, share ≤ 120 0.475; pre-shift P > 0.5 0.117
    fixed q 3e-05: memory ≈   183 sessions; lag median   74, share ≤ 120 0.675; pre-shift P > 0.5 0.183
    fixed q 1e-04: memory ≈   100 sessions; lag median   28, share ≤ 120 0.860; pre-shift P > 0.5 0.246
    fixed q 3e-04: memory ≈    58 sessions; lag median   13, share ≤ 120 0.945; pre-shift P > 0.5 0.302
    fixed q 1e-03: memory ≈    32 sessions; lag median    8, share ≤ 120 0.990; pre-shift P > 0.5 0.354
    causal track (refit every 63, first 100 of the same series): lag median 262, quartiles 105 / 451, share ≤ 120 0.270; pre-shift P > 0.5 0.076
8. Bai–Perron size, n 2500, ε 0.15, m_max 5, HAC bandwidth Andrews AR(1), sequential at 5 %
  AR(1) φ 0.0: any break selected 0.064; supF(1) p < 0.05 0.064; UDmax p < 0.05 0.063 (MC se 0.007, 1000 sims)
  AR(1) φ 0.2: any break selected 0.050; supF(1) p < 0.05 0.050; UDmax p < 0.05 0.045 (MC se 0.007, 1000 sims)
done in 774s
```

## 2. Replications of the mean test (seeds declared before each run)

1,000 sims each, seeds 201–204 × 10⁴ (`simulate_rejection`, n 2,500, σ_d 1 %, n_boot 999):

```
replication size iid: 0.057 (MC se 0.007)
replication size garch_t: 0.071 (MC se 0.008)
replication power iid: 0.786 (MC se 0.013)
replication power garch_t: 0.787 (MC se 0.013)
size iid 3000 sims: 0.0423 (MC se 0.0037)
size garch_t 3000 sims: 0.0503 (MC se 0.0040)
```

Pooled size over the three runs (5,000 sims each): iid (64 + 57 + 127)/5,000 = **0.0496**; GARCH-t (56 + 71 + 151)/5,000 = **0.0556**. Pooled power at the MDE (2,000 each): iid 0.791, GARCH-t **0.779**.

## 3. Bai–Perron size, before and after the review fix (2,000 sims each, `bai_perron`, n 2,500)

```
known-variance supF1 at T 2500, 20000 draws: q90 7.108 q95 8.621 q99 12.469; table 7.123 8.631 12.260; size at table 5%: 0.0498
--- before: ω̂² from the alternative model's residuals, Andrews bandwidth (seeds 5.0e6 / 5.1e6)
andrews, AR(1) phi 0.0, 2000 sims: selected 0.0585  supF1 0.0585  UDmax 0.0605  (MC se 0.0049)
andrews, AR(1) phi 0.2, 2000 sims: selected 0.0720  supF1 0.0720  UDmax 0.0735  (MC se 0.0049)
--- after: ω̂² from the null model's residuals (seeds 6.0e6 / 6.1e6)
null-normalised, bw 21, phi 0.0: selected 0.0455 UDmax 0.0430 (MC se 0.0049)
null-normalised, bw 21, phi 0.2: selected 0.0645 UDmax 0.0625 (MC se 0.0049)
null-normalised, bw andrews_pw, phi 0.0: selected 0.0530 UDmax 0.0510 (MC se 0.0049)
null-normalised, bw andrews_pw, phi 0.2: selected 0.0640 UDmax 0.0590 (MC se 0.0049)
```

Prewhitened Andrews was chosen over bandwidth 21 by the rule fixed before this run (the smaller worst-case size). Pooled with section 1's 1,000 sims: φ 0 **0.057**, φ 0.2 **0.059**.

## 4. Bai–Perron null table: `uv run python scripts/make_bp_null.py` (N 2,000, 10,000 draws, seed 0, 421 s)

```
wrote C:\Users\Nick\Desktop\Code\Python Projects\Trading Strategies\VWAP-Mean-Reversion\vwap_mr\stats\bp_null.csv (N 2000, reps 10000, seed 0, eps 0.15) in 421s
  0.90: supF1 7.123  supF2 6.225  supF3 5.260  supF4 4.437  supF5 3.462  UDmax 7.479
  0.95: supF1 8.631  supF2 7.140  supF3 5.947  supF4 4.959  supF5 3.865  UDmax 8.877
  0.99: supF1 12.260  supF2 9.174  supF3 7.471  supF4 6.174  supF5 4.845  UDmax 12.331
  discretisation supF1 q0.90: N 7.232  5N 7.231
  discretisation supF1 q0.95: N 8.753  5N 8.786
  discretisation supF1 q0.99: N 12.269  5N 12.257
```
