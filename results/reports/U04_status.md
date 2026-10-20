# U4 status — statistics toolkit; SPEC.md §6

Date 2026-10-20. Branch `unit/04-stats` (off `dev` @ 4ada8a7), tip c043a47 (f8a1639 code, 9db9873 review fixes,
c043a47 a label). Windows 11, uv, Python 3.12. Raw output: `results/reports/U04_checks.md`. Formulas and
references: `SPEC.md` §6 (on `dev`).

## What shipped

- `stats/sharpe.py`: `sharpe_ratio`, `return_moments`, `psr`, `psr_of`, `min_track_record_length` (per-period SR,
  raw kurtosis).
- `stats/bootstrap.py`:
  - `circular_block_indices`, `hac_bartlett` (fractional bandwidth allowed), `andrews_bandwidth`, `newey_west_ols`.
  - The three studentized block-bootstrap tests: `mean_test` (H1), `cond_mean_test` (H3) and `sharpe_test`
    (Ledoit–Wolf, H4).
  - Every test returns `n_invalid`.
- `stats/multiplicity.py`: `holm`, `bonferroni`. `bonferroni` moved here from `experiments/looks.py`, which
  re-exports it.
- `stats/power.py`: `mde_mean`, `mde_cond_diff`, `mde_delta_sharpe`, `garch_t`, `simulate_rejection`.
- `stats/breaks.py`:
  - `segment_dp` (exact DP) and `bai_perron` (supF(1…5), UDmax, sequential count, Bai 1997 date intervals).
  - `local_level` (MLE, profile, q → 0 LR test; fits on the whole sample, so it is the answer at T).
  - `local_level_track` (the causal form, refitted on the past).
- `stats/bp_null.csv`: simulated null table, from `scripts/make_bp_null.py`.
- Ported from wavelet-meta-model (fetched via `gh api`): `circular_block_indices`, `hac_bartlett`, `mean_test`,
  `sharpe_test`, `_boot_pvalue`, `holm`, `mde_alpha` → `mde_mean`, `mde_delta_sharpe`, `garch_t`, and `psr` /
  `min_track_record_length`.
- Written new: `cond_mean_test`, `mde_cond_diff`, `breaks.py`, the null table.
- Not ported: DSR (now a test helper only), James–Stein, PBO.

Decisions taken inside the unit (reported here, documented in SPEC §6):

- σ_d (daily volatility) parameterises power, as in PLAN §6.6. The sibling used annualized vol.
- Simulations use n_boot 999. Registered runs use 5,000.
- The Newey–West cross-check t uses 5 lags, as in the sibling. The bootstrap's HAC uses the block, 21 (≡ 20 lags).
- **Bai–Perron normalises every F by ω̂² of its null model's residuals (LM form), with AR(1)-prewhitened Andrews
  bandwidth.**
  - The first version used the alternative's residuals. That was oversized: 0.059 / 0.072 at φ 0 / 0.2 on 2,000
    sims.
  - It also used two different supF(1)s (reviewer SEVERE 1–2).
  - Prewhitened Andrews beat bandwidth 21 by a rule fixed before the comparison: worst-case size 0.064 vs 0.0645.
- The Bai–Perron null comes from our own simulation, not from tables copied from memory.
  - With known variance it is exact: size 0.0498 at T 2,500 on 20,000 draws.
  - Its 95 % values (8.63 / 7.14 / 5.95 / 4.96 / 3.87, UDmax 8.88) and its implied sequential critical values agree
    with Bai–Perron (2003) to ≤ 1 %. The reviewer did that comparison; this session could not verify the paper's
    table online.
- `local_level` returns P(μ_T > 0) both at q̂ and at q = 0. On noise, an insignificant q̂ > 0 moved it by 0.17
  (tested).

## Done-when

- [x] **Bailey & López de Prado test vector.**
  - The DSR example reproduces through `psr`: SR₀ 0.1132, PSR(SR₀) 0.9004
    (`test_dsr_example_reproduced_through_psr`).
  - PSR and MinTRL invert each other to 1e-12 (`test_min_track_record_length_and_psr_invert_each_other`), which also
    reproduces PLAN §6.6's MinTRL of 10.8 / 2.7 years.
  - Holm matches the hand example [0.01, 0.04, 0.03, 0.005] → [0.03, 0.06, 0.06, 0.02] (`test_holm_hand_example`).
- [~] **Size and power: met within Monte Carlo error; one sub-item sits on its bar.** The ± 0.01 band at 1,000 sims is
  ± 1.4 MC se, so a correct test misses it about 16 % of the time. The first run did miss it, so more runs were
  added with seeds declared first (`U04_checks.md` §2):

  | criterion | registered run (1,000) | replications | pooled | |
  |---|---|---|---|---|
  | mean size iid, 0.05 ± 0.01 | 0.064 | 0.057; 0.042 (3,000) | **0.0496** (5,000) | met |
  | mean size GARCH-t | 0.056 | 0.071; 0.050 (3,000) | **0.0556** (5,000) | met |
  | power at MDE ≥ 0.78, iid | 0.795 | 0.786 | 0.791 | met |
  | power at MDE ≥ 0.78, GARCH-t | 0.771 | 0.787 | **0.779** | at the bar (MC se 0.009) |
  | cond. diff size ≤ 0.06, coin π 0.4 | iid 0.050, GARCH-t 0.049 | Markov gate (episodes 21) on GARCH-t 0.049 | — | met |
  | lag-1 autocorrelation within 10 % | ratio 0.950 / 0.950 / 0.952 at φ 0.1 / 0.3 / 0.5 | — | — | met (the ≈ 1/21 loss at block joins) |

  Other measured rates:
  - Power of the conditional test at its MDE: 0.806.
  - Size of the Ledoit–Wolf paired test at ρ 0.9: 0.060 (300 sims).
  - A diagnostic on 600 sims shows the mean test is calibrated: t has sd 1.005 and the mean bootstrap 95 % critical
    value is 1.66.

  Tests at reduced scale: `test_mean_test_size_and_power_small_scale`,
  `test_cond_mean_test_size_independent_coin_small_scale`,
  `test_block_bootstrap_preserves_lag1_autocorrelation_within_10pct` (full scale).
- [ ] **Kalman tracker and Bai–Perron: two of three sub-items met.**
  - **Met: q → 0 detected and reported.**
    - The LR test of q = 0 (½χ² mixture) flags `q_zero` on 0.970 of 200 iid series (nominal 0.95).
    - P(μ_T > 0) is returned at q̂ and at q = 0.
    - Tests: `test_local_level_q_zero_is_the_recursive_mean_and_is_flagged`,
      `test_local_level_lr_mixture_and_grid_edge`.
    - The likelihood and filter match statsmodels: `test_local_level_likelihood_and_filter_match_statsmodels`.
  - **Met, on the pooled estimate: Bai–Perron size ≤ 0.06 on stationary AR(1).**
    - Any break selected at 5 %, n 2,500: φ 0: 0.053 (2,000 sims) and 0.064 (1,000), pooled **0.057**.
    - φ 0.2: 0.064 and 0.050, pooled **0.059**.
    - UDmax p-values: 0.051 / 0.059 and 0.063 / 0.045.
  - **Not met: P(μ > 0) crosses 0.5 within 120 sessions of a planted 10 bp/day shift at σ 1 %.**
    - Scenario: −5 → +5 bp/day, n 2,500, shift at 1,250.
    - At the whole-sample MLE q (not causal, so a lower bound): median lag **226** (quartiles 101 / 460), 0.305
      within 120.
    - On the causal track: median **262**, 0.27 within 120.
    - A fixed q ≥ 3e-5 gets the median under 120 (74 at 3e-5, 28 at 1e-4). The price is that P > 0.5 on 18–25 % of
      the 250 pre-shift sessions, while μ is still −5 bp.
    - The criterion cannot be met without a high false-crossing rate. A 10 bp change at σ 1 % takes (σ/δ)² = 100
      sessions to reach a one-standard-error separation. The MLE also cannot see a q this small: q̂ ≈ 5e-6, with
      `q_zero` in 52 % of runs.
    - Proposal in PLAN §11. The code is not tuned to pass.

## Looks

None. Everything is synthetic; no bar was read. K was 1, is 1.

## Adversarial review: BREAKING 0 / SEVERE 3 / MINOR 6+ (all SEVERE fixed; MINOR fixed except as noted)

| # | finding | disposition |
|---|---|---|
| S1 | supF / UDmax p oversized on AR(1); selection borderline | fixed: null-model ω̂² + prewhitened Andrews; size above |
| S2 | sequential step 0 and the reported supF(1) used different ω̂² | fixed: one definition; `test_bai_perron_statistics_by_definition` asserts step 0 = supF(1) |
| S3 | `local_level`'s per-t P(μ_t > 0) not causal (q̂ on all of y) | fixed: `local_level_track` (perturbation + truncation test, the whole-sample fit is the caught mutant); `local_level` documented as "the answer at T"; the crossing study reports both |
| M1 | 14 of 38 mutants survived | fixed except one: tests for one-sided bound coverage, `n_invalid` counting and denominator, sequential 1 − G^{l+1}, supF/m, date quantile and half-width, LR ½, grid edge, fractional HAC. **Survives:** roots divided by se instead of s* (an unstudentized bootstrap) — asymptotically equivalent under the null; no cheap test separates them; deferred |
| M2 | date-interval coverage below 95 % for small shifts (0.85 at 20 bp) | documented (module docstring, SPEC §6.5: report as "nominal 95 %"); Bai's interval's known weakness, not a bug |
| M3 | stale N = 400 docs | fixed |
| M4 | constant input → bare ZeroDivisionError | fixed: raises with a message (tested) |
| M5 | statsmodels agreement only in the high-q regime | SPEC §6.5 qualified (statsmodels' steady-state tolerance; with tolerance 0 the codes agree to ≈ 5e-9) |

Gate: `powershell -NoProfile -File scripts/done_gate.ps1` → 118 tests pass (32 in `tests/test_stats.py`), ruff
check/format clean, `ty` clean (no ignores). Package size 3,808 lines in `vwap_mr/` including docstrings (target
≤ 5,000 non-test lines). Test-suite time 22 s.
