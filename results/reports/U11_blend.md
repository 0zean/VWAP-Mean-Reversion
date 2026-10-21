# H4_blend — H4 regime-weighted blend vs static blend (PLAN.md §6.5)

Spec `specs/H4_blend.yaml` @ `798ce8721864`; code `798ce8721864` (hash `fb78f8da8730`); data vintage `2026-10` (`a2efbd9fbba4`). Window 2016-01-04 → 2025-12-31, SPY, 1.0 bp, close → next_open. Look `U11 run H4_blend@798ce8721864` (n 12, 2026-10-21T07:36:17.110699+00:00). **K = 182** (every window); p·K beside every p.

**Registered as a diagnostic** (§11, 2026-10-21): the weight's state, B1, was chosen after its H3 Δμ was read on this window; the expected ΔSharpe, 0.08, is below the MDE 0.352 (ρ 0.9, n 2513). A pass cannot confirm anything; only the holdout (U12) and the forward test (U13) can.

## 1. Headline (one-sided H₁: ΔSharpe > 0, blend vs static 50/50)

| configuration | P | κ | TF | σ* | Sharpe blend | Sharpe static | ΔSharpe (LW) | 90 % lower | p one-sided (p·K) | ρ | %/yr blend / static | floor | passes |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| B1 | B1 | 0.5 | TSMOM | — | 0.39 | 0.25 | +0.15 | -0.04 | 0.1502 (1.000) | 0.844 | +4.03 / +2.01 | True | False |

**Verdict: null (diagnostic)** — pass: p < 0.05 and the blend's net return ≥ the static's − 0.1 × |static's|. w_t: mean 0.449, shares {'0.25': 0.601829753381066, '0.75': 0.39817024661893397}. ΔSharpe is annualized; the 90 % lower bound is one-sided.

## 2. Variants (each vs its own static blend: the same TF and vol target, w = 0.5)

| configuration | P | κ | TF | σ* | Sharpe blend | Sharpe static | ΔSharpe (LW) | 90 % lower | p one-sided (p·K) | ρ | %/yr blend / static | floor | undefined P (w = registered) |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| R2 | R2 | 0.5 | TSMOM | — | 0.41 | 0.25 | +0.17 | +0.00 | 0.0962 (1.000) | 0.876 | +3.91 / +2.01 | True | 0 |
| B1_percentile | B1_pct | 0.5 | TSMOM | — | 0.37 | 0.25 | +0.12 | -0.02 | 0.1466 (1.000) | 0.883 | +3.52 / +2.01 | True | 0 |
| B6_diff | B6_diff | 0.5 | TSMOM | — | 0.07 | 0.25 | -0.17 | -0.37 | 0.8772 (1.000) | 0.850 | +0.76 / +2.01 | False | 62 |
| hard_switch | B1 | 1.0 | TSMOM | — | 0.41 | 0.25 | +0.16 | -0.11 | 0.2156 (1.000) | 0.641 | +5.91 / +2.01 | True | 0 |
| kappa_025 | B1 | 0.25 | TSMOM | — | 0.35 | 0.25 | +0.10 | -0.00 | 0.1070 (1.000) | 0.950 | +3.06 / +2.01 | True | 0 |
| kappa_075 | B1 | 0.75 | TSMOM | — | 0.41 | 0.25 | +0.16 | -0.08 | 0.1878 (1.000) | 0.734 | +4.98 / +2.01 | True | 0 |
| vol_target_10 | B1 | 0.5 | TSMOM | 0.1 | 0.52 | 0.39 | +0.12 | -0.05 | 0.1748 (1.000) | 0.885 | +3.20 / +1.94 | True | 0 |
| donchian55 | B1 | 0.5 | DC55 | — | 0.34 | 0.15 | +0.19 | -0.00 | 0.1012 (1.000) | 0.832 | +3.89 / +1.52 | True | 0 |

## 3. Matched volatility and drawdown

| configuration | vol %/yr blend / static | %/yr blend at static vol | %/yr static | max DD blend / static | DD difference |
|---|---|---|---|---|---|
| B1 | 10.2 / 8.1 | +3.18 | +2.01 | 17.9 / 16.7 | +1.2 |
| R2 | 9.4 / 8.1 | +3.34 | +2.01 | 17.2 / 16.7 | +0.5 |
| B1_percentile | 9.6 / 8.1 | +2.95 | +2.01 | 17.1 / 16.7 | +0.4 |
| B6_diff | 10.1 / 8.1 | +0.60 | +2.01 | 20.4 / 16.7 | +3.7 |
| hard_switch | 14.3 / 8.1 | +3.33 | +2.01 | 24.8 / 16.7 | +8.1 |
| kappa_025 | 8.8 / 8.1 | +2.81 | +2.01 | 16.2 / 16.7 | -0.5 |
| kappa_075 | 12.1 / 8.1 | +3.31 | +2.01 | 20.8 / 16.7 | +4.1 |
| vol_target_10 | 6.2 / 4.9 | +2.55 | +1.94 | 11.8 / 11.0 | +0.8 |
| donchian55 | 11.3 / 10.0 | +3.46 | +1.52 | 20.4 / 17.6 | +2.9 |

## 3b. Timing vs static tilt: each blend vs its matched-mean static (reported, not the test)

| configuration | matched w | from | Sharpe matched | ΔSharpe vs matched (LW) | p one-sided (p·K) | net %/yr blend / 50-50 static |
|---|---|---|---|---|---|---|
| B1 | 0.450 | registered π | 0.26 | +0.13 | 0.1864 (1.000) | +6.24 / +4.22 |
| R2 | 0.477 | realized mean w | 0.26 | +0.16 | 0.1164 (1.000) | +6.12 / +4.22 |
| B1_percentile | 0.493 | realized mean w | 0.25 | +0.11 | 0.1560 (1.000) | +5.73 / +4.22 |
| B6_diff | 0.434 | realized mean w | 0.26 | -0.19 | 0.8880 (1.000) | +2.97 / +4.22 |
| hard_switch | 0.400 | registered π | 0.27 | +0.15 | 0.2671 (1.000) | +8.12 / +4.22 |
| kappa_025 | 0.475 | registered π | 0.26 | +0.09 | 0.1378 (1.000) | +5.27 / +4.22 |
| kappa_075 | 0.425 | registered π | 0.26 | +0.15 | 0.2346 (1.000) | +7.19 / +4.22 |
| vol_target_10 | 0.450 | registered π | 0.43 | +0.08 | 0.2486 (1.000) | +5.41 / +4.15 |
| donchian55 | 0.450 | registered π | 0.15 | +0.20 | 0.1158 (1.000) | +6.10 / +3.74 |

The 50/50 static is the registered benchmark (§6.5), but E[w] = 0.5 + κ(π − 0.5) ≠ 0.5, so the headline ΔSharpe also holds a constant reallocation (0.5 − E[w])(μ_TF − μ_MR); the matched-mean static removes it (w fixed at the registered-π mean for B1 weights, at the realized mean otherwise: then it is descriptive). A varying w also raises the blend's volatility (E[w² + (1 − w)²] > 0.5), which lowers its Sharpe against either static.

## 4. Benchmarks and components alone (no test; references)

| stream | bp/day | %/yr | vol %/yr | Sharpe | max DD % | round trips/yr | flat share | cost bp/day |
|---|---|---|---|---|---|---|---|---|
| R0 | -0.23 | -0.58 | 13.8 | -0.04 | 31.3 | 66.8 | 0.020 | 0.31 |
| DC55 | +0.66 | +1.65 | 18.1 | 0.09 | 37.7 | 2.3 | 0.000 | 0.01 |
| TSMOM | +1.07 | +2.70 | 13.6 | 0.20 | 27.2 | 9.6 | 0.117 | 0.04 |
| vol_managed | +4.20 | +10.59 | 12.9 | 0.82 | 19.3 | 1.2 | 0.000 | 0.01 |
| static (B1's) | +0.80 | +2.01 | 8.1 | 0.25 | 16.7 | 32.2 | 0.053 | 0.15 |

## 5. MR–TF daily excess-return correlation by year

| TF | 2016 | 2017 | 2018 | 2019 | 2020 | 2021 | 2022 | 2023 | 2024 | 2025 | all |
|---|---|---|---|---|---|---|---|---|---|---|---|
| DC55 | -0.06 | -0.32 | -0.06 | 0.15 | -0.40 | 0.00 | -0.11 | -0.06 | 0.04 | -0.47 | -0.23 |
| TSMOM | -0.24 | -0.40 | -0.18 | 0.05 | -0.39 | -0.11 | -0.17 | -0.34 | -0.15 | -0.61 | -0.30 |

The static blend already harvests this diversification; H4 asks whether the weight adds to it.

## 6. Turnover (one-way, × equity per year) and cost attributed to each leg

| configuration | blend | MR leg (w f^MR) | TF leg ((1 − w) f^TF) | netted | cost bp/day | MR share | TF share |
|---|---|---|---|---|---|---|---|
| B1 | 64.5 | 63.7 | 11.8 | 11.0 | 0.15 | 0.13 | 0.02 |
| R2 | 65.7 | 66.1 | 14.4 | 14.7 | 0.15 | 0.13 | 0.03 |
| B1_percentile | 66.3 | 68.5 | 10.8 | 13.0 | 0.15 | 0.13 | 0.02 |
| B6_diff | 62.9 | 62.5 | 12.7 | 12.3 | 0.15 | 0.12 | 0.02 |
| hard_switch | 70.5 | 60.8 | 14.3 | 4.7 | 0.17 | 0.13 | 0.03 |
| kappa_025 | 63.5 | 65.2 | 10.6 | 12.3 | 0.15 | 0.13 | 0.02 |
| kappa_075 | 67.3 | 62.3 | 13.1 | 8.0 | 0.16 | 0.13 | 0.03 |
| vol_target_10 | 41.2 | 39.9 | 10.0 | 8.7 | 0.10 | 0.08 | 0.02 |
| donchian55 | 65.2 | 63.7 | 5.9 | 4.4 | 0.15 | 0.14 | 0.01 |

Legs are filled as the engine fills the blend; the cost split is pro rata to the legs' turnover.

## 7. R2 refits over the window (the R2 variant's caveat, U10)

121 refits, 0 not converged, 0 degenerate, 103 swap-flagged (|Δφ| < 0.05); median φ 0.648 / 0.677, separation 0.028 (10 % quantile 0.005).

## 8. Reading (written by hand after the run)

- **Headline: null (diagnostic).** The B1-weighted blend's Sharpe is 0.39 against 0.25 for the static 50/50
  (ΔSharpe +0.15, p 0.15, studentized t 1.05; one-sided 90 % lower bound −0.04). The effect is in the registered
  direction and at about twice the expected +0.08, but well under the MDE (0.35 at ρ 0.9; the realized ρ is 0.84,
  where the MDE is ≈ 0.44).
- **It is mostly timing, not tilt.** The matched-mean static (w 0.45) has Sharpe 0.26, so the blend's ΔSharpe
  against it is +0.13 (p 0.19). Leaning toward TSMOM accounts for ≈ 0.01–0.02 of the +0.15.
- **The variants agree in sign but not in strength.** Every B1-weighted variant has ΔSharpe +0.10 … +0.19 with
  p 0.10–0.22 (K = 182, every p·K = 1.0). R2's continuous P(fast) gives +0.17 (p 0.096), but its two φs are
  0.648 / 0.677 with 103 of 121 refits swap-flagged, so it is a variance-regime weight like B1, not a reversion-speed
  one (U10). B6_diff ("which rule has been working") is the one negative row (−0.17) and fails the floor.
- **The retail benchmark dominates.** The vol-managed buy-and-hold (15 % target, exposure ≤ 1) earned Sharpe 0.82 and
  +10.6 %/yr excess on 2016–2025, against 0.39 for the headline blend and 0.20 for TSMOM alone. R₀ alone is
  −0.04. On this window neither the reversal rule nor its regime weighting comes close to holding SPY with a vol cap.
- **Diversification is real but small in value.** R₀ and TSMOM are negatively correlated in 9 of 10 years (−0.30
  overall; −0.61 in 2025), which is why the static blend's vol is 8.1 % against 13.6–13.8 % for each component. Yet
  the static blend's Sharpe (0.25) is only 0.05 above TSMOM's alone (0.20): R₀ lowers the volatility but adds no
  return.
- **Costs.** The blend trades ≈ 65 × equity per year one-way, almost all of it R₀'s leg. The TF leg's opposite
  trades net out ≈ 11 × equity per year, and the cost (0.15 bp/day) is 85 % MR.
- **What it means for the program.** H1 (no edge), H3 (no state predicts the edge) and now H4 (the state-weighted
  blend does not beat the static blend) are all diagnostic nulls on 2016–2025. The +0.15 is the largest positive
  number the program has produced, but it was earned with a state chosen after U10 read it, on the same window. Only
  the holdout (U12) and a forward test (U13) can say whether it is anything.
