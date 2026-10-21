# U8 — State estimators and their noise (synthetic; PLAN.md §7 U8)

Synthetic only: no market return or P&L is read, so no look is recorded (K unchanged). 500 draws per cell; Gaussian fGn (Davies–Harte), ARFIMA(0, H − ½, 0) with Gaussian innovations, and the same with Student-t₄ innovations (the Gaussian ARFIMA separates ARFIMA's short-lag structure from the tails); seeds fixed in `regime/calibrate.py`. Runtime of the study 52 s.

## 1. Bias / SD by estimator, innovations, window and H

MF-DFA h(2) on the standard scales equals DFA-1 standard exactly (tested), so E4's h(2) is the DFA-1 standard row at n 1,008. Abry–Veitch standard (j 2–6) needs ≥ 512 sessions; DFA-1 standard needs ≥ 252.

| estimator | innovations | n | H 0.40 | 0.45 | 0.50 | 0.55 | 0.60 | ms / estimate |
|---|---|---|---|---|---|---|---|---|
| DFA-1 standard [10, 126] | gaussian fGn | 252 | -0.006 / 0.070 | -0.002 / 0.076 | -0.014 / 0.079 | -0.008 / 0.086 | -0.013 / 0.088 | 0.24 |
| DFA-1 short [4, 32] | gaussian fGn | 252 | +0.063 / 0.048 | +0.059 / 0.051 | +0.051 / 0.061 | +0.048 / 0.057 | +0.042 / 0.063 | 0.25 |
| Abry–Veitch short j 1–4 | gaussian fGn | 252 | -0.009 / 0.071 | -0.002 / 0.072 | +0.012 / 0.077 | +0.015 / 0.076 | +0.020 / 0.076 | 0.18 |
| R/S classic (reference) | gaussian fGn | 252 | +0.110 / 0.057 | +0.102 / 0.063 | +0.084 / 0.060 | +0.073 / 0.066 | +0.059 / 0.065 | 0.33 |
| DFA-1 standard [10, 126] | gaussian fGn | 504 | +0.002 / 0.047 | -0.002 / 0.058 | -0.001 / 0.060 | -0.004 / 0.060 | -0.005 / 0.064 | 0.26 |
| DFA-1 short [4, 32] | gaussian fGn | 504 | +0.062 / 0.035 | +0.057 / 0.039 | +0.054 / 0.037 | +0.048 / 0.040 | +0.047 / 0.043 | 0.28 |
| Abry–Veitch short j 1–4 | gaussian fGn | 504 | -0.017 / 0.050 | -0.005 / 0.049 | +0.003 / 0.047 | +0.012 / 0.046 | +0.015 / 0.051 | 0.19 |
| R/S classic (reference) | gaussian fGn | 504 | +0.099 / 0.041 | +0.085 / 0.045 | +0.076 / 0.047 | +0.062 / 0.050 | +0.049 / 0.051 | 0.35 |
| DFA-1 standard [10, 126] | gaussian fGn | 1008 | +0.007 / 0.038 | +0.004 / 0.038 | +0.001 / 0.043 | -0.001 / 0.047 | +0.002 / 0.049 | 0.30 |
| DFA-1 short [4, 32] | gaussian fGn | 1008 | +0.063 / 0.024 | +0.059 / 0.027 | +0.054 / 0.027 | +0.050 / 0.029 | +0.049 / 0.030 | 0.35 |
| Abry–Veitch short j 1–4 | gaussian fGn | 1008 | -0.018 / 0.033 | -0.006 / 0.034 | +0.004 / 0.031 | +0.008 / 0.033 | +0.016 / 0.033 | 0.21 |
| Abry–Veitch standard j 2–6 | gaussian fGn | 1008 | -0.012 / 0.043 | -0.003 / 0.042 | +0.005 / 0.044 | +0.008 / 0.043 | +0.015 / 0.041 | 0.28 |
| R/S classic (reference) | gaussian fGn | 1008 | +0.086 / 0.035 | +0.073 / 0.034 | +0.064 / 0.038 | +0.052 / 0.041 | +0.042 / 0.042 | 0.38 |
| DFA-1 standard [10, 126] | gaussian fGn | 2016 | +0.007 / 0.025 | +0.003 / 0.028 | +0.006 / 0.029 | +0.004 / 0.030 | +0.002 / 0.031 | 0.37 |
| DFA-1 short [4, 32] | gaussian fGn | 2016 | +0.065 / 0.018 | +0.059 / 0.018 | +0.055 / 0.018 | +0.052 / 0.023 | +0.049 / 0.020 | 0.47 |
| Abry–Veitch short j 1–4 | gaussian fGn | 2016 | -0.017 / 0.023 | -0.007 / 0.023 | +0.001 / 0.023 | +0.010 / 0.023 | +0.016 / 0.022 | 0.24 |
| Abry–Veitch standard j 2–6 | gaussian fGn | 2016 | -0.011 / 0.029 | -0.007 / 0.028 | +0.002 / 0.027 | +0.009 / 0.030 | +0.012 / 0.027 | 0.32 |
| R/S classic (reference) | gaussian fGn | 2016 | +0.076 / 0.026 | +0.065 / 0.027 | +0.058 / 0.031 | +0.049 / 0.031 | +0.038 / 0.032 | 0.45 |
| DFA-1 standard [10, 126] | ARFIMA gaussian | 252 | +0.009 / 0.074 | -0.002 / 0.078 | -0.011 / 0.085 | -0.012 / 0.086 | -0.020 / 0.091 | 0.24 |
| DFA-1 short [4, 32] | ARFIMA gaussian | 252 | +0.083 / 0.051 | +0.065 / 0.054 | +0.052 / 0.056 | +0.034 / 0.055 | +0.029 / 0.061 | 0.25 |
| Abry–Veitch short j 1–4 | ARFIMA gaussian | 252 | +0.028 / 0.073 | +0.017 / 0.073 | +0.009 / 0.071 | -0.008 / 0.071 | -0.011 / 0.072 | 0.18 |
| R/S classic (reference) | ARFIMA gaussian | 252 | +0.125 / 0.061 | +0.107 / 0.062 | +0.088 / 0.065 | +0.071 / 0.066 | +0.050 / 0.069 | 0.33 |
| DFA-1 standard [10, 126] | ARFIMA gaussian | 504 | +0.013 / 0.053 | +0.007 / 0.054 | -0.003 / 0.060 | -0.008 / 0.062 | -0.012 / 0.061 | 0.26 |
| DFA-1 short [4, 32] | ARFIMA gaussian | 504 | +0.081 / 0.035 | +0.069 / 0.036 | +0.055 / 0.039 | +0.039 / 0.043 | +0.031 / 0.042 | 0.28 |
| Abry–Veitch short j 1–4 | ARFIMA gaussian | 504 | +0.020 / 0.048 | +0.018 / 0.048 | +0.006 / 0.049 | -0.005 / 0.050 | -0.014 / 0.047 | 0.19 |
| R/S classic (reference) | ARFIMA gaussian | 504 | +0.108 / 0.042 | +0.091 / 0.044 | +0.071 / 0.047 | +0.061 / 0.050 | +0.045 / 0.050 | 0.34 |
| DFA-1 standard [10, 126] | ARFIMA gaussian | 1008 | +0.016 / 0.036 | +0.009 / 0.040 | +0.003 / 0.040 | -0.004 / 0.043 | -0.007 / 0.043 | 0.29 |
| DFA-1 short [4, 32] | ARFIMA gaussian | 1008 | +0.082 / 0.024 | +0.070 / 0.027 | +0.054 / 0.026 | +0.043 / 0.027 | +0.031 / 0.029 | 0.34 |
| Abry–Veitch short j 1–4 | ARFIMA gaussian | 1008 | +0.019 / 0.032 | +0.013 / 0.033 | +0.002 / 0.032 | -0.006 / 0.033 | -0.015 / 0.033 | 0.21 |
| Abry–Veitch standard j 2–6 | ARFIMA gaussian | 1008 | +0.005 / 0.039 | +0.006 / 0.044 | +0.005 / 0.039 | +0.004 / 0.042 | +0.002 / 0.041 | 0.28 |
| R/S classic (reference) | ARFIMA gaussian | 1008 | +0.093 / 0.033 | +0.080 / 0.034 | +0.064 / 0.038 | +0.051 / 0.039 | +0.039 / 0.040 | 0.38 |
| DFA-1 standard [10, 126] | ARFIMA gaussian | 2016 | +0.018 / 0.026 | +0.011 / 0.028 | +0.005 / 0.029 | +0.001 / 0.031 | -0.005 / 0.033 | 0.37 |
| DFA-1 short [4, 32] | ARFIMA gaussian | 2016 | +0.085 / 0.018 | +0.069 / 0.019 | +0.056 / 0.019 | +0.043 / 0.020 | +0.031 / 0.020 | 0.48 |
| Abry–Veitch short j 1–4 | ARFIMA gaussian | 2016 | +0.019 / 0.022 | +0.010 / 0.024 | +0.002 / 0.022 | -0.009 / 0.023 | -0.018 / 0.022 | 0.23 |
| Abry–Veitch standard j 2–6 | ARFIMA gaussian | 2016 | +0.004 / 0.028 | +0.004 / 0.028 | +0.003 / 0.027 | -0.000 / 0.028 | -0.003 / 0.026 | 0.32 |
| R/S classic (reference) | ARFIMA gaussian | 2016 | +0.081 / 0.025 | +0.070 / 0.028 | +0.057 / 0.032 | +0.046 / 0.033 | +0.031 / 0.032 | 0.44 |
| DFA-1 standard [10, 126] | ARFIMA t4 | 252 | +0.007 / 0.070 | -0.002 / 0.076 | -0.003 / 0.077 | -0.015 / 0.088 | -0.022 / 0.092 | 0.24 |
| DFA-1 short [4, 32] | ARFIMA t4 | 252 | +0.082 / 0.054 | +0.069 / 0.051 | +0.055 / 0.056 | +0.038 / 0.059 | +0.026 / 0.060 | 0.25 |
| Abry–Veitch short j 1–4 | ARFIMA t4 | 252 | +0.029 / 0.077 | +0.020 / 0.076 | +0.009 / 0.078 | -0.007 / 0.076 | -0.015 / 0.075 | 0.18 |
| R/S classic (reference) | ARFIMA t4 | 252 | +0.122 / 0.057 | +0.103 / 0.059 | +0.089 / 0.059 | +0.069 / 0.065 | +0.051 / 0.066 | 0.32 |
| DFA-1 standard [10, 126] | ARFIMA t4 | 504 | +0.019 / 0.051 | +0.008 / 0.056 | +0.001 / 0.059 | -0.007 / 0.063 | -0.010 / 0.066 | 0.25 |
| DFA-1 short [4, 32] | ARFIMA t4 | 504 | +0.087 / 0.035 | +0.071 / 0.037 | +0.056 / 0.041 | +0.039 / 0.039 | +0.033 / 0.044 | 0.28 |
| Abry–Veitch short j 1–4 | ARFIMA t4 | 504 | +0.025 / 0.050 | +0.015 / 0.050 | +0.002 / 0.053 | -0.003 / 0.045 | -0.012 / 0.050 | 0.18 |
| R/S classic (reference) | ARFIMA t4 | 504 | +0.112 / 0.040 | +0.094 / 0.045 | +0.076 / 0.047 | +0.057 / 0.051 | +0.044 / 0.050 | 0.34 |
| DFA-1 standard [10, 126] | ARFIMA t4 | 1008 | +0.019 / 0.038 | +0.008 / 0.038 | +0.002 / 0.044 | -0.001 / 0.041 | -0.009 / 0.048 | 0.28 |
| DFA-1 short [4, 32] | ARFIMA t4 | 1008 | +0.085 / 0.027 | +0.068 / 0.028 | +0.054 / 0.029 | +0.041 / 0.030 | +0.031 / 0.029 | 0.34 |
| Abry–Veitch short j 1–4 | ARFIMA t4 | 1008 | +0.018 / 0.030 | +0.011 / 0.035 | +0.001 / 0.033 | -0.008 / 0.032 | -0.017 / 0.033 | 0.20 |
| Abry–Veitch standard j 2–6 | ARFIMA t4 | 1008 | +0.007 / 0.045 | +0.006 / 0.043 | +0.003 / 0.046 | +0.002 / 0.045 | +0.001 / 0.042 | 0.27 |
| R/S classic (reference) | ARFIMA t4 | 1008 | +0.094 / 0.034 | +0.080 / 0.033 | +0.066 / 0.037 | +0.053 / 0.037 | +0.038 / 0.040 | 0.37 |
| DFA-1 standard [10, 126] | ARFIMA t4 | 2016 | +0.019 / 0.026 | +0.012 / 0.028 | +0.001 / 0.028 | -0.003 / 0.031 | -0.004 / 0.032 | 0.36 |
| DFA-1 short [4, 32] | ARFIMA t4 | 2016 | +0.086 / 0.020 | +0.070 / 0.020 | +0.056 / 0.022 | +0.041 / 0.020 | +0.029 / 0.021 | 0.46 |
| Abry–Veitch short j 1–4 | ARFIMA t4 | 2016 | +0.020 / 0.022 | +0.010 / 0.022 | +0.001 / 0.024 | -0.008 / 0.023 | -0.019 / 0.022 | 0.23 |
| Abry–Veitch standard j 2–6 | ARFIMA t4 | 2016 | +0.007 / 0.028 | +0.005 / 0.029 | +0.001 / 0.030 | -0.002 / 0.027 | -0.003 / 0.027 | 0.31 |
| R/S classic (reference) | ARFIMA t4 | 2016 | +0.083 / 0.027 | +0.068 / 0.027 | +0.056 / 0.031 | +0.045 / 0.032 | +0.035 / 0.033 | 0.43 |

## 2. Smallest window with SD ≤ 0.025 (worst over H)

The registered grid is 252 / 504 / 1,008 (§7 U8). The 2,016 column (8 years) is informational: it shows the window a powered estimate would need. A gate on an 8-year window barely moves, so it is never frozen. The rule is on the raw SD; the SD in H units (raw SD ÷ the slope of the mean estimate on the true H) is stricter wherever an estimator responds to H by less than one for one, and is shown beside it.

| estimator | innovations | min n (raw) | min n (H units) | SD 252 | SD 504 | SD 1008 | SD 2016 | SD_H 252 | SD_H 504 | SD_H 1008 | SD_H 2016 |
|---|---|---|---|---|---|---|---|---|---|---|---|
| Abry–Veitch short j 1–4 | ARFIMA gaussian | 2016 | — | 0.073 | 0.050 | 0.033 | 0.024 | 0.093 | 0.062 | 0.041 | 0.029 |
| Abry–Veitch short j 1–4 | ARFIMA t4 | 2016 | — | 0.078 | 0.053 | 0.035 | 0.024 | 0.101 | 0.065 | 0.042 | 0.029 |
| Abry–Veitch short j 1–4 | gaussian fGn | 2016 | 2016 | 0.077 | 0.051 | 0.034 | 0.023 | 0.067 | 0.044 | 0.029 | 0.020 |
| Abry–Veitch standard j 2–6 | ARFIMA gaussian | — | — | — | — | 0.044 | 0.028 | — | — | 0.045 | 0.029 |
| Abry–Veitch standard j 2–6 | ARFIMA t4 | — | — | — | — | 0.046 | 0.030 | — | — | 0.047 | 0.031 |
| Abry–Veitch standard j 2–6 | gaussian fGn | — | — | — | — | 0.044 | 0.030 | — | — | 0.039 | 0.027 |
| DFA-1 short [4, 32] | ARFIMA gaussian | 2016 | — | 0.061 | 0.043 | 0.029 | 0.020 | 0.085 | 0.058 | 0.039 | 0.028 |
| DFA-1 short [4, 32] | ARFIMA t4 | 2016 | — | 0.060 | 0.044 | 0.030 | 0.022 | 0.085 | 0.062 | 0.041 | 0.030 |
| DFA-1 short [4, 32] | gaussian fGn | 2016 | 2016 | 0.063 | 0.043 | 0.030 | 0.023 | 0.070 | 0.047 | 0.032 | 0.024 |
| DFA-1 standard [10, 126] | ARFIMA gaussian | — | — | 0.091 | 0.062 | 0.043 | 0.033 | 0.106 | 0.071 | 0.049 | 0.037 |
| DFA-1 standard [10, 126] | ARFIMA t4 | — | — | 0.092 | 0.066 | 0.048 | 0.032 | 0.107 | 0.077 | 0.055 | 0.037 |
| DFA-1 standard [10, 126] | gaussian fGn | — | — | 0.088 | 0.064 | 0.049 | 0.031 | 0.091 | 0.067 | 0.050 | 0.031 |
| R/S classic (reference) | ARFIMA gaussian | — | — | 0.069 | 0.050 | 0.040 | 0.033 | 0.109 | 0.073 | 0.055 | 0.044 |
| R/S classic (reference) | ARFIMA t4 | — | — | 0.066 | 0.051 | 0.040 | 0.033 | 0.102 | 0.078 | 0.055 | 0.043 |
| R/S classic (reference) | gaussian fGn | — | — | 0.066 | 0.051 | 0.042 | 0.032 | 0.089 | 0.067 | 0.054 | 0.040 |

**Lo's modified R/S (the reference test; V outside [0.809, 1.862] = reject short memory at 5 %), fGn:**

| H | n | rejection rate |
|---|---|---|
| 0.4 | 1008 | 0.246 |
| 0.45 | 1008 | 0.122 |
| 0.5 | 1008 | 0.072 |
| 0.55 | 1008 | 0.112 |
| 0.6 | 1008 | 0.160 |

The R/S row of §1 is the classic R/S Hurst exponent (`nolds.hurst_rs` convention), the number most often quoted; Lo's V is a test statistic, not an H estimate, so it appears here as size and power.

## 3. Overlap: consecutive rolling estimates (one seeded 30-year fGn path, H = 0.5; the integrated autocorrelation is summed until ρ first drops below 0.05; no interval)

| estimator | window | lag-1 autocorrelation | independent estimates per decade |
|---|---|---|---|
| DFA-1 short [4, 32] | 252 | 0.924 | 13.2 |
| DFA-1 standard [10, 126] | 252 | 0.996 | 6.4 |
| Abry–Veitch short j 1–4 | 252 | 0.999 | 10.9 |
| DFA-1 short [4, 32] | 504 | 0.921 | 7.6 |
| DFA-1 standard [10, 126] | 504 | 0.997 | 4.5 |
| Abry–Veitch short j 1–4 | 504 | 1.000 | 6.5 |
| DFA-1 short [4, 32] | 1008 | 0.918 | 4.9 |
| DFA-1 standard [10, 126] | 1008 | 0.997 | 3.4 |
| Abry–Veitch short j 1–4 | 1008 | 1.000 | 3.8 |

## 4. Spurious multifractality from fat tails alone (MF-DFA Δα, monofractal input)

| series | n | mean Δα | SD | 95th pct |
|---|---|---|---|---|
| iid Gaussian | 1008 | 0.158 | 0.087 | 0.309 |
| iid t4 | 1008 | 0.359 | 0.129 | 0.571 |

## 5. B4's random-walk reference

AR(1) φ of d*_t over 126 sessions on random-walk closes: mean 0.708, SD 0.054, 5–95 % [0.613, 0.788]. d* reverts by construction; B4 is read against this value, not against 0.

## 6. Frozen configurations (copied into the H3 spec draft)

| label | estimator | §6.4 window | frozen window | SD at §6.4 | SD at frozen | favourable | π | status |
|---|---|---|---|---|---|---|---|---|
| E1 | DFA-1 standard [10, 126] | 504 | 504 | 0.0656 | 0.0656 | below trailing-504 median | 0.5 | under-powered |
| E2 | DFA-1 short [4, 32] | 504 | 504 | 0.0442 | 0.0442 | below trailing-504 median | 0.5 | under-powered |
| E3 | Abry–Veitch short j 1–4 | 512 | 512 | 0.0531 | 0.0531 | below trailing-504 median | 0.5 | under-powered |
| E4 | DFA-1 standard [10, 126] | 1008 | 1008 | 0.0489 | 0.0489 | h(2) below trailing median | 0.5 | under-powered |

Baselines B1–B6 are fixed by §6.4 (no noise study: they are not long-memory estimators).

## 7. Compute

All state columns for SPY, QQQ, IWM, DIA on the spliced long series (SPY 8288, QQQ 6746, IWM 6438, DIA 7032 sessions): 14.8 s (target < 60 s for 2,500 sessions × 4). NaN columns on 2016-01-04: {'SPY': 0, 'QQQ': 0, 'IWM': 0, 'DIA': 0} (the `diag` warm-up fills every window). No state value was printed or read.

## 8. Reading (hand-written after the run; not regenerated by the script)

- **No Hurst-type estimator is powered at a usable window.**
  - Raw SD stays above the 0.025 rule at every window up to 1,008 sessions, under all three generators. At the
    §6.4 windows the SDs are DFA-1 standard 0.066 (504), DFA-1 short 0.044 (504), Abry–Veitch short 0.053 (512,
    read at 504) and MF-DFA h(2) 0.049 (1,008).
  - The best two reach the raw rule only at 2,016 sessions (8 years).
  - In H units (SD ÷ the estimator's response to H) the rule is met only on fGn at 2,016, and never on either
    ARFIMA.
  - By §7 U8's decision rule, **E1–E4 are all variants labelled under-powered.**
- **Consequence for §6.4's H3 headline program (B1, B4, E2, R2), for U10.** E2 cannot enter it under the rule. At
  SD 0.044, H 0.45 and 0.50 sit about one SD of a single estimate apart. A "below trailing median" gate on E2
  would switch mostly on estimation noise. Replacing E2 in the headline program is an amendment, and the user
  decides it. Under the §6.2 routing the registered H3 runs only B1 on `diag`, so this binds only if that routing
  is amended.
- **Bias comes from short-lag structure, not tails.** DFA-1 short is biased up by +0.05 to +0.06 on fGn at every
  H. On ARFIMA the bias depends on H: +0.08 at H 0.40, falling to +0.03 at 0.60. That pattern is the same with
  Gaussian and with t₄ innovations, so it comes from ARFIMA's different short-lag autocorrelation (ρ₁ at H 0.40 is
  −0.091 for ARFIMA and −0.129 for fGn), not from fat tails. s = 4 is noisy, as §7 U8's reviewer focus warned. The
  H-dependent bias attenuates the estimator's response to H (≈ 0.73 per unit on ARFIMA), which is why the
  H-units SDs are larger. Abry–Veitch is nearly unbiased on fGn (|bias| ≤ 0.02); on ARFIMA its bias also depends
  on H.
- **The reference test is weak too.** Lo's modified R/S at n 1,008 has size 0.072 (nominal 0.05) and rejects short
  memory only 25 % of the time at H 0.40 and 16 % at H 0.60.
- **Overlap.** Consecutive rolling estimates are almost the same number (lag-1 autocorrelation 0.92–1.00). A decade
  holds about 3.4–7.6 independent estimates at windows of 504–1,008 sessions (one seeded path; no interval). Any
  gate built on them has a handful of independent episodes in 2016–2025.
- **Multifractality.** Fat tails alone give MF-DFA Δα ≈ 0.36 on a monofractal t₄ series (95th percentile 0.57),
  against 0.16 on Gaussian noise. A market Δα below about 0.6 is not evidence of multifractality.
- **B4's reference.** On a random walk, φ of d*_t over 126 sessions is 0.71 (5–95 % 0.61–0.79). B4 is read against
  that value, never against 0.
- **What this says about "improving on the vanilla rule" with these estimators.** At the windows where they move
  fast enough to time a 1–6-week reversion rule, they cannot tell H 0.45 from 0.50. Where they can, they move too
  slowly to time anything. U9's regime models (filtered, one-step-ahead) are the remaining candidates. Their
  detection study measures delay and false alarms directly.
