# U7 — H2 persistence: is the edge still there? (PLAN.md §6.3)

Looks: K 139 → **153** (every window). Development-window stream: the H1 headline (R₀ on SPY, `next_open`, 1.0 bp round trip, fees, borrow, DTB3), 2016-01-05 → 2025-12-31, n 2513; rebuilt and equal to its H1 ledger row (and QQQ, IWM, DIA to theirs) to 1e-9 bp/day. `diag` stream: R₀ on SPY 1993-02-01 → 2015-12-31 (n 5773), `next_open`, gross and at the historical cost schedule (one tick / previous close, $1/16 to 2001-04-09 then $0.01, floor 1.0 bp; mean 3.7 bp, 14.2 bp at the start, no fees; an assumption, stated in `experiments/persistence.py`). No conditional table uses `diag` (H3's test sample under the §6.2 routing). Intervals: two-sided 90 % block bootstrap (block 21, 5,000, seed 0).

**Summary line (§6.3):** last-5-year (2021–2025) Newey–West t 0.19 (+0.40 bp/day); Bai–Perron 0 break(s) (supF(1) p 0.808, UDmax p 0.796); P(μ_T > 0) = 0.447 at q̂ (0.447 at q = 0; causal track 0.447), q_zero True.

## 1. Edge by period

**H1 stream, net excess @1.0 bp — by calendar year**

| period | n | bp/day | 90 % CI | t NW |
|---|---|---|---|---|
| 2016 | 251 | +3.47 | [-0.91, +7.84] | 1.33 |
| 2017 | 251 | +0.08 | [-2.91, +3.07] | 0.05 |
| 2018 | 251 | -8.76 | [-15.87, -1.66] | -2.04 |
| 2019 | 252 | -1.26 | [-6.40, +3.87] | -0.40 |
| 2020 | 253 | +2.17 | [-10.10, +14.43] | 0.27 |
| 2021 | 252 | +2.41 | [-3.75, +8.56] | 0.66 |
| 2022 | 251 | +2.83 | [-9.85, +15.52] | 0.42 |
| 2023 | 250 | -6.06 | [-11.91, -0.22] | -1.94 |
| 2024 | 252 | -1.99 | [-6.46, +2.49] | -0.58 |
| 2025 | 250 | +4.81 | [-8.07, +17.68] | 0.91 |

**H1 stream, net excess @1.0 bp — rolling 3 calendar years**

| period | n | bp/day | 90 % CI | t NW |
|---|---|---|---|---|
| 2016–2018 | 753 | -1.74 | [-4.70, +1.23] | -0.96 |
| 2017–2019 | 754 | -3.31 | [-6.09, -0.53] | -1.77 |
| 2018–2020 | 756 | -2.61 | [-7.24, +2.03] | -0.81 |
| 2019–2021 | 757 | +1.10 | [-3.32, +5.53] | 0.36 |
| 2020–2022 | 756 | +2.47 | [-3.11, +8.04] | 0.67 |
| 2021–2023 | 753 | -0.26 | [-4.95, +4.42] | -0.09 |
| 2022–2024 | 753 | -1.73 | [-6.25, +2.79] | -0.63 |
| 2023–2025 | 752 | -1.08 | [-5.19, +3.03] | -0.46 |

**H1 stream, net excess @1.0 bp — last 5 years**

| period | n | bp/day | 90 % CI | t NW |
|---|---|---|---|---|
| 2021–2025 (last 5 years; the summary line) | 1255 | +0.40 | [-3.07, +3.87] | 0.19 |

**H1 stream, gross — by calendar year**

| period | n | bp/day | 90 % CI | t NW |
|---|---|---|---|---|
| 2016 | 251 | +3.94 | [-0.44, +8.33] | 1.51 |
| 2017 | 251 | +0.59 | [-2.41, +3.60] | 0.36 |
| 2018 | 251 | -7.93 | [-15.01, -0.85] | -1.84 |
| 2019 | 252 | -0.37 | [-5.54, +4.80] | -0.12 |
| 2020 | 253 | +2.68 | [-9.61, +14.98] | 0.34 |
| 2021 | 252 | +2.79 | [-3.39, +8.97] | 0.76 |
| 2022 | 251 | +3.81 | [-9.00, +16.63] | 0.56 |
| 2023 | 250 | -4.30 | [-10.15, +1.54] | -1.37 |
| 2024 | 252 | -0.28 | [-4.79, +4.22] | -0.08 |
| 2025 | 250 | +6.30 | [-6.61, +19.22] | 1.19 |

**H1 stream, gross — rolling 3 calendar years**

| period | n | bp/day | 90 % CI | t NW |
|---|---|---|---|---|
| 2016–2018 | 753 | -1.13 | [-4.10, +1.83] | -0.62 |
| 2017–2019 | 754 | -2.57 | [-5.34, +0.21] | -1.37 |
| 2018–2020 | 756 | -1.86 | [-6.49, +2.77] | -0.58 |
| 2019–2021 | 757 | +1.70 | [-2.74, +6.14] | 0.55 |
| 2020–2022 | 756 | +3.09 | [-2.51, +8.70] | 0.84 |
| 2021–2023 | 753 | +0.77 | [-3.90, +5.45] | 0.28 |
| 2022–2024 | 753 | -0.25 | [-4.77, +4.26] | -0.09 |
| 2023–2025 | 752 | +0.57 | [-3.54, +4.68] | 0.24 |

**diag R₀, gross — by calendar year**

| period | n | bp/day | 90 % CI | t NW |
|---|---|---|---|---|
| 1993 | 233 | +2.27 | [-0.71, +5.24] | 1.43 |
| 1994 | 252 | +0.95 | [-4.08, +5.99] | 0.36 |
| 1995 | 252 | +1.13 | [-2.99, +5.26] | 0.50 |
| 1996 | 254 | -0.48 | [-6.93, +5.96] | -0.13 |
| 1997 | 253 | +4.22 | [-1.67, +10.12] | 1.02 |
| 1998 | 252 | +2.90 | [-8.39, +14.19] | 0.45 |
| 1999 | 252 | -0.56 | [-9.53, +8.40] | -0.13 |
| 2000 | 252 | +5.85 | [-2.02, +13.72] | 0.98 |
| 2001 | 248 | -3.21 | [-18.35, +11.93] | -0.45 |
| 2002 | 252 | +10.25 | [-2.51, +23.01] | 1.30 |
| 2003 | 252 | +3.67 | [-4.03, +11.36] | 0.86 |
| 2004 | 252 | -2.48 | [-6.34, +1.39] | -0.89 |
| 2005 | 252 | +2.06 | [-2.10, +6.22] | 0.75 |
| 2006 | 251 | +0.89 | [-1.80, +3.58] | 0.46 |
| 2007 | 251 | +1.54 | [-4.88, +7.97] | 0.43 |
| 2008 | 253 | +32.25 | [+12.04, +52.46] | 2.42 |
| 2009 | 252 | +3.94 | [-9.08, +16.95] | 0.57 |
| 2010 | 252 | +0.12 | [-8.79, +9.04] | 0.02 |
| 2011 | 252 | +8.97 | [-3.36, +21.31] | 1.38 |
| 2012 | 250 | -1.16 | [-5.61, +3.29] | -0.40 |
| 2013 | 252 | -0.52 | [-5.58, +4.54] | -0.17 |
| 2014 | 252 | -0.61 | [-7.03, +5.81] | -0.19 |
| 2015 | 252 | +3.70 | [-2.31, +9.72] | 0.86 |

**diag R₀, gross — rolling 3 calendar years**

| period | n | bp/day | 90 % CI | t NW |
|---|---|---|---|---|
| 1993–1995 | 737 | +1.43 | [-0.76, +3.62] | 1.11 |
| 1994–1996 | 758 | +0.53 | [-2.29, +3.35] | 0.31 |
| 1995–1997 | 759 | +1.62 | [-1.40, +4.64] | 0.81 |
| 1996–1998 | 759 | +2.21 | [-2.06, +6.48] | 0.78 |
| 1997–1999 | 757 | +2.19 | [-2.44, +6.82] | 0.75 |
| 1998–2000 | 756 | +2.73 | [-2.07, +7.53] | 0.84 |
| 1999–2001 | 752 | +0.71 | [-4.89, +6.32] | 0.21 |
| 2000–2002 | 752 | +4.34 | [-2.06, +10.73] | 1.06 |
| 2001–2003 | 752 | +3.61 | [-2.96, +10.17] | 0.94 |
| 2002–2004 | 756 | +3.81 | [-0.87, +8.50] | 1.20 |
| 2003–2005 | 756 | +1.08 | [-1.85, +4.02] | 0.56 |
| 2004–2006 | 755 | +0.16 | [-1.85, +2.17] | 0.11 |
| 2005–2007 | 754 | +1.50 | [-0.89, +3.89] | 0.91 |
| 2006–2008 | 755 | +11.62 | [+3.24, +19.99] | 2.39 |
| 2007–2009 | 756 | +12.62 | [+3.69, +21.54] | 2.37 |
| 2008–2010 | 757 | +12.13 | [+3.11, +21.15] | 2.22 |
| 2009–2011 | 756 | +4.35 | [-1.75, +10.44] | 1.19 |
| 2010–2012 | 754 | +2.66 | [-2.25, +7.56] | 0.89 |
| 2011–2013 | 754 | +2.44 | [-2.21, +7.10] | 0.94 |
| 2012–2014 | 754 | -0.76 | [-3.50, +1.97] | -0.43 |
| 2013–2015 | 756 | +0.86 | [-2.28, +4.00] | 0.42 |

**diag R₀, net excess at the historical schedule — by calendar year**

| period | n | bp/day | 90 % CI | t NW |
|---|---|---|---|---|
| 1993 | 233 | -1.69 | [-4.42, +1.05] | -1.12 |
| 1994 | 252 | -3.46 | [-8.28, +1.35] | -1.34 |
| 1995 | 252 | -2.81 | [-6.82, +1.20] | -1.29 |
| 1996 | 254 | -4.17 | [-10.50, +2.16] | -1.15 |
| 1997 | 253 | +0.69 | [-5.20, +6.59] | 0.17 |
| 1998 | 252 | -0.13 | [-11.32, +11.06] | -0.02 |
| 1999 | 252 | -3.29 | [-12.19, +5.62] | -0.75 |
| 2000 | 252 | +2.76 | [-5.05, +10.57] | 0.47 |
| 2001 | 248 | -4.89 | [-20.06, +10.28] | -0.68 |
| 2002 | 252 | +9.41 | [-3.39, +22.21] | 1.20 |
| 2003 | 252 | +3.02 | [-4.65, +10.69] | 0.71 |
| 2004 | 252 | -3.15 | [-7.04, +0.74] | -1.13 |
| 2005 | 252 | +0.94 | [-3.17, +5.05] | 0.35 |
| 2006 | 251 | -0.53 | [-3.23, +2.18] | -0.27 |
| 2007 | 251 | +0.12 | [-6.27, +6.50] | 0.03 |
| 2008 | 253 | +31.49 | [+11.28, +51.70] | 2.36 |
| 2009 | 252 | +3.50 | [-9.49, +16.49] | 0.50 |
| 2010 | 252 | -0.26 | [-9.16, +8.63] | -0.05 |
| 2011 | 252 | +8.60 | [-3.72, +20.93] | 1.33 |
| 2012 | 250 | -1.50 | [-5.95, +2.95] | -0.52 |
| 2013 | 252 | -0.88 | [-5.93, +4.17] | -0.29 |
| 2014 | 252 | -0.94 | [-7.34, +5.46] | -0.30 |
| 2015 | 252 | +3.34 | [-2.66, +9.33] | 0.77 |

**diag R₀, net excess at the historical schedule — rolling 3 calendar years**

| period | n | bp/day | 90 % CI | t NW |
|---|---|---|---|---|
| 1993–1995 | 737 | -2.68 | [-4.78, -0.58] | -2.14 |
| 1994–1996 | 758 | -3.48 | [-6.22, -0.75] | -2.11 |
| 1995–1997 | 759 | -2.10 | [-5.08, +0.88] | -1.06 |
| 1996–1998 | 759 | -1.21 | [-5.46, +3.04] | -0.43 |
| 1997–1999 | 757 | -0.91 | [-5.49, +3.68] | -0.31 |
| 1998–2000 | 756 | -0.22 | [-4.96, +4.52] | -0.07 |
| 1999–2001 | 752 | -1.79 | [-7.31, +3.74] | -0.52 |
| 2000–2002 | 752 | +2.47 | [-3.91, +8.84] | 0.60 |
| 2001–2003 | 752 | +2.55 | [-4.02, +9.13] | 0.66 |
| 2002–2004 | 756 | +3.10 | [-1.58, +7.77] | 0.98 |
| 2003–2005 | 756 | +0.27 | [-2.65, +3.19] | 0.14 |
| 2004–2006 | 755 | -0.91 | [-2.90, +1.08] | -0.62 |
| 2005–2007 | 754 | +0.18 | [-2.19, +2.55] | 0.11 |
| 2006–2008 | 755 | +10.42 | [+1.99, +18.84] | 2.14 |
| 2007–2009 | 756 | +11.75 | [+2.84, +20.65] | 2.21 |
| 2008–2010 | 757 | +11.60 | [+2.63, +20.58] | 2.12 |
| 2009–2011 | 756 | +3.95 | [-2.13, +10.03] | 1.09 |
| 2010–2012 | 754 | +2.29 | [-2.61, +7.19] | 0.77 |
| 2011–2013 | 754 | +2.08 | [-2.56, +6.73] | 0.80 |
| 2012–2014 | 754 | -1.11 | [-3.84, +1.62] | -0.63 |
| 2013–2015 | 756 | +0.51 | [-2.63, +3.64] | 0.25 |

## 2. Structural breaks in the mean (Bai–Perron, HAC)

**H1 stream, net excess** — breaks chosen (sequential, 5 %): **0**; UDmax 2.81 (p 0.796); h = 377 sessions.

| step | supF(l+1 \| l) | p |
|---|---|---|
| 0 → 1 | 1.92 | 0.808 |

**diag R₀, net excess at the historical schedule** — breaks chosen (sequential, 5 %): **0**; UDmax 8.81 (p 0.052); h = 866 sessions.

| step | supF(l+1 \| l) | p |
|---|---|---|
| 0 → 1 | 6.94 | 0.108 |

## 3. Kalman edge tracker (local level) — is it true today?

Whole-sample MLE: q̂ = 0.00e+00 (σ̂_ε 86.6 bp); LR test of q = 0: 0.00, p 1.000 → **q_zero = True**. Filtered μ_T = -0.23 bp/day (sd 1.73; 90 % interval [-3.07, +2.61]); **P(μ_T > 0) = 0.447** at q̂, 0.447 at q = 0 (the full-sample mean's posterior).

Profile log-likelihood (relative to its maximum):

| q | Δ log L |
|---|---|
| 0 | +0.00 |
| 1e-08 | -0.00 |
| 1e-07 | -0.04 |
| 1e-06 | -0.35 |
| 1e-05 | -1.91 |
| 1e-04 | -7.57 |
| 1e-03 | -26.46 |

Causal track (refit every 21 sessions on the past only, from session 252; 108 refits, 108 with q_zero): the U13 monitor (`live.monitor.EdgeMonitor`) reproduces it exactly.

| year end | μ bp/day | sd | P(μ > 0) |
|---|---|---|---|
| 2016-12-30 | — | — | — |
| 2017-12-29 | +1.78 | 1.81 | 0.837 |
| 2018-12-31 | -7.20 | 4.10 | 0.039 |
| 2019-12-31 | -2.57 | 3.14 | 0.206 |
| 2020-12-31 | -0.86 | 2.61 | 0.371 |
| 2021-12-31 | -0.31 | 2.25 | 0.445 |
| 2022-12-30 | +0.14 | 2.19 | 0.525 |
| 2023-12-29 | -0.63 | 1.99 | 0.375 |
| 2024-12-31 | -0.79 | 1.80 | 0.332 |
| 2025-12-31 | -0.23 | 1.73 | 0.447 |

## 4. Conditional tables (development window; reported, never used to change any F_S of §6.4)

**B1 — 21-session realized vol, quintile vs trailing 504 (Q4–Q5 = §6.4's favourable set)**

| period | n | bp/day | 90 % CI | t NW |
|---|---|---|---|---|
| Q1 | 602 | -0.40 | [-2.91, +2.12] | -0.27 |
| Q2 | 527 | -2.92 | [-6.68, +0.85] | -1.37 |
| Q3 | 383 | -0.77 | [-7.68, +6.14] | -0.20 |
| Q4 | 449 | -0.03 | [-6.85, +6.79] | -0.01 |
| Q5 | 552 | +2.73 | [-4.81, +10.28] | 0.53 |

**B2 — VIX at the decision, tercile vs trailing 504**

| period | n | bp/day | 90 % CI | t NW |
|---|---|---|---|---|
| T1 low | 832 | -2.76 | [-5.01, -0.51] | -2.08 |
| T2 | 831 | +1.06 | [-1.72, +3.83] | 0.58 |
| T3 high | 850 | +0.99 | [-4.80, +6.78] | 0.27 |

**Prior 21 sessions: sign of the region's net excess**

| period | n | bp/day | 90 % CI | t NW |
|---|---|---|---|---|
| negative | 1335 | +2.41 | [-1.33, +6.15] | 1.18 |
| positive | 1157 | -3.22 | [-7.64, +1.21] | -1.24 |
| no state (warm-up) | 21 | — | [—, —] | — |

**Prior 21 sessions: sign of SPY's total return**

| period | n | bp/day | 90 % CI | t NW |
|---|---|---|---|---|
| negative | 757 | +3.17 | [-2.93, +9.27] | 0.82 |
| positive | 1756 | -1.69 | [-3.87, +0.49] | -1.30 |

**Weekday of the return session**

| period | n | bp/day | 90 % CI | t NW |
|---|---|---|---|---|
| Friday | 506 | -1.40 | [-8.98, +6.17] | -0.37 |
| Monday | 468 | -7.36 | [-15.53, +0.80] | -1.60 |
| Thursday | 506 | -2.90 | [-9.84, +4.04] | -0.76 |
| Tuesday | 518 | +13.20 | [+0.69, +25.72] | 2.92 |
| Wednesday | 515 | -3.48 | [-12.32, +5.36] | -0.82 |

**Long vs short side and legs (gross, first order; as U6 §4)**

| part | n | bp/day | t NW |
|---|---|---|---|
| overnight leg (a·r_on) | 2513 | -0.48 | -0.47 |
| intraday leg (b·r_id) | 2513 | +1.22 | 1.01 |
| long side | 2513 | +2.33 | 2.12 |
| short side | 2513 | -1.59 | -1.61 |
| cross term | 2513 | -0.01 | -0.51 |

## 5. Cross-instrument coherence (R₀ at the registered cost)

| year | SPY | QQQ | IWM | DIA | others with SPY's sign |
|---|---|---|---|---|---|
| 2016 | +3.47 | +0.34 | -0.16 | +0.88 | 2 / 3 |
| 2017 | +0.08 | -0.22 | +4.75 | -2.28 | 1 / 3 |
| 2018 | -8.76 | -5.58 | -7.92 | -9.66 | 3 / 3 |
| 2019 | -1.26 | +0.86 | +1.07 | -1.10 | 1 / 3 |
| 2020 | +2.17 | +12.72 | -2.35 | +3.95 | 2 / 3 |
| 2021 | +2.41 | +7.03 | +8.09 | +1.40 | 3 / 3 |
| 2022 | +2.83 | +5.12 | +6.83 | +1.16 | 3 / 3 |
| 2023 | -6.06 | -6.78 | -7.63 | -6.87 | 3 / 3 |
| 2024 | -1.99 | +0.16 | +1.70 | -2.97 | 1 / 3 |
| 2025 | +4.81 | +4.81 | +11.12 | +1.60 | 3 / 3 |

Correlation of the daily net excess streams:

|  | SPY | QQQ | IWM | DIA |
|---|---|---|---|---|
| SPY | 1.00 | 0.85 | 0.77 | 0.90 |
| QQQ | 0.85 | 1.00 | 0.59 | 0.66 |
| IWM | 0.77 | 0.59 | 1.00 | 0.74 |
| DIA | 0.90 | 0.66 | 0.74 | 1.00 |

## 6. Reading (hand-written after the run; not regenerated by the script)

- **Nothing persistent to track.**
  - The H1 stream's mean is indistinguishable from zero in every 3-year window (t from −1.77 to +0.67) and over
    the last 5 years (+0.40 bp/day, t 0.19).
  - Bai–Perron finds no break (supF(1) p 0.81).
  - The local-level MLE puts q at 0, and so did all 108 causal refits. The data do not support a moving edge, and
    the tracker reduces to the running mean.
  - "Is it true today?": P(μ_T > 0) = 0.45 at 2025-12-31, with μ_T −0.23 bp/day [−3.07, +2.61].
- **Track history.** Of the year-ends, only 2017's was above 0.8 (0.84). It fell to 0.04 after 2018 (−8.8
  bp/day that year) and has had no year-end above 0.53 since.
- **`diag` 1993–2015, at the historical cost schedule.**
  - The edge is a 2006–10 episode: +10.4 to +11.8 bp/day in the windows containing 2008, and ≈ 0 or negative
    elsewhere.
  - 1993–96 is negative at the wider ticks of the 1990s.
  - Bai–Perron on `diag` stops short of a break (supF(1) p 0.11, UDmax p 0.052), so one crisis episode is not
    enough to call a regime.
  - This repeats U5's finding: the inherited claim was the 2008 crisis.
- **Conditional tables (2016–2025; reported only).** Low VIX (bottom tercile vs its trailing 504 sessions) is the
  one bin with an interval below zero: −2.8 bp/day [−5.0, −0.5]. The high-vol and high-VIX bins are positive but
  wide (Q5 +2.7 [−4.8, +10.3]). The prior-21-session signs point to reversal of the strategy's own P&L and better
  results after down months, none with t > 1.3. Tuesday is U6's split cell again: one of five weekdays.
- **Cross-instrument coherence.** The four streams correlate 0.59–0.90, so they are nearly one bet. Same-sign years
  are 3/3 in 2018, 2021–23 and 2025, which shows the common market path rather than independent confirmation.
- **For H3 on `diag` (the §6.2 routing).** The only hint, high VIX / high vol good and low VIX bad, is in the
  direction U5 found and in the direction of §6.4's B1 favourable set. Its size on 2016–2025 (Q4–Q5 vs Q1–Q3
  ≈ +1.5 vs −1.4, a difference of ≈ 3 bp/day) is far below the H3 MDE (≈ 11 bp/day on the Alpaca window; ≈ 6 on `diag` + Alpaca, §6.6). That
  diagnostic will be underpowered too.
