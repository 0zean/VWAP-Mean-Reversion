# H3_regime_gates — H3 regime predictability (PLAN.md §6.4)

Spec `specs/H3_regime_gates.yaml` @ `bd6e887c429a`; code `2e77cfb98967` (hash `0a4162aef7f7`); data vintage `2026-10` (`a2efbd9fbba4`). Window 2016-01-04 → 2025-12-31, SPY, R₀ at 1.0 bp, close → next_open. Look `U10 run H3_regime_gates@bd6e887c429a` (n 16, 2026-10-21T05:40:44.912077+00:00). **K = 170** (every window); p·K beside every p.

**Registered as a diagnostic** (§11, 2026-10-21): H1 was rejected on this window and its stream was already examined (U6, U7); the expected effect, 3.0 bp/day, is below every headline's MDE. A rejection says little; a pass would need the holdout (U12) and the forward test (U13) to count.

## 1. Headline gates (Holm over the headlines, one-sided H₁: Δμ > 0)

| gate | n | no gate | Δμ bp/day | 90 % lower | μ in | μ out | p one-sided (p·K) | t NW | π realized / registered | episodes | median length | MDE | Holm p | floors (μ in > 0; π drift) | passes |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| B1 | 2513 | 0 | +2.86 | -1.15 | +1.49 | -1.37 | 0.1900 (1.000) | 0.83 | 0.398 / 0.4 | 42 | 12 | 11.6 | 0.7439 | True; True | False |
| B4 | 2513 | 0 | -2.28 | -7.49 | -1.32 | +0.95 | 0.7760 (1.000) | -0.66 | 0.519 / 0.5 | 47 | 3 | 11.4 | 1.0000 | False; True | False |
| B5 | 2513 | 0 | -9.71 | -20.93 | -1.27 | +8.44 | 0.9700 (1.000) | -1.50 | 0.893 / rep. | 25 | 4 | 11.4 | 1.0000 | False; n/a | False |
| R2 | 2513 | 0 | +2.65 | -1.16 | +1.21 | -1.45 | 0.1860 (1.000) | 0.87 | 0.458 / rep. | 85 | 5 | 11.4 | 0.7439 | True; n/a | False |

**Verdict: 0 of 4 headline gates pass** — diagnostic (pass: Holm p < 0.05, in-gate mean > 0.0, and π within ± 0.05 of its registered value where one is registered). Ungated R₀: -0.23 bp/day net excess over 2513 sessions. Return row t is conditioned on the gate at decision t − 1; MDE at the registered π (0.5 where π is only reported; Holm k = 4 for headlines) and the registered σ_d 92.4 bp, on the gate's n; registered lines {'B1': 11.6, 'B4': 11.37, 'B5': 11.37, 'R2': 11.37}.

## 2. Variant gates (reported, not in the Holm step; E1–E4 under-powered, U8)

| gate | n | no gate | Δμ bp/day | 90 % lower | μ in | μ out | p one-sided (p·K) | t NW | π realized / registered | episodes | median length | MDE |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| B2 | 2513 | 0 | +1.84 | -3.08 | +0.99 | -0.85 | 0.3137 (1.000) | 0.48 | 0.338 / 0.33 | 80 | 2 | 9.7 |
| B3 | 2513 | 0 | -2.25 | -6.70 | -0.92 | +1.33 | 0.7748 (1.000) | -0.66 | 0.693 / rep. | 53 | 9 | 9.2 |
| B6 | 2451 | 62 | -2.59 | -8.32 | -1.59 | +1.00 | 0.7678 (1.000) | -0.74 | 0.526 / rep. | 91 | 2 | 9.3 |
| E1 | 2513 | 0 | -0.85 | -4.45 | -0.73 | +0.12 | 0.6203 (1.000) | -0.28 | 0.406 / 0.5 | 70 | 2 | 9.2 |
| E2 | 2513 | 0 | -3.23 | -7.28 | -1.72 | +1.51 | 0.8762 (1.000) | -1.07 | 0.538 / 0.5 | 199 | 2 | 9.2 |
| E3 | 2513 | 0 | +1.00 | -2.29 | +0.29 | -0.71 | 0.3575 (1.000) | 0.34 | 0.483 / 0.5 | 23 | 8 | 9.2 |
| E4 | 2513 | 0 | -0.36 | -4.35 | -0.43 | -0.07 | 0.5431 (1.000) | -0.12 | 0.441 / 0.5 | 109 | 3 | 9.2 |
| R1 | 2513 | 0 | +1.32 | -4.02 | +0.71 | -0.60 | 0.3781 (1.000) | 0.31 | 0.285 / rep. | 88 | 4 | 9.2 |
| R3 | 2513 | 0 | -4.65 | -14.19 | -0.81 | +3.84 | 0.7592 (1.000) | -0.64 | 0.876 / rep. | 38 | 36 | 9.2 |
| R2_refit63 | 2513 | 0 | +1.38 | -2.35 | +0.50 | -0.88 | 0.3139 (1.000) | 0.46 | 0.470 / rep. | 85 | 5 | 9.2 |
| R2_fit756 | 2513 | 0 | -3.41 | -6.88 | -1.97 | +1.44 | 0.9046 (1.000) | -1.16 | 0.489 / rep. | 89 | 4 | 9.2 |
| R2_fit1512 | 2513 | 0 | -0.83 | -4.89 | -0.71 | +0.12 | 0.6069 (1.000) | -0.27 | 0.421 / rep. | 89 | 4 | 9.2 |

## 3. Economic version: the gated stream (target × gate, flat off-gate, registered costs)

| gate | bp/day | %/yr | vol %/yr | %/yr at ungated vol | Sharpe | ΔSharpe vs ungated (LW) | p one-sided (p·K) | round trips/yr | flat share |
|---|---|---|---|---|---|---|---|---|---|
| B1 | +0.58 | +1.47 | 12.2 | +1.66 | 0.12 | +0.16 | 0.1178 (1.000) | 30.4 | 0.611 |
| B4 | -0.25 | -0.64 | 8.4 | -1.04 | -0.08 | -0.03 | 0.5529 (1.000) | 35.2 | 0.489 |
| B5 | -1.06 | -2.68 | 12.6 | -2.92 | -0.21 | -0.17 | 0.9724 (1.000) | 60.8 | 0.125 |
| R2 | +0.55 | +1.39 | 11.5 | +1.66 | 0.12 | +0.16 | 0.1398 (1.000) | 34.3 | 0.553 |
| B2 | +0.48 | +1.22 | 12.1 | +1.39 | 0.10 | +0.14 | 0.1424 (1.000) | 27.7 | 0.671 |
| B3 | -0.74 | -1.86 | 11.4 | -2.24 | -0.16 | -0.12 | 0.7860 (1.000) | 48.6 | 0.321 |
| B6 | -0.39 | -0.98 | 9.5 | -1.41 | -0.10 | -0.06 | 0.6057 (1.000) | 39.0 | 0.497 |
| E1 | -0.06 | -0.15 | 8.7 | -0.24 | -0.02 | +0.02 | 0.4507 (1.000) | 30.5 | 0.601 |
| E2 | -0.83 | -2.09 | 10.0 | -2.87 | -0.21 | -0.17 | 0.8100 (1.000) | 42.8 | 0.471 |
| E3 | +0.20 | +0.49 | 10.4 | +0.65 | 0.05 | +0.09 | 0.3229 (1.000) | 32.2 | 0.527 |
| E4 | +0.38 | +0.97 | 8.7 | +1.54 | 0.11 | +0.15 | 0.2440 (1.000) | 33.3 | 0.568 |
| R1 | +0.25 | +0.64 | 11.6 | +0.76 | 0.06 | +0.10 | 0.2823 (1.000) | 25.7 | 0.720 |
| R3 | -1.04 | -2.61 | 10.6 | -3.41 | -0.25 | -0.21 | 0.8988 (1.000) | 58.4 | 0.142 |
| R2_refit63 | +0.06 | +0.15 | 11.5 | +0.18 | 0.01 | +0.06 | 0.3687 (1.000) | 35.2 | 0.542 |
| R2_fit756 | -0.59 | -1.48 | 11.0 | -1.84 | -0.13 | -0.09 | 0.7155 (1.000) | 35.2 | 0.522 |
| R2_fit1512 | +0.04 | +0.09 | 10.2 | +0.12 | 0.01 | +0.05 | 0.3727 (1.000) | 32.3 | 0.589 |

Ungated: Sharpe -0.04, -0.58 %/yr, vol 13.8 %/yr. A gate with no information on a share π of sessions has Sharpe ≈ √π × the ungated one (§6.4), so ΔSharpe < 0 is the null's expectation, not a finding. The gated stream holds row t's overnight leg on the gate at t − 2 (it was decided then) while Δμ credits gate t − 1 with all of row t: the two differ at gate transitions, so an effect in the overnight leg can show in Δμ and less here.

## 4. Incremental value given B1 + B4 (Newey–West, 5 lags)

| gate | n | β bp/day | t NW |
|---|---|---|---|
| B5 | 2513 | -9.57 | -1.49 |
| E1 | 2513 | -2.07 | -0.63 |
| E2 | 2513 | -2.71 | -0.93 |
| E3 | 2513 | +1.16 | 0.40 |
| E4 | 2513 | -0.70 | -0.22 |
| R1 | 2513 | -0.33 | -0.08 |
| R2 | 2513 | +2.24 | 0.76 |
| R3 | 2513 | -4.26 | -0.60 |

## 5. Regime refits over the window (R2: φ ordering; R1: σ ordering)

| model | refits | not converged | degenerate | swap flag (|Δφ| < 0.05) | skipped | median low / high | median separation (10 % quantile) |
|---|---|---|---|---|---|---|---|
| R1 | 121 | 0 | 0 | — | 0 | 0.392 / 2.917 | 2.508 (1.543) |
| R2 | 121 | 0 | 0 | 103 | 0 | 0.648 / 0.677 | 0.028 (0.005) |
| R2_refit63 | 41 | 0 | 0 | 36 | 0 | 0.649 / 0.676 | 0.029 (0.005) |
| R2_fit756 | 121 | 1 | 3 | 79 | 0 | 0.640 / 0.682 | 0.036 (0.004) |
| R2_fit1512 | 121 | 0 | 0 | 113 | 0 | 0.651 / 0.671 | 0.019 (0.004) |

U9's reading: on regime-free data R2 calls 'fast' 40 % of the time with 78 % degenerate refits, and its two φs sit near the random-walk 0.71 (gap often < 0.05). An R2 gate is meaningful only if the separation is wide and the degenerate count low.

## 6. Reading (hand-written after the run; not regenerated by the script)

- **H3 is a diagnostic null: 0 of 4 headline gates pass.** The smallest Holm p is 0.74 (B1 and R2). Every p·K is
  1.0 at K = 170.
- **B1 (high realized vol) points the expected way, at the expected size, far below the MDE.**
  - Δμ is +2.86 bp/day (90 % lower bound −1.15, p 0.19), against the registered 3.0 bp/day from U7 and an MDE of
    11.6. In-gate +1.49, out-of-gate −1.37.
  - π held its registered 0.40 (realized 0.398), with 42 episodes of median length 12.
  - The gated stream earns Sharpe 0.12 against the ungated −0.04 (ΔSharpe +0.16, p 0.12).
  - This is the U5 / U7 "the edge lives in high vol" story once more. It is consistent with a small effect, but the
    window cannot separate it from zero.
- **R2 did not find reversion-speed regimes on SPY's d*.**
  - Over the window, its two fitted φs sit at 0.648 / 0.677 (median). Both are near the random-walk value of 0.71
    (U9), the median separation is 0.028, and 103 of 121 refits carry the swap flag (|Δφ| < 0.05).
  - There are 0 degenerate refits: the two regimes are real, but they split by variance (σ²), not by φ.
  - With φs that close, which regime is labelled "fast" is close to arbitrary at each refit. So R2's +2.65 bp/day
    (p 0.19) is not evidence about reversion speed. Its incremental t given B1 + B4 is 0.76.
  - The three hyper-parameter variants agree (separation 0.019–0.036; Δμ −3.4 to +1.4).
- **B4 (rolling φ below its median) has the wrong sign** (−2.28, p 0.78). Measured reversion speed does not
  condition the rule's return either.
- **B5's favourable set is the base state.**
  - VR(5) over 252 sessions is below 1 on 89 % of sessions (π reported, not registered). Short-horizon reversal
    in SPY was nearly continuous through 2016–2025.
  - Its few "VR ≥ 1" stretches (25 episodes) are the better ones: Δμ −9.7, the wrong sign, p 0.97. The incremental
    t is −1.49.
- **Variants (reported only).**
  - The under-powered estimators E1–E4: |t| ≤ 1.1.
  - R1, the HMM on standardized returns, finds clean σ regimes: 0 degenerate refits, σ² 0.39 vs 2.92. Its Δμ is
    +1.3 bp/day; it is a noisier B1.
  - B2 (high VIX): +1.8.
  - R3's stability gate is on 88 % of sessions: −4.7.
  - B6 (the strategy's own momentum): −2.6.
  - None has an incremental t beyond ±1.5 given B1 + B4.
- **What this means for the program.**
  - Under §6.6's power table this result was expected: the one effect seen before (≈ 3 bp/day) sits at a quarter of
    the MDE.
  - No gate improves on the vanilla rule on the development window. The best gated stream (B1, Sharpe 0.12) would
    still need the holdout and a forward test to count.
- **For U11 (H4).** The amended routing makes R2's P(fast) the blend weight. On this window, that probability does
  not track reversion speed; its two regimes are variance regimes with nearly equal φ. An R2-weighted blend
  therefore tests a variance-regime weight under another name. This is a question for the user before U11 is
  registered (U10 status note).
