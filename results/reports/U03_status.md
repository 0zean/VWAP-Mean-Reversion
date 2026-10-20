# U3 status — backtest engine, costs, account; SPEC.md §1–§5

Date 2026-10-20. Branch `unit/03-backtest` (off `dev` @ 190981d), tip 909762d. Windows 11, uv, Python 3.12.
Raw output of the real-data checks: `results/reports/U03_checks.md`. Conventions: `SPEC.md` §1–§5 (on `dev`).

## Decisions

- **§9.4 answered by the user (2026-10-20): margin, $30k, no leverage, shorts allowed.** Registered profile
  `margin_30k` (|exposure| ≤ 1 at every fill and overnight, stricter than Reg-T's 2×); `cash_30k` kept as the
  long-only alternative the check refuses shorts on.
- Taken inside the unit (documented in SPEC.md, reported here for the user):
  - Dividends go to the overnight holder: r_on = (open + D)/close_prev − 1 on raw prints (the holder of record at
    the ex-date open is whoever held at the previous close). Next-open legs use raw prints + `dividend`, as U1's
    handoff asked; adjusted prices are for strategies only.
  - Short-sale proceeds earn nothing; cash earns DTB3 on (1 − |exposure|). A short therefore pays the T-bill in
    excess terms.
  - Missing cross → that session's raw 5Min first open / last close (not `1DayRTH`, which drops the 2019-08-12
    sessions whose last bars are missing), printed and counted; no fallback → raise.
  - `same_close` lives only behind `run_diagnostic` (U5's README reproduction needs it).

## Done-when

- [x] **Invariants (tests, `tests/test_backtest.py`).**
  - Always-long = buy-and-hold at zero cost to 1e-12 under both fills: `test_always_long_is_buy_and_hold_at_zero_cost`
    (independent share ledger, dividends reinvested at the fill print; `moc` also equals the adjusted close path)
    and `test_always_long_on_fixture_is_the_adjusted_close` (real 2024Q1, one ex-date).
  - Sign flip: `test_sign_flip_negates_gross`. Exact negation under `moc`; under `next_open` the exact identity
    g + g′ = 2ab·r_on·r_id and the bound Σ|g + g′| ≤ 2Σ|r_on·r_id|. **The PLAN's bound (factor 1) is wrong**; the
    reviewer confirmed it with a constant ±1 target (0.01076 vs 0.00540). Amended in PLAN §11.
  - Cost booked = Σ|Δf|·notional·rate to the cent on a 1,200-session target with ≥ 1,000 changes, both fills,
    spread + SEC + TAF recomputed independently: `test_cost_booked_matches_independent_sum_to_the_cent`. A long →
    short flip trades 2×: `test_long_to_short_flip_trades_twice`.
  - Random ±1 target: |gross mean| < 2 SE and net − gross = −cost_ret exactly:
    `test_random_target_has_no_edge_and_loses_exactly_the_booked_cost`.
  - `moc` = `next_open` when open_{t+1} = close_t: `test_moc_and_next_open_agree_when_open_equals_previous_close`
    (zero cost and carry; with carry they differ by design, since `moc` holds f_t overnight and `next_open` holds f_{t−1}).
  - Causal under the §8.6 harness for both decision times: `test_strategies_are_causal` (VWAP cell, 16-cell region,
    exo + state strategy, 15:45 snapshot cell) and `test_engine_is_causal_at_both_decision_times`. Mutants caught: a
    one-bar peek, a 15:45 strategy reading close_t (directly and through the engine), future-dividend and
    backward-factor peeks, a 15:45 strategy reading state row t. A planted same-bar fill is caught by the
    fills-after-decision check: `test_fills_after_decision_and_planted_same_bar_fill_caught`.
- [x] **`same_close`** exists, is refused by `run` (the spec path) and by `run_diagnostic` for 15:45 strategies,
      and reproduces a hand-computed 10-session example: `test_same_close_is_unreachable_from_a_spec_fill`,
      `test_same_close_reproduces_hand_computed_example`. U6's runner must re-test this once specs exist.
- [x] **Vectorized = per-session loop** on the fixture quarter: targets via DecisionViews vs `targets()` and the
      full ledger (registered costs, fees, borrow, T-bill), rtol 1e-12: `test_vectorized_equals_loop_on_fixture_quarter`.
      On the frozen vintage (`U03_checks.md` §4–5, residuals only): |Δnet| ≤ 4.4e-16 and every ledger cell
      within 1e-12 rel. for all four symbols × 2,514 sessions; targets identical; 4–5 ms vectorized vs ~100 ms loop.

Other evidence (`U03_checks.md`): every DTB3 value as of 16:25 is visible and ≤ 10 days old; fee-table rows cover
every session; `margin_30k` tradable, `cash_30k` refused for a ±1 target; missing crosses listed per symbol with
the fallback bar's time.

Gate: `powershell -NoProfile -File scripts/done_gate.ps1` → 86 tests pass, ruff check/format clean, `ty` clean
(no ignores added). Package size: 3,052 lines in `vwap_mr/` including docstrings (target ≤ 5,000 non-test lines).

## Looks

None. No strategy P&L was computed on the development window. The checks script uses a constant long, a seeded
random ±1 and a view probe, and prints residuals, counts and runtimes only. The reviewer worked under the same
constraint. K was 1, is 1.

## Adversarial review: BREAKING 0 / SEVERE 0 / MINOR 10 (fixed 9, deferred 1)

| # | finding | disposition |
|---|---|---|
| 1 | metrics only key-checked (4 planted bugs survived) | fixed: `test_metrics_against_hand_values` (Sharpe/Sortino/PSR on excess, beta 0.5 / alpha exact, turnover, flat share) |
| 2 | harness did not perturb dividends; adjusted levels ≤ c embed future dividends | fixed: harness perturbs future dividends and recomputes the factor (and the snap1545 scale) on perturbation and truncation; mutants caught |
| 3 | state row c visible to a 15:45 decision | fixed: state row t is defined as of the close decision; t1545 views get rows ≤ t−1; harness perturbs row c under t1545 |
| 4 | 2019-08-12 `moc` fallback close = the 15:45 snapshot | fixed (counted): `fallbacks["close_seen_at_decision"]`, printed; SPEC §1 requires a registered `moc` run to report it |
| 5 | missing snap1545 row silently gave `snapshot=None` | fixed: raises |
| 6 | TAF-cap path untested; `reprice` lacked `vectorized` | fixed: `test_taf_cap_binds_in_the_loop_and_the_vectorized_path_refuses`; `reprice(vectorized=…)` |
| 7 | PLAN sign-flip bound off by 2 | fixed: PLAN §11 amendment (code was right) |
| 8 | SPEC drift bound only for f ∈ [0, 1]; "share count is held" wrong | fixed: SPEC §3 rewritten (exposure is the state; −1 short drifts ≈ 2|r|; implicit daily rebalance) |
| 9 | SPEC worked example said "no carry" but included borrow | fixed: caption |
| 10 | diag guard bypassable via the `"synthetic"` label | deferred to U5 (documented in SPEC §1): the guard protects `"alpaca"`; U5 builds diag markets from `load_spy_long` frames, which carry `attrs["source"] = "diag"` |

Weakly shown, per the reviewer: `same_close` unreachability (no spec runner yet; U6), `moc` = `next_open` at
zero cost only, the engine causality test runs without truncation (a truncated t1545 market has no close to mark),
and the fixture quarter has no missing cross (fallbacks are covered by synthetic tests and the checks script).

## U1 deferred items picked up

- Sticky diag guard: `Market(source=…)`; `"alpaca"` needs `load_market` provenance (attrs). See #10 for the limit.
- `Exo.asof` staleness: `max_age=` added; `tbill_yield` uses 10 days. U8 sets the VIX bound.
- Next-open legs on raw prints + `dividend`: done.

## Open / proposals (PLAN §11)

- Integer shares and Alpaca's refusal of fractional OPG/CLS orders: a 1/16 region step at $30k is about 3 SPY
  shares, so whole-share rounding is a tracking error the account check could report (not a P&L change).
- The `moc` variant (only if §9.2/§9.3 ask) has one session per symbol filled at a price its decision saw; U6
  decides whether such a session is skipped or kept and reported.
