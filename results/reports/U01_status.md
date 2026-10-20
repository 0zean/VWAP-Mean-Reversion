# U1 status — data layer: Alpaca cache, prints, calendar, exogenous series, `diag` history

Date 2026-10-20. Branch `unit/01-data` (off `dev` @ 7b22b31), tip 69e6b7d. Windows 11, uv, Python 3.12.
Full check output: `results/reports/U01_checks.md`. Quality table: `results/data_quality.csv` (on the unit branch).

## Decisions taken with the user during U1 (recorded in PLAN.md §9.1 and §11)

- §9.1: **Yahoo chart API** for `diag`. Stooq now serves a JavaScript bot check and can't be fetched by a
  script. DTB3 comes from the **FRED API** (`FRED_API_KEY`), because fredgraph.csv times out from this machine.
  The vintage is **`vintage_2026-10`**.
- **Amendment (inside U1):** the live smoke showed that Alpaca's `1DayPrint` open and close are not the auction
  prints on many 2016–2022 sessions, and that Alpaca's `adjustment=all` mishandles some dividends. Two changes
  follow:
  - Prices now come from `1DayAuction`: the listing exchange's crosses taken from SIP trades.
  - Total-return adjustment now comes from our own reconciled dividend table.
  - PLAN.md §4, §5, §6.1, §7 U3/U13 and §8.1–8.2 were amended on `dev` @ b759d63.

## Done-when

- [x] **Tests (mocked client)**: 43 pass with no network. They cover each item below:
  - **tz conversion:** `test_bars_from_raw_is_utc_and_bar_open`.
  - **RTH filter:** on a half-day, a holiday and both DST transitions (`test_rth_filter_regular_half_day_holiday_dst`).
  - **Resample parity:** checked against hand-computed bars (`test_1dayrth_matches_hand_computed_bar`, snapshot tests).
  - **Cache:** round-trip with a hash mismatch raising, and a swapped parquet+sidecar pair refused by the manifest.
  - **Top-up:** no duplicates (`test_top_up_without_duplicates`).
  - **Re-adjustment:** triggers a full re-fetch.
  - **Holdout refusal:** `load_bars` and `load_series` refuse, and `allow_forward` works only on `live`, with an audit row.
  - **`available_at`:** at both decision times (VIX 16:20 vs 16:25 / 15:45, early close 13:25; DTB3 H.15 next business day 16:30).
  - **`diag` rows** are refused by the cost model, including after `attrs` are lost.
  - **Splice:** refuses a drifting or re-scaled source.
  - **Since added:**
    - auction-cross selection (decoys, unflagged trades, Q/M official reports, Nasdaq T→Q code change);
    - missing-cross logging and the refill command;
    - dividend reconciliation (tolerance, one-sided, overrides, stale overrides, splits refused);
    - the exact total-return factor;
    - the 5Min family sharing the auction factor;
    - snapshot causality with a planted 5-minute-late mutant caught;
    - a frozen vintage that is read-only;
    - the fixture quarter's invariants.
- [x] **Live smoke (recorded)**:
  - **Fetch:** four symbols into `vintage_2026-10`, frozen, `data_hash a2efbd9f…674c` (23 files); `diag` cache frozen, `fe8ff374…7cf`.
    `python -m vwap_mr.data verify` passes for both, and a top-up request on the frozen vintage is refused.
  - **Quality report:** for every symbol and timeframe, **0 duplicates, 0 OHLC violations, 0 missing sessions, 0 off-calendar**.
  - **5Min missing bars:** 14 to 17 of 195,336 per symbol, and one session per Arca-listed symbol has no last bar.
    That session is 2019-08-12: Alpaca's SIP has no SPY/IWM/DIA trades from 15:35 to 16:00 that day (CTA tape).
    QQQ, on the UTP tape, is complete. `1DayRTH` drops the session and logs it.
  - **`1DayAuction` missing crosses** (NaN, counted, never filled):
    - SPY: open 2023-06-05, close 2019-08-12.
    - QQQ: open 2020-03-16. The market-wide circuit breaker at the open delayed the Nasdaq cross past the 15-minute window.
    - IWM: opens 2021-09-02, 2021-09-14, 2023-06-05; closes 2017-03-20, 2019-08-12.
    - DIA: open 2023-06-05; closes 2017-03-20, 2019-08-12.
    - After the code-set fix, no cross needed the wide window.
  - **`1DayPrint` volume / RTH 5Min sum (median, p1–p99):**
    - SPY 1.19 (1.09–1.54); QQQ 1.12 (1.05–1.28); IWM 1.12 (1.05–1.42); DIA 1.05 (1.01–1.33).
    - **Convention: confirmed.** The print volume is consolidated and includes extended hours and the closing cross. The RTH 5Min sum excludes the cross, because the 16:00 cross falls in the 16:00 bar. The cross's median share of print volume is 1.8 % for SPY, 0.9 % QQQ, 1.8 % IWM, 0.9 % DIA.
    - Proposal in §11: the RTH-volume variant should add the cross size.
  - **`1DayPrint` close vs last RTH 5Min close (raw):**

    | symbol | median abs, bp | p99 abs, bp | max abs, bp | date of max |
    |---|---|---|---|---|
    | SPY | 0.80 | 8.3 | 288 | 2020-03-12 |
    | QQQ | 0.97 | 10.9 | 83 | 2020-03-13 |
    | IWM | 1.33 | 11.3 | 214 | 2020-03-16 |
    | DIA | 0.77 | 6.3 | 231 | 2020-03-16 |
  - **`1DayPrint` vs the official close (Yahoo):** they differ by more than 1.5¢ on SPY 924, IWM 693, DIA 327 and QQQ 29 sessions. All of these are 2016–2022; from 2023 the two agree.
    The QQQ `1DayPrint` open differs on 752 sessions (2016–2020).
  - **`1DayAuction` close vs Yahoo:** they agree to 1 bp on every session from 2017 for SPY and DIA; in 2016 they differ on 102 and 49 sessions.
    **`1DayAuction` open vs `1DayPrint`/Yahoo open:** they differ by more than 1 bp on 50 to 150 sessions a year. The print open is the first trade; the cross is the price an OPG order gets.
- [x] **`diag` overlap**:
  - The splice is raw vs raw: `1DayAuction` close vs Yahoo close over 2,512 sessions.
  - Median ratio 1.000000. Yearly median deviations are below 1.2e-8. Daily |dev| has median 0.0002 bp, p99 2.9 bp and max 11.4 bp (2016-05-31).
  - **Adjusted close correlation 0.99999996 (> 0.9999).** Yahoo's and our dividend chains differ by at most 0.11 % in level.
  - **Adjustment conventions** (secondary.py and dividends.py docstrings):
    - Diag prices before 2016-01-04 carry Yahoo's dividend factor, chained to our factor at DEV_START.
    - From DEV_START, prices use the reconciled table with the exact total-return step P_e/(P_e+D).
    - `close_split` carries Yahoo's split-adjusted close (raw for SPY), then the raw cross (U5's VWAP.R emulation).
    - Volume: `1DayPrint`/Yahoo median 1.015.
- [x] **No secret in logs**:
  - 84 run logs, task outputs, sidecars and the quality CSV were scanned for the `.env` values: 0 hits.
  - `git grep -I -E "(PK|SK)[A-Z0-9]{16,}"` is empty.
  - The FRED key travels as a query parameter, and `http_get` strips the query string from every error; tested with a mocked failure.

Gate: `scripts/done_gate.ps1` → pytest 43 passed; ruff check clean; ruff format clean (58 files); ty clean.
Non-test package lines: 2,185 (budget 5,000).

## Dividend reconciliation (2016-01-04 → 2025-12-31)

- **Ex-dates:** SPY 40, QQQ 41 (one special), IWM 40, DIA 120.
- **Tolerance:** 6e-4 $, because Yahoo rounds to 3 decimals.
- **Overrides:** ten disagreements were resolved in `data/dividends_overrides.csv` with a cited third source:
  - dividendvision.com for SPY and DIA, api.nasdaq.com for QQQ.
  - Alpaca missed 5 (SPY 2016-03-18, 2018-06-15, 2025-12-19; DIA 2016-03-18, 2025-12-19).
  - Alpaca doubled QQQ 2022-09-19 and had QQQ 2018-06-18 wrong.
  - Yahoo missed QQQ 2020-09-21.
  - The two sources differ on 2021-12-17 for SPY and DIA; the third source agrees with Alpaca.
  - IWM needed no override.

## Looks

K is still 1. No look was taken: U1 measured prints, volumes and adjustment factors only. It computed no strategy
P&L and no return statistic.

## Adversarial review (one pass, run before the final fetch)

BREAKING 0 / SEVERE 1 / MINOR 6. Fixed 3, deferred 3 with reasons.

- **SEVERE, fixed (c3bdb77).**
  - 5Min, 1DayRTH and snap1545 used Alpaca's dividend adjustment, which is wrong on 4 ex-dates. That would have offset `snap1545`/RTH-VWAP variants by 30–100 bp against `1DayAuction` closes.
  - The 5Min family is now cached raw and scaled by the session's reconciled `1DayAuction` factor (tested). The vintage holds raw 5Min only; the `sip/all/5Min` cache was deleted before the freeze.
- **MINOR, fixed:** research reads of the `live` cache built factors over holdout rows. They are now cut at HOLDOUT_START unless `allow_forward`.
- **MINOR, fixed:** PLAN citations for the amendment were missing. They were added on `dev`, and the stale `1DayPrint` comment in alpaca.py was corrected.
- **MINOR, fixed:** the fixture quarter was missing. Added: `tests/fixtures/spy_2024q1.parquet` (5Min raw RTH) and `spy_2024q1_daily.parquet` (adjusted `1DayAuction` + `1DayPrint` volume).
- **MINOR, documented, not changed:** the "exact total return" holds close-to-close only. An adjusted open-to-open return across an ex-date is off by about 0.25 bp (4 bp worst). The docstring now tells U3 to build next-open legs from raw prices plus `dividend`.
- **MINOR, deferred to U3:** the diag guard is not sticky. A new DataFrame built from a diag column passes `check_costable`. U3's engine must cost only frames from `load_bars`, or carry `is_diag`.
- **MINOR, deferred to U3/U8:** `Exo.asof` has no staleness bound. A missing VIX day silently serves the previous close. It should get a max-age check against the calendar when the first state feature uses VIX.

Found after the review, during the live fetch, and fixed:
- **`Q`/`M` official reports.** When the cross print is absent, the listing exchange's official open/close report (`Q`/`M`) is accepted; SPY 2025-09-05 is an example.
- **Nasdaq's exchange code.** Alpaca reports Nasdaq's crosses as `T` until mid-2023 and as `Q` after, so a listing exchange is now a set of codes. Without this fix QQQ had about 760 sessions without crosses.
- **Refill.** `python -m vwap_mr.data refill` re-queried the missing sessions before the freeze.

## Deferred / open

- **§9.4 is still unanswered** (it blocks U3 and U13).
- **Inherited from U0:** the Windows `PermissionError` on reading during an append (revisit in U6).
