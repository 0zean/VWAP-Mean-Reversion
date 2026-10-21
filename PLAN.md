# PLAN — VWAP mean reversion, rebuilt as a tested, regime-aware research program

Date: 2026-10-20 (v1, after one adversarial review of the draft; §11). This file is the working source of truth
for the repository: thesis, protocol, work breakdown, decisions and status. Interfaces that stabilise move to
`SPEC.md` (created in U3/U4); numbers live in `results/`; this file says what is being built, why, in what order,
and what "done" means for each unit.

Method reference: [0zean/wavelet-meta-model](https://github.com/0zean/wavelet-meta-model) (`PLAN3.md` §4,
`SPEC.md` §5–§9 and §20–§22, `REVIEW.md`). What is carried over, what is changed, and which modules are ported
is in §3.

---

## 0. How to use this file between sessions

**Starting a session.**
1. `git checkout dev && git pull`. Read §10 (status table) and the newest entry of §11 (handoff notes). If a unit
   branch is open, its handoff note names it; check it out.
2. Pick the first unit in §7 whose dependencies are complete and whose §9 decisions are answered (or have a
   default you may use). Create `unit/NN-slug` off `dev` for code. **PLAN.md, SPEC.md, `results/ledger.jsonl`,
   `results/looks.jsonl` and `results/reports/*.md` are edited on `dev` directly** in small commits, so the next
   session sees them without a merge.
3. Read the unit's Goal, Scope, Done-when, Reviewer focus. Do not widen the scope; write anything new you find
   into §11 as a proposal.

**Finishing a unit.**
1. Done-gate: `uv run pytest -q` green; `uvx ruff check . && uvx ruff format --check .` clean; `uv run ty check`
   clean (or each ignore justified in the status note); every Done-when box ticked with the command and its
   output recorded in `results/reports/UNN_status.md`.
2. One `adversarial-reviewer` pass scoped to the unit's diff and claims. Fix every BREAKING/SEVERE finding; fix
   or explicitly defer (with reason, in the status note) each MINOR.
3. Update the unit's **Status** line in §7 and the row in §10 (one line each; detail stays in the status note).
   Append a handoff note to §11 using the template at the end of §11. Stop and report. The user decides on merge
   and on starting the next unit.

**Cross-cutting rules (every unit).**
- **No network in tests.** Alpaca, FRED, CBOE and the secondary source are mocked or replaced by fixtures.
- **No silent fallbacks.** Missing data, a refused request, a failed fit, an empty window: raise, or log a counted
  skip that appears in the report. Never forward-fill a price, never substitute IEX for SIP, never swallow an
  exception around an order or a position query.
- **Causality is tested, not asserted.** Every feature, state estimator, regime model, strategy, gate, blend and
  the engine passes the perturbation test at the *decision time* (§8.6) and a "fills after its decision" test;
  each unit adds a mutant test proving the harness catches a planted one-bar peek.
- **Every look is counted, and the runner enforces it.** A development-window result is rendered only after
  `look` has appended a row to `results/looks.jsonl` (label, n configurations, git SHA). The report prints K and
  the Bonferroni bound p·K beside every bootstrap p. Mechanism diagnostics count; strategy P&L on the Alpaca
  development window is **not read before U6** except for the single inherited cell in U5 (§7).
- **Pre-registration is a file.** A hypothesis runs only from a committed `specs/<id>.yaml` whose git SHA is
  recorded in the ledger; the runner refuses an uncommitted or dirty spec. Amendments are new files with a reason.
- **The holdout is locked** (§4). Research code cannot read bars on or after `HOLDOUT_START`; `live/` reads them
  after the freeze date with an audit row per session.
- **Secrets** live in `.env` only (`ALPACA_API_KEY`, `ALPACA_API_SECRET`), read in one module, never logged.
- **Keep the harness small.** Target ≤ 5,000 non-test lines. Port named functions (§3.3), not frameworks. No
  machine learning until a configuration is "consistent" in the forward test (U15).

---

## 1. Where we start: the inherited strategy and what is wrong with it

### 1.1 What `VWAP.R` does (read against the code, not the README)

- `getSymbols("SPY", src = "yahoo")` with **no `from=`**: `startDate` is defined on line 16 but never used, so
  quantmod's default start of **2007-01-01** applies. The README's figures are a 2007 → download-date sample that
  includes 2008–09. `Cl()` is Yahoo's split-adjusted close (not dividend-adjusted); ex-dividend days (about four a
  year, 0.3–0.5 %) appear as price drops and generate spurious long signals.
- `vwap = TTR::VWAP(Cl, Vo, n = 3)` = Σ close·volume / Σ volume over the last three sessions: a volume-weighted
  moving average of closes, **not** the intraday execution VWAP.
- `signal = close / vwap`; long if `< 0.999`, short if `> 1.001`, else flat; `NA → 1 → flat`.
- `strategyReturns = trade × lag(dailyRet, −1)`: the position decided at close[t] is assumed filled **at close[t]**
  and held to close[t+1]. No costs. `SharpeRatio.annualized` with PerformanceAnalytics defaults (scale 252, Rf 0).
- README: annualized Sharpe 0.468 vs 0.314 for SPY, "does better under a down market regime".

### 1.2 What it really is

With a ±0.1 % band around a 3-day volume-weighted average, the rule holds a position on most days: it is a
**one-day-hold reversal rule on the index**, `position ≈ sign(VWAP₃ − close)`. The relevant literature is
time-series autocorrelation of index returns, not cross-sectional stock reversal: Campbell, Grossman & Wang (1993)
tie index-return autocorrelation to volume (the mechanism a *volume*-weighted anchor is implicitly using);
daily index autocorrelation turned negative in the 2000s and is strongest in high-volatility periods; Nagel
(2012) reads short-term reversal returns as compensation for liquidity provision and shows VIX predicts them
(his evidence is cross-sectional; the state dependence is what transfers). This is consistent with the README's
"down market" observation and with a 2007-start sample that includes 2008–09. So the question is not "is the
idea sane" but "is it still there net of realistic execution, and when".

### 1.3 Defects (ranked by how much they change the answer)

1. **Fill timing.** The signal needs close[t] and the session's full volume; the backtest fills at close[t]. That
   is only executable as a market-on-close order decided from a pre-close snapshot. The Python port's `live.py`
   runs after the close and fills at the **next open**. Backtest and live disagree, and for a one-day reversal
   the overnight gap is a large part of the return.
2. **Costs.** The R script has none. The port charges a flat 5 bp per side (`fees=0.0005`), which is 5–10× the
   realistic auction cost on SPY and would itself decide the result. Neither models borrow, regulatory fees on
   sells, or the yield on cash.
3. **No uncertainty.** A Sharpe of 0.47 over about twelve years has a standard error of roughly 0.28; the
   difference to SPY's 0.31 is not distinguishable from zero. Parameters were chosen on the full sample. How much
   of the 0.47 is 2008–09 is never asked.
4. **Python port (branch `dev`, `trading/`).** `vectorbt.Portfolio.from_signals` fills at the signal bar's close
   (same-bar fill); `signals.py` carries stray imports (`matplotlib.pylab.roll`, `pygments.lexer.default`,
   `pylab.sign`); `config.py` says Alpaca's free plan has ~5 years of history (SIP history runs from 2016);
   `data.py` uses `end = datetime.now()` and can include an unfinished session; `live.py` deploys 95 % of equity,
   shorts SPY (needs a margin account), and `get_current_position` swallows every exception and returns 0 — the
   kind of silent fallback §0 bans. `vectorbt` goes (user decision). No tests, no cache, no ledger.
5. **`.gitignore` ignores `*.yaml`**, which would hide the spec files this plan introduces. Fixed in U0.

The port is deleted in U0 (its content is recorded here and in git history); the R logic is re-implemented from
`VWAP.R` in U5 and the README figure reproduced from 2007 by sub-period.

---

## 2. Thesis and the questions this program answers

> A one-day reversal rule on liquid index ETFs, expressed as a VWAP-deviation **region** (not one parameter
> triple), decided at the close and filled at an auction print, costed at realistic fills, has an edge that is
> **state-dependent**: it earns in high-volatility, fast-reverting states and roughly nothing otherwise. A causal
> state estimate can therefore (a) gate the rule and (b) weight it against a trend rule, and a regime-weighted
> blend should beat the static blend. The edge's persistence is a quantity to be estimated and tracked, not
> assumed.

| id | question | unit | headline statistic (§6) |
|---|---|---|---|
| **H1 existence** | Does the VWAP-MR region earn positive net excess return on SPY 2016–2025 at realistic fills? | U6 | mean daily net excess return vs 0, one-sided block bootstrap |
| **H2 persistence** | Is the edge stable or decaying, and how likely is it to be present today? | U7 | rolling t, Bai–Perron break p, Kalman P(μ_now > 0) |
| **H3 predictability** | Does a causal state variable (vol, reversion speed, short-scale Hurst, regime model) predict when the rule earns, beyond cheap baselines? | U10 | conditional mean difference, favourable vs not, one-sided; Holm over 4 |
| **H4 combination** | Does a regime-weighted MR + trend blend beat the static 50/50 blend? | U11 | paired Ledoit–Wolf Sharpe difference vs the static blend |
| **H5 forward** | Does the registered configuration behave as registered in paper trading? | U13 | sign, t > 0 and inside the registered interval after 26 weeks |

A null on any of these is a valid, reportable outcome. H3 and H4 are the research contribution; H1 and H2 make
them meaningful; H5 is the only holdout nobody can have seen. **Out of scope** until a gate opens: single stocks,
options, intraday families (U14), machine learning (U15), parameter optimisation of any kind.

---

## 3. Principles, carried over and adapted

### 3.1 Kept from wavelet-meta-model (and why)

- **Mechanism first, no screening.** Its 1,402-trial screen produced fourteen noise draws; its one pass came from a
  pre-registered region with a stated mechanism.
- **Headline = region, never a point; variants on a specification curve; splits reported, never selected.**
  Parameter selection had negative expected value in its RMV replication; an equal-weight region cannot overfit
  by construction. Coherence = share of cells with the headline's sign.
- **Average, don't select**, across window lengths, hazard rates and the like; selection among near-identical
  configurations is what produced PBO 0.6–0.8 there.
- **One execution convention** shared by strategies, backtest and live loop; auction fills priced at the official
  prints from Alpaca's daily bar.
- **Costs as a curve** (0.3 / 1.0 / 2.3 bp round trip, then `measured`), a per-session cost ledger for re-pricing,
  cash earning the T-bill, every statistic on excess returns.
- **Power before registration.** Each spec writes its minimum detectable effect; an expected effect below it is a
  *diagnostic* and says so.
- **Statistics:** studentized circular block bootstrap, Newey–West t, Ledoit–Wolf paired Sharpe difference, PSR,
  MinTRL, Holm across ≤ 4 simultaneous hypotheses, looks counted with a Bonferroni bound.
- **Forward paper test as the real holdout**, with fill reconciliation and a BOCPD monitor on live P&L.
- **Account realism** reported as "tradable or not", never changing a P&L.
- **Engineering:** one `load_bars` entry point; cache with hashed sidecar and atomic writes; re-adjustment
  detection on top-up; exchange calendar; point-in-time `available_at` on exogenous rows; frozen config validated
  at load; cell hash = spec + config + code hash + data hash; append-only ledger with fsync, **committed to git**.

### 3.2 Changed, and why

- **Daily first.** H1–H4 run on daily bars; a family test is seconds. No PWFO engine, no process pool, no feature
  cache, no 100-field config.
- **A long diagnostic sample.** Ten years of daily data can only confirm a Sharpe ≳ 0.8 (§6.6); a daily reversal
  rule is plausibly 0.3–0.5. U1 adds a free daily source back to 1993 for *diagnostics, persistence and estimator
  warm-up*, never costed as Alpaca fills and never in a registered headline; H1 uses a three-tier verdict (§6.2).
- **Regime models are the object of study.** They get a noise study (U8) and a detection study (U9) on synthetic
  series with known truth, and their configurations are frozen there, before any market P&L is read.
- **Region and execution are fixed in this file, from the mechanism**, not chosen after reading the development
  window (the draft's U4 would have done exactly that; §11).
- **No DSR, PBO or Benjamini–Hochberg columns.** With a handful of pre-registered trials they add nothing a Holm
  step and a looks count do not; the sibling repo retired DSR from its verdict for the same reason.
- **Tests named by module**, not by unit.

### 3.3 Modules to port from wavelet-meta-model (verified in its tree on 2026-10-20)

Copy the function and its tests; rename to this package; keep the citation in the docstring.

| theirs | ours | what |
|---|---|---|
| `data/alpaca_source.py` (`AlpacaSource`, `bars_from_raw`, `_credentials`) | `vwap_mr/data/alpaca.py` | SIP bars; Alpaca's inclusive `end` trimmed to `[start, end)`; credentials from `.env` only |
| `data/store.py` (`atomic_write`, `merge_bars`, sidecar hash) | `vwap_mr/data/store.py` | cache with sha256 sidecar, overlap re-fetch, re-adjustment detection |
| `data/bars.py::load_bars`, `data/sessions.py` (`rth_filter`, `resample_daily`, `drop_sessions_missing_last_bar`), `data/timeframes.py` (`1DayPrint`) | `vwap_mr/data/bars.py` | the one entry point; RTH filter; auction prints (amended: `1DayAuction` from SIP trades, §11) |
| `data/exo.py::load_series` + `available_at`; `features/exo_align.py::Exo` | `vwap_mr/data/exo.py` | VIX (CBOE, 16:20 ET), DTB3 (FRED H.15 rule) |
| `data/quality.py` | `vwap_mr/data/quality.py` | missing / duplicate / OHLC / off-calendar report |
| `validation/stats.py` (`psr`, `min_track_record_length`) | `vwap_mr/stats/sharpe.py` | with the Bailey–López de Prado test vector |
| `families/stats.py` (`sharpe_test`, `mean_test`, `circular_block_indices`, `hac_bartlett`, `newey_west_alpha`, `holm`) | `vwap_mr/stats/bootstrap.py`, `multiplicity.py` | the H1/H3/H4 tests |
| `families/power.py` (`mde_alpha`, `mde_delta_sharpe`, `simulate_power`) | `vwap_mr/stats/power.py` | MDE line in every spec |
| `families/looks.py` (`record_look`, `looks_total`, `bonferroni`) | `vwap_mr/experiments/looks.py` | the looks ledger |
| `families/spec.py::register`, `check_registered` | `vwap_mr/experiments/spec.py` | spec committed and clean at a recorded SHA |
| `wfo/position_backtest.py::position_backtest` | `vwap_mr/backtest/engine.py` | vectorized position-series backtest (numpy; numba only if needed) |
| `risk/costs.py::fill_costs`, `risk/account.py::check_account` | `vwap_mr/backtest/costs.py`, `account.py` | cost by fill class; margin/cash and Reg-T flags |
| `features/causal_modwt.py` | `vwap_mr/regime/wavelet.py` | causal MODWT pyramid (no circular wrap) for the wavelet-H estimator |
| `features/kernels.py::session_vwap` | `vwap_mr/features/vwap.py` | session VWAP (snapshot, U14) |
| `tests/test_methodology.py::perturb_after` + the "catches a planted leak" mutants | `tests/causality.py` | the causality harness |
| `experiments/ledger.py::Ledger` | `vwap_mr/experiments/ledger.py` | append-only JSONL with lock and fsync |

Not ported: PWFO, model zoo, sizers, the portfolio simulator's event loop, feature registry and cache, the
families runner, fracdiff, Goertzel, quotes and option samplers, PBO.

---

## 4. Data, windows and instruments

- **Primary data (Alpaca, SIP; prices total-return adjusted by our own reconciled dividend table — amended
  2026-10-20, §11).** For SPY, QQQ, IWM, DIA from 2016-01-04:
  - `1DayAuction`: per session the listing exchange's **opening and closing cross** from SIP trades (condition
    `O` / `6`, else the exchange's official open / close report `Q` / `M`): the price an OPG / MOC order is filled
    at. Missing crosses are NaN and counted, never filled. Adjusted with the dividend table of
    `vwap_mr/data/dividends.py` (Alpaca corporate actions cross-checked against Yahoo, disagreements resolved in
    `data/dividends_overrides.csv` with a cited source), exact total return close-to-close.
  - `1DayPrint`: Alpaca's native daily bar, kept **for its consolidated volume only** (it includes extended hours
    and the closing cross; U1 measured it). U1 found its `open`/`close` are *not* the auction prints on many
    2016–2022 sessions and its `adjustment=all` misses or doubles some dividends. The headline VWAP uses the
    `1DayAuction` close and the `1DayPrint` volume.
  - `5Min` RTH bars: for RTH-only volume (a variant), the pre-close snapshot `snap1545` (last trade at or before
    15:45, cumulative RTH volume and session VWAP to 15:45), and U14.
  - **Session close for every `decision="close"` strategy and for marking is the `1DayAuction` close**, in the
    backtest and in the live loop alike (U2/U13 confirm the closing-cross trade is readable at 16:25 on the Basic
    plan's 15-minute-delayed SIP).
  - **Frozen vintage.** The development-window cache is fetched once and frozen (`data/cache/vintage_2026-10/`, user decision 2026-10-20);
    live top-ups go to a separate cache. Our factors are normalised at the window's last session, so a frozen
    vintage does not re-scale; a growing live cache does, so hashing it would make identical configurations look
    like new cells every quarter.
  - Alpaca's Basic plan provides this history; it refuses SIP requests whose window ends inside the last 15
    minutes, which matters only for the live loop (U2, U13).
- **Exogenous (point-in-time).** CBOE VIX close (`available_at` 16:20 ET: visible to a `close` decision made at
  16:25 the same day, and to a `t1545` decision only as the previous session's value); FRED `DTB3` via the FRED API, key `FRED_API_KEY` in `.env` (fredgraph.csv times out from this machine; cash yield;
  H.15 rule, next business day 16:30). Nothing else until a unit needs it.
- **Secondary daily history (`diag`).** SPY daily OHLCV 1993-01-29 → 2015-12-31 from a free source (**Yahoo chart API**,
  §9.1 as amended 2026-10-20), ratio-spliced to Alpaca at 2016-01-04 after verifying the overlap ratio is
  constant to rounding (close correlation > 0.9999). Uses: (i) the reproduction of the README claim from 2007,
  (ii) reversal-structure diagnostics and the H2 long-sample tables, (iii) **warm-up** of trailing windows and
  regime-model fits so that every gate and weight exists from 2016-01-04 (an HMM fitted on 1,008 sessions needs
  2012–2015). Frames carry `attrs["source"] = "diag"`; the cost model refuses to cost them as Alpaca fills; no
  registered headline statistic is computed on them.
- **Windows.**
  - Development window **2016-01-04 → 2025-12-31** (≈ 2,500 sessions): all looks, all registered tests.
  - Holdout **2026-01-01 → freeze date** (`HOLDOUT_START = 2026-01-01`). Research code is refused these bars. Read
    **once**, in U12, as a robustness slice (≈ 200 sessions carry no inference; the report says so). The read is
    logged to `results/holdout_access.jsonl`.
  - Forward test from the freeze date (U13): `live/` reads bars past `HOLDOUT_START` with one audit row per
    session in `results/forward_access.jsonl`. The one-time-read invariant applies to research code only.
- **Instruments.** SPY is the headline. QQQ, IWM, DIA are sample splits (N_eff ≈ 1.1–1.3 at their correlation;
  they test coherence, not power). No pooled variant.
- **Calendar.** Alpaca's exchange calendar, cached, refreshed monthly; half-days handled; a session the data layer
  dropped is a session with no bars and is logged.

---

## 5. Target architecture

```
vwap_mr/
  config.py            frozen RunConfig (≤ 30 fields), paths, HOLDOUT_START, DecisionTime enum
  data/                alpaca.py  store.py  bars.py  calendar.py  exo.py  quality.py  secondary.py  synthetic.py
  features/            vwap.py (rolling VWAP variants, session VWAP, snapshot)  deviation.py (trading d_t, state d*_t)
                       state.py (realized vol, VIX, lag-k autocorrelation, variance ratio, OU half-life, rule P&L)
  regime/              hurst.py (DFA, MF-DFA; R/S for the noise study only)  wavelet.py (causal MODWT, Abry–Veitch)
                       hmm.py (EM fit + Hamilton forward filter + one-step prediction; MS-AR(1))  bocpd.py  calibrate.py
  strategies/          base.py (Strategy protocol, DecisionView)  region.py  vwap_mr.py  trend.py  blend.py  gates.py
  backtest/            engine.py (position-series backtest: next_open and moc fills at prints)
                       costs.py (half-spread + slippage by fill kind, SEC/TAF on sells, borrow, T-bill)  account.py  metrics.py
  stats/               bootstrap.py  sharpe.py  power.py  multiplicity.py  breaks.py (Bai–Perron, Kalman edge tracker)
  experiments/         spec.py  runner.py  ledger.py  looks.py  report.py
  live/                bars.py  signals.py  orders.py  reconcile.py  monitor.py  score.py   # U2 smoke, U13
specs/                 pre-registered hypothesis specs (H1_vwap_mr.yaml, …); amendments as new files
results/               ledger.jsonl, looks.jsonl, holdout_access.jsonl, forward_access.jsonl and reports/*.md COMMITTED;
                       cells/ and reports/*.html gitignored
tests/                 one file per module + tests/causality.py (harness) + tests/fixtures/
scripts/               uNN_checks.py — a unit's done-when demonstrations (never "take a look" at performance)
PLAN.md  SPEC.md (from U3)  README.md  VWAP.R (kept as the historical record)
```

**Strategy protocol** (fixed in U3, SPEC §2). A strategy never sees a bar it could not have seen at its decision
time; the engine builds the view.

```python
class DecisionView(NamedTuple):
    sessions: pd.DataFrame      # completed sessions ≤ t (close decision) or ≤ t−1 (t1545 decision); 1DayAuction close
    snapshot: pd.Series | None  # t1545 only: last trade, RTH volume and session VWAP to 15:45 of session t
    state: pd.DataFrame | None  # state variables aligned to the same cut-off
    exo: Exo                    # .asof(decision_time)

class Strategy(Protocol):
    name: str
    decision: Literal["close", "t1545"]
    fill: Literal["next_open", "moc"]       # close → next_open (OPG); t1545 → moc (CLS)
    def target(self, view: DecisionView) -> float: ...   # target exposure f_t ∈ [−1, 1], fraction of equity
```

A region is a `Strategy` whose target is the equal-weight mean of its cells'. A gate multiplies a target by a
causal indicator from `view.state`. A blend is `w_t f^MR_t + (1 − w_t) f^TF_t` with `w_t` from `view.state`. The
engine, not the strategy, knows fill prices. The vectorized engine evaluates `target` over all t at once through
the same view construction (tested equal to the per-session loop).

---

## 6. Statistical protocol (v1)

### 6.1 Headline = region; variants on a curve; splits reported

A spec names one headline (or, for H3, up to four gates tested together under Holm), ≤ 8 variants per headline
(≤ 12 for H3's shared pool), the splits to report, instruments, costs, expected magnitude with its source, the
MDE line, and a trial budget the runner enforces. Variants and splits are reported, never used to pick a winner.
Cells of a region appear on a specification curve with bootstrap intervals; coherence = share of cells with the
headline's sign and the median cell's statistic.

**The VWAP-MR region R₀ is fixed here, from the mechanism, before any development-window P&L is read:**
`n ∈ {2, 3, 5, 10}` sessions × `b ∈ {0, 0.10, 0.25, 0.50 %}` = 16 cells; `VWAP_n` from `1DayAuction` close × `1DayPrint` volume
over sessions t−n+1 … t; cell target −1 / 0 / +1 per §8.1; region target = mean of the 16. Variants (U6): RTH-only
volume; VWAP from the sessions' own VWAPs; cells with b > 0 only; cells with n ≤ 5 only; long-only; short-only;
the other execution; vol-targeted at 10 %.

**Execution is chosen by feasibility, not by P&L** (§9.2): headline `decision="close"` at 16:25 ET with
`fill="next_open"` at the opening print (executable on the Basic plan); `t1545`/`moc` is a variant, built only if
§9.2/§9.3 ask for it.

### 6.2 H1 existence (one-sided mean test)

- Statistic: annualized mean daily **net excess** return of R₀ on SPY at the registered cost (bp/day shown).
- Test: studentized circular block bootstrap of the mean (block 21 sessions, 5,000 resamples, seeded), Newey–West
  t alongside, PSR(0). One-sided, H₁: mean > 0. k = 1 (H1 is alone in its stage).
- Floors: net return ≥ 3 %/yr at 1× notional; gross edge per round trip ≥ 2× the registered round-trip cost;
  coherence ≥ 12 of 16 cells positive.
- **Verdict:** **confirmed** — one-sided p < 0.05, floors, coherence; **promising** — one-sided p < 0.10 (the
  one-sided 90 % lower bound is > 0), floors, coherence → H2–H4 run as registered and the configuration goes to
  the forward test labelled *diagnostic*; **rejected** — otherwise → H3/H4 run on the `diag` sample with the vol
  baseline only, as diagnostics, and the program reports a null.
- Reported: beta and alpha against SPY, the cost curve, every split (year, vol quintile, VIX tercile, weekday,
  long vs short side, overnight vs intraday leg).

### 6.3 H2 persistence (descriptive, with pre-registered summaries)

- Edge by calendar year and by rolling 3-year window with block-bootstrap intervals, on Alpaca 2016–2025 and on
  `diag` 1993–2015 (gross, and at a stated historical cost schedule, labelled).
- Structural break: Bai–Perron on the mean of the daily P&L (HAC); the number and dates of breaks with intervals.
- **Kalman edge tracker:** local-level model on daily net P&L, `y_t = μ_t + ε_t`, `μ_t = μ_{t−1} + η_t`; MLE of the
  signal-to-noise ratio q with its likelihood profile shown (q → 0 reproduces the sample mean and is reported as
  such, not hidden); filtered posterior of μ_T → **P(μ_T > 0)** and an interval: the program's answer to "is it
  true today". The same filter is the U13 edge monitor.
- Pre-registered summary line: last-5-year Newey–West t; Bai–Perron break count and p; P(μ_T > 0). No pass/fail;
  the numbers feed the H3/H4 expected magnitudes.

### 6.4 H3 regime predictability (conditional mean difference, paired)

**The null must be stated correctly.** If a gate is "in" a fraction π of sessions and carries no information, the
gated stream's Sharpe is √π × the ungated Sharpe (its mean scales by π, its σ by √π): a paired Sharpe test of
gated vs ungated against ΔSR = 0 would reject an informative gate and accept nothing. The test is therefore on
the **conditional mean**:

- For state variable S with pre-registered favourable set F_S (fraction π_S fixed in the spec; defined causally
  from trailing quantiles or a filtered probability threshold), using the H1 region's net daily return r_t and
  the state known at the decision time S_{t} (one-step-ahead for regime models):
  `Δμ_S = mean(r_t | S ∈ F_S) − mean(r_t | S ∉ F_S)`, one-sided, H₁: Δμ_S > 0; circular block bootstrap of the
  session index (block 21) recomputing both conditional means per resample; Newey–West t on the regression of
  r_t on 1[S ∈ F_S].
- Floors: in-gate mean > 0; π_S within ± 0.05 of its registered value (a trailing-quantile gate drifts; a drift
  is reported).
- Reported (economic version): Sharpe of the gated stream **scaled to the ungated stream's volatility** vs the
  ungated (paired Ledoit–Wolf); number of distinct gate episodes and their median length (a 504–1,008-session
  estimator yields few independent episodes in ten years — the daily test is honest under the null, but the
  result rests on those episodes and the report says how many); the incremental HAC regression of each
  estimator/regime variable on top of the two baselines B1 and B4.
- **Headline program (Holm over 4):** B1, B4, E2, R2 (table below). Everything else is a variant.
- Estimator and regime configurations (windows, scale ranges, state count, hazard set, thresholds, standardization
  windows, priors) are **frozen in U8/U9 from synthetic studies** and written into the spec draft before any
  market P&L is read. A later change is an amendment.

| label | state variable | definition (window in sessions) | favourable set F_S (π) |
|---|---|---|---|
| B1 | realized volatility | σ̂ over 21; quantile vs trailing 504 | top two quintiles (0.40) |
| B2 | VIX | close as-of the decision; quantile vs trailing 504 | top tercile (0.33) |
| B3 | lag-1 autocorrelation | of daily returns over 63 | < 0 (≈ 0.5, reported) |
| B4 | reversion speed | AR(1) φ of the state deviation d*_t (§8.1) over 126; quantile vs trailing 504 | below median (0.50) |
| B5 | variance ratio | Lo–MacKinlay VR(5) over 252 | < 1 (≈ 0.5, reported) |
| B6 | strategy momentum | region net P&L over the trailing 63 | > 0 (≈ 0.5, reported) |
| E1 | DFA α, standard | DFA-1 on returns over 504, scales s ∈ [10, 126] | below trailing-504 median (0.50) |
| **E2** | DFA α, short-scale | DFA-1 on returns over 504, scales s ∈ [4, 32] (the 1–6-week horizon the rule trades) | below trailing-504 median (0.50) |
| E3 | wavelet H, short-scale | Abry–Veitch on the causal MODWT, levels j = 1–4, window 512 | below trailing-504 median (0.50) |
| E4 | MF-DFA | h(2) and spectrum width Δα over 1,008; q ∈ {−5 … 5} | h(2) below trailing median (0.50); Δα reported only |
| R1 | HMM, 2-state | Gaussian HMM on returns standardized by trailing 252 σ; EM on trailing 1,008, refit every 21; states ordered by σ; **one-step-ahead** P(high-σ state at t+1 \| ≤ t) | P > 0.5 |
| **R2** | MS-AR(1) on d*_t | two regimes in (φ, σ); fit as R1; regimes ordered by φ at each refit, swap flagged when \|φ₁ − φ₂\| < 0.05; one-step-ahead P(fast-reversion regime) | P > 0.5 |
| R3 | BOCPD, Normal | Adams–MacKay on standardized returns; NIG prior fixed from `diag` 1993–2011; hazard 1/λ averaged over λ ∈ {63, 126, 252}; pruning at log-prob < max − 10 | E[run length] > 21 and P(cp) < 0.2 (reported as a stability gate) |
| R4 | BOCPD, AR(1) on d*_t | NIG prior on (φ, σ²) per run; run-length-weighted posterior φ | posterior P(φ < trailing median φ) > 0.5 — optional, U9 |

### 6.5 H4 combination (blend vs static blend)

- Components: the MR region (H1) and a trend region (TSMOM: sign of the trailing return over {21, 63, 126, 252}
  sessions, equal weight, 1× notional, fixed from Moskowitz–Ooi–Pedersen; Donchian-55 breakout as a variant).
- Blend: `f_t = w_t f^MR_t + (1 − w_t) f^TF_t`, `w_t = 0.5 + κ (P_t − 0.5)` with `P_t` = R2's one-step-ahead
  P(fast reversion) and shrink κ = 0.5 registered. Benchmarks: static 50/50; MR alone; TF alone; vol-managed
  buy-and-hold (σ* 15 %, σ̂ 21, exposure ≤ 1: the retail benchmark).
- Test: paired Ledoit–Wolf Sharpe difference, blend vs static blend, one-sided (ρ ≈ 0.9, so the √π problem of §6.4
  does not arise); the matched-volatility comparison and the drawdown difference reported. Floor: blend net return
  ≥ 0.9 × static blend's.
- Variants (≤ 6): `w_t` from B1; from B6 computed on MR − TF (the "which rule has been working" baseline the regime
  model must beat); from R4; hard switch (w ∈ {0, 1}); κ ∈ {0.25, 0.75}; vol-targeted blend at 10 %. The MR–TF
  daily return correlation by year is reported: the static blend already harvests that diversification.

### 6.6 Power first (numbers to keep in mind)

n ≈ 2,500 development sessions (with `diag` warm-up every gate exists from 2016-01-04); fully invested long/short
SPY daily σ_d ≈ 1.0–1.1 %; one-sided α, 80 % power; MDE = (z₁₋α/k + z_power)·SE.

| test | k | SE | MDE | ≈ Sharpe |
|---|---|---|---|---|
| H1 mean > 0 | 1 | σ_d/√n ≈ 2.1 bp | ≈ 5 bp/day ≈ 12 %/yr | 0.8 |
| H1 on `diag` + Alpaca, n ≈ 8,300 (diagnostic) | 1 | ≈ 1.15 bp | ≈ 2.9 bp/day | 0.45 |
| H3 Δμ, π = 0.40 | 1 / Holm 4 | σ_d·√(1/(πn) + 1/((1−π)n)) ≈ 4.3 bp | ≈ 11 / 13 bp/day | — |
| H3 Δμ, π = 0.50 | 1 / Holm 4 | ≈ 4.2 bp | ≈ 10.5 / 13 bp/day | — |
| H3 Δμ on `diag` + Alpaca, π = 0.40 | 1 | ≈ 2.4 bp | ≈ 6 bp/day | — |
| H4 ΔSharpe, ρ 0.9 | 1 | √(2(1−ρ)/n)·√252 ≈ 0.14 | ≈ 0.35 | — |
| H4 ΔSharpe, ρ 0.7 (e.g. a hard switch) | 1 | ≈ 0.25 | ≈ 0.6 | — |

What this says: an edge concentrated in the favourable state at, say, 8 bp/day with ≈ 0 elsewhere (Δμ ≈ 8 bp) is
**below** the H3 MDE on the Alpaca window alone. H3 and H4 are therefore registered as diagnostics unless the H2
numbers put the expected effect above the line; the `diag` extension and the forward test are where they gain
power. MinTRL for PSR(0) ≥ 0.95 is ≈ 10.8 years at Sharpe 0.5 and ≈ 2.7 years at 1.0: a Sharpe-0.5 edge is never
"confirmed" by a one-year forward test either (§6.7 defines what the forward test can say).

### 6.7 Multiplicity, looks, holdout, forward verdict

- Holm across the simultaneous hypotheses of a stage (≤ 4). The looks ledger prints K and p·K beside every p.
- Trial budget per spec: 1 headline + ≤ 8 variants (H3: 4 headlines + ≤ 12 variants); the runner refuses a cell
  beyond the budget.
- Cash earns `DTB3` (ACT/360); every Sharpe, alpha and PSR is on excess returns, benchmark included.
- Holdout and forward reads as §4.
- **Forward verdict (H5).** After 26 weeks: **consistent** iff the forward net mean has the registered sign, its
  Newey–West t > 0, and it lies inside the registered 90 % interval from the development window; **inconsistent**
  otherwise. Thereafter: a monthly track-record line (cumulative forward Sharpe, PSR(0), MinTRL remaining). The
  decision to trade capital is the user's, made on that line; "confirmed" (PSR(0) ≥ 0.95) is the label it earns
  if and when it does.

---

## 7. Units of work

```
U0 scaffold ─► U1 data ─┬► U2 live plumbing smoke (one-share paper orders)
                        ├► U3 backtest + costs + account (SPEC §1–§5) ─► U5 reproduce & diagnose ─► U6 H1 ─► U7 H2
                        └► U4 statistics toolkit (SPEC §6) ───────────┘                                   │
U8 state estimators + noise study (synthetic from U0; market series from U1) ─► U9 regime models + detection ─┤
                                                                      U10 H3 (after U6, U8, U9) ─► U11 H4 ─► U12 holdout + verdict + registry ─► U13 forward test
U14 (gated) intraday session-VWAP family — after U6's verdict and §9.7
U15 (gated) ML conditioning overlay — only after "consistent" in U13
```

U4 and U8 need only synthetic data and can run in parallel with U1–U3. Hyper-parameter sensitivity of the regime
layer is a set of variants inside U10/U11 (the draft's separate walk-forward unit was cut; §11). "Session" = one
working session. Each unit's status note (`results/reports/UNN_status.md`) records commands and output, looks
taken (label, n), reviewer findings and their disposition.

---

### U0 — Scaffold, protocol and hygiene

**Goal.** A repository where §0 can be followed from the first commit.

**Scope.**
- `pyproject.toml`: remove `vectorbt`, `plotly`; add `pandas`, `scipy`, `statsmodels`, `pyarrow`, `pywt`,
  `pyyaml`, `python-dotenv`, `alpaca-py`, `requests`, `matplotlib`; dev: `pytest`, `pytest-cov`, `ruff`, `ty`,
  `hypothesis`, `MFDFA` and `nolds` (cross-checks only). `[tool.ruff]` line-length 110;
  `[tool.pytest.ini_options]` `testpaths = ["tests"]`.
- Package skeleton per §5 with module docstrings stating each contract; `config.py` with `HOLDOUT_START =
  date(2026, 1, 1)`, `DEV_START = date(2016, 1, 4)`, `DEV_END = date(2025, 12, 31)`, `DecisionTime`.
- `experiments/ledger.py` and `experiments/looks.py` (ported), `results/ledger.jsonl` and `results/looks.jsonl`
  created; the looks ledger seeded with **K₀ = 1** (the README's in-sample chart and its "down-market" reading, a
  prior look at a sample overlapping the development window).
- `.gitignore`: replace `*.yaml` with `alpaca_api.yaml` and `.env`; ignore `data/cache/`, `results/cells/`,
  `results/reports/*.html`; **not** `results/*.jsonl`, `results/reports/*.md`, `specs/`.
- `.env.example`; credentials migrated out of `alpaca_api.yaml` by the user, file deleted.
- **Delete `trading/`** (its content is recorded in §1.3 and git history; the R logic is re-implemented in U5).
- Copy the user-level `~/.claude/agents/adversarial-reviewer.md` to `.claude/agents/` so the protocol travels
  with the checkout. `scripts/done_gate.ps1` runs the §0 gate.
- `README.md`: one paragraph pointing here; the old claim lives in §1.1.

**Done when.**
- [ ] `uv sync` from a clean checkout; `uv run pytest -q` (one smoke test), ruff, ty clean; `vectorbt` absent from
      `uv.lock`; `import vwap_mr` works; `trading/` gone.
- [ ] `git check-ignore specs/x.yaml` matches nothing; `git check-ignore alpaca_api.yaml .env` matches both.
- [ ] `looks.jsonl` has the K₀ row; the ledger round-trips a row under lock (test).
- [ ] No secret in the new files' history (`git grep -I -E "(PK|SK)[A-Z0-9]{16,}"` empty).

**Reviewer focus.** The gitignore rule; secrets; anything still importing `trading`.

**Cost.** ½ session. **Depends on.** nothing (§9.6 default used unless overridden). **Status.** done on
`unit/00-scaffold` @ d623674, awaiting merge; 4/4 done-when; `results/reports/U00_status.md`.

---

### U1 — Data layer: Alpaca cache, prints, calendar, exogenous series, `diag` history and warm-up

**Goal.** Reproducible, hashed, holdout-guarded bars behind one entry point; the `diag` history spliced so that
estimators can warm up before 2016.

**Scope.**
- `data/alpaca.py`, `data/store.py`, `data/bars.py`, `data/calendar.py` ported per §3.3. Cache
  `data/cache/<vintage>/{feed}/{adjustment}/{timeframe}/{SYMBOL}.parquet` + `.json` sidecar (sha256, coverage,
  fetched_at, schema version); atomic writes; 7-session overlap re-fetch with re-adjustment detection; `end`
  clamped to the last completed session; `load_bars(symbol, timeframe, start, end, *, vintage, allow_forward=False)`
  raises `HoldoutError` at or past `HOLDOUT_START` unless `allow_forward` (live only; audit row).
- Timeframes: `1DayPrint` (native; open/close = prints; volume kept and **compared** with the RTH sum from 5Min —
  the result settles whether it includes extended hours), `5Min` (native, RTH-filtered), `1DayRTH` (resampled:
  RTH volume, session VWAP = Σ bar_vwap·volume / Σ volume), `snap1545` (from 5Min).
- `data/exo.py`: VIX (CBOE CSV; `available_at` 16:20 ET), `DTB3` (FRED; H.15 rule). `Exo.asof(decision_time)`.
- `data/secondary.py`: SPY daily 1993 → 2015 from §9.1's source; ratio-splice at 2016-01-04 (ratio constant over
  the overlap to rounding, else refuse); `attrs["source"] = "diag"`; stored under `data/cache/diag/`. A combined
  frame `load_spy_long()` for diagnostics and warm-up only; the cost model refuses `diag` rows.
- `data/quality.py` report → `results/data_quality.csv`. `data/synthetic.py` (seeded GBM with vol regimes, 5Min →
  daily) and `tests/fixtures/spy_2024q1.parquet` (one real quarter, checked in).
- CLI `uv run python -m vwap_mr.data fetch --symbols SPY QQQ IWM DIA --start 2016-01-04 --vintage 2026-xx`; `end`
  defaults to `HOLDOUT_START` (exclusive).

**Done when.**
- [ ] Tests (mocked client): tz conversion; RTH filter on a half-day; resample parity vs hand-computed bars;
      cache round-trip with hash mismatch raising; top-up without duplicates; re-adjustment triggers a full
      re-fetch; holdout refusal; `available_at` rules for both decision times; `diag` rows refused by the cost
      model; splice refuses a non-constant ratio.
- [ ] Live smoke (recorded): four symbols fetched into the frozen vintage; quality report: 0 duplicates, 0 OHLC
      violations, missing sessions listed and explained; `1DayPrint` volume vs RTH sum — ratio distribution
      reported and the headline volume convention confirmed or amended; `1DayPrint` close vs last 5Min close
      median |Δ| and p99 reported.
- [ ] `diag` overlap: close correlation > 0.9999 after splice; the adjustment conventions documented.
- [ ] No secret in logs.

**Reviewer focus.** Bar stamp (bar open) and its off-by-one; Alpaca's inclusive `end`; DST; the splice ratio
hiding a dividend-adjustment mismatch; `available_at` of VIX at 15:45 vs 16:25; the vintage directory actually
being frozen (no top-up path writes into it).

**Cost.** 1½ sessions. **Depends on.** U0, §9.1. **Status.** done on `unit/01-data` @ 69e6b7d, awaiting merge; 4/4
done-when; vintage_2026-10 frozen (data_hash a2efbd9f…); `results/reports/U01_status.md`.

---

### U2 — Live plumbing smoke (early, one share, paper)

**Goal.** Learn Alpaca's order mechanics months before U13 so that the execution convention the backtest encodes
is one the account can actually run.

**Scope.** `live/orders.py` minimal: submit a one-share `opg` market order after 19:00 ET and (if §9.2 keeps the
variant alive) a one-share `cls` order at 15:48 ET; record acceptance/rejection times, fill price and time,
and the paper environment's fill rule (paper fills are **simulated** from the live quote; U13 treats them so).
Record: the OPG acceptance window (Alpaca rejects OPG submitted between 09:28 and 19:00 ET), the CLS cutoff
(Alpaca rejects after 15:50 ET), what `get_latest_bar` returns at 16:25 on the Basic plan (15-minute SIP delay),
and whether IEX real-time volume is usable for a snapshot (it is a few percent of consolidated; expect "no").
Five sessions of one-share orders; a table in `results/reports/U02_status.md`.

**Done when.**
- [ ] Table of five sessions: order type, submit time, accept/reject, fill price vs the official print, delay.
- [ ] The backtest's two fill conventions (U3) are confirmed executable or the infeasible one is struck from the
      spec draft with the evidence.
- [ ] No position is left open (a flatten step runs at the end of each smoke).

**Reviewer focus.** Paper-fill semantics being mistaken for auction fills; the 19:00 OPG window; time zones.

**Cost.** ½ session of code; one calendar week. **Depends on.** U1 (credentials, calendar), a paper account
(free). **Status.** not started.

---

### U3 — Backtest engine, costs, account; SPEC.md §1–§5

**Goal.** A position-series backtest that mirrors the live loop exactly, with every cost booked where it is paid
and the engine's invariants tested.

**Scope.**
- `strategies/base.py` (`DecisionView`, `Strategy`), `strategies/region.py`, `features/vwap.py`,
  `features/deviation.py` (trading `d_t` and state `d*_t`, §8.1).
- `backtest/engine.py::run(bars, strategy, *, costs, cash_yield) -> Result`. Builds the `DecisionView` for every
  t and fills:
  - `next_open`: shares change at the opening print of t+1. Session t+1 gross return is **compounded**:
    `R_{t+1} = (1 + f_{t−1}·r_on)(1 + f_t·r_id) − 1`, `r_on = open_{t+1}/close_t − 1`, `r_id = close_{t+1}/open_{t+1} − 1`;
    cost of the change `|f_t − f_{t−1}|` booked at the open of t+1.
  - `moc`: shares change at the closing print of t (decision from `snap1545`); `R_{t+1} = f_t·(close_{t+1}/close_t − 1)`;
    cost of the change booked at the close of **t** (the fill).
  - Equity marked at `1DayAuction` closes; a missing cross is a counted fallback to the last RTH 5Min close, never silent;
    per-session cost ledger (notional traded, cost paid, by fill kind) so a stream can be re-priced.
- `backtest/costs.py`: per fill, half-spread + slippage by kind (`open_auction`, `close_auction`); registered
  1.0 bp round trip until `measured`; curve 0.3 / 1.0 / 2.3; SEC Section 31 fee and FINRA TAF on sells from a
  dated table `data/costs/fees.csv` (rates change each fiscal year; the 2025 zero-rate period included); short
  borrow at an annual rate on short notional per session held (default 0.4 %/yr); cash yield from `DTB3`.
  `reprice(result, round_trip_bp)`.
- `backtest/account.py::check(result, profile) -> (tradable, reasons)`: margin vs cash (shorts refused on cash);
  Reg-T (|f| ≤ 1 overnight at the headline); a day-trade counter (the daily rules make none; a flip from long to
  short at one print is one trade, not a day trade). Profiles `margin_30k`, `cash_30k`. Nothing more.
- `backtest/metrics.py`: annualized excess return, vol, Sharpe, Sortino, max DD and duration, PSR(0), turnover,
  round trips/yr, flat share, worst day, beta/alpha vs SPY, by-year table.
- `SPEC.md` §1 data, §2 strategy protocol with a worked example per fill, §3 engine, §4 costs, §5 account.

**Done when.**
- [ ] Invariants (tests): always-long = buy-and-hold at zero cost to 1e-12 under both fills; a sign-flipped target
      negates gross P&L exactly under `moc` and to first order (|Δ| ≤ Σ|r_on·r_id|) under `next_open`; cost
      booked = Σ |Δf|·notional·rate to the cent on a 1,000-change random target (a long→short flip trades 2×);
      a random ±1 target has |gross mean| < 2 SE and loses exactly the booked cost; `moc` and `next_open` agree
      when `open_{t+1} = close_t`; the engine is causal under the §8.6 harness **for both decision times** (a
      `t1545` strategy that reads `close_t` is caught); a planted same-bar fill is caught.
- [ ] A test-only fill `same_close` (the R convention) exists, is unreachable from a spec (test), and reproduces a
      hand-computed 10-session example.
- [ ] The vectorized engine equals the per-session loop on the fixture quarter to 1e-12.

**Reviewer focus.** The compounded decomposition and its invariants; cost on flips; borrow on partial sessions;
fee table dates; `moc` reading `snap1545` and nothing later; `same_close` leaking into a spec path.

**Cost.** 1½ sessions. **Depends on.** U1. **Status.** done, merged (PR #3); 3/3 done-when; SPEC.md §1–§5;
`results/reports/U03_status.md`.

---

### U4 — Statistics toolkit; SPEC.md §6

**Goal.** Every statistic in §6, validated against published numbers and by simulation before any market result
is read.

**Scope.** `stats/bootstrap.py` (circular block indices; studentized block bootstrap of a mean, of a conditional
mean difference, and of a Ledoit–Wolf Sharpe difference, Bartlett HAC at the block bandwidth; seeded),
`stats/sharpe.py` (Sharpe, moments, PSR, MinTRL, Ledoit–Wolf paired difference), `stats/multiplicity.py` (Holm;
Bonferroni bound with K), `stats/power.py` (analytic MDE for a mean, for a conditional mean difference at a given
π, and for a paired Sharpe difference at a given ρ; simulated power under GARCH(1,1)-t), `stats/breaks.py`
(Bai–Perron via `statsmodels` or own implementation with HAC; local-level Kalman tracker with MLE and the
likelihood profile). `SPEC.md` §6 with each formula and its reference.

**Done when.**
- [ ] Bailey & López de Prado test vector: PSR/MinTRL invert each other; the DSR example (SR₀ 0.1132, DSR 0.9004)
      reproduced once in a test as a check on the PSR implementation (DSR itself is not used); Holm matches a hand
      example.
- [ ] Size and power: mean test size 0.05 ± 0.01 over 1,000 iid and 1,000 GARCH-t simulations at n 2,500; power ≥
      0.78 at the analytic MDE; the conditional-mean-difference test has size ≤ 0.06 when the gate is an
      independent coin with π = 0.4; the block bootstrap preserves lag-1 autocorrelation within 10 %.
- [ ] Kalman tracker (amended 2026-10-20, §11): the operating curve for a planted 10 bp/day shift at σ 1 % is
      reported, i.e. the crossing lag of P(μ > 0) = 0.5 against the pre-shift false-crossing share, over q and for
      the causal track. The MLE's q → 0 case is detected and reported, not silently accepted. Bai–Perron size
      ≤ 0.06 on stationary AR(1) noise. (Original wording: "P(μ > 0) crosses 0.5 within 120 sessions of a planted
      10 bp/day shift at σ 1 %"; it cannot be met without a high false-crossing rate.)

**Reviewer focus.** Per-period vs annualized Sharpe in PSR; raw vs excess kurtosis; one-sided conventions; the √π
scaling in the conditional test's simulation; HAC bandwidth.

**Cost.** 1 session. **Depends on.** U0. **Status.** done, merged into `dev`; done-when 1 and 3 ticked (3 as amended,
§11), 2 met within Monte Carlo error (GARCH-t power 0.779 vs 0.78); SPEC.md §6; `results/reports/U04_status.md`.

---

### U5 — Reproduce the inherited claim and diagnose the mechanism (looks counted; no region P&L)

**Goal.** Settle §1 and measure the reversal structure the rule depends on, **without reading the region's P&L on
the Alpaca development window** (that is U6's one test). Output `results/reports/U05_diagnosis.md`.

**Scope.**
- Exact re-implementation of `VWAP.R` (one cell: n = 3, b = 0.1 %, VWAP from close × volume, `same_close` fill,
  zero cost) on the spliced long series from **2007-01-01**, with Yahoo's split-adjusted convention emulated on
  `diag` where possible: reproduce the README's 0.468 vs 0.314 within a tolerance explained by data vintage;
  the same by sub-period (2007–09, 2010–15, 2016–25) to show how much of the figure is 2008–09. This is the
  single permitted strategy-P&L look on the development window (one cell; recorded).
- The same cell under `next_open` and `moc` at 0 / 0.3 / 1.0 / 2.3 bp on **`diag` 1993–2015 only**: the honest
  version of the claim before 2016, with intervals; and the overnight vs intraday legs of the cell's return on
  `diag` (which execution loses what).
- Reversal structure on daily index returns (no strategy): lag-1…5 autocorrelation by year with intervals,
  VR(2, 5, 10), the overnight/intraday decomposition of index returns, all on `diag` 1993–2015 **and** Alpaca
  2016–2025 (mechanism diagnostics on the development window are allowed and counted); the same for QQQ, IWM, DIA.
- The 16 R₀ cells' gross bp/day on **`diag` 1993–2015 only**, as a specification curve: the source of H1's
  expected magnitude (with the literature). R₀ itself is not changed by this unit.
- Every table recorded as a look before it is printed (the runner enforces it).

**Done when.**
- [ ] Report written; every number carries an interval or a t; K recorded in the report and in §10.
- [ ] README claim reproduced from 2007 or the residual explained; sub-period table present.
- [ ] The report ends with H1's expected magnitude (bp/day, source: `diag` R₀ curve and the autocorrelation
      tables) and the MDE at that magnitude — the inputs to U6's spec.

**Reviewer focus.** Any Alpaca 2016–2025 strategy P&L beyond the single inherited cell; the Yahoo adjustment
emulation; the `same_close` mode leaking into non-diagnostic runs.

**Cost.** 1 session. **Depends on.** U1, U3, U4. **Status.** done, merged into `dev` (7ac429c); 3/3
done-when; K 1 → 112 (every window, §11 ruling); `results/reports/U05_diagnosis.md`, `U05_status.md`.

---

### U6 — H1: registration and the existence test

**Goal.** Register and run the one existence test (§6.2) on R₀ as fixed in §6.1. **H1 is registered as a
diagnostic** (user decision 2026-10-21, from U5): the expected magnitude, 2.3 bp/day net at 1.0 bp (an upper value;
`diag` R₀ 1993–2015, 0.7–1.4 outside 2007–09), is below the MDE of 4.6 bp/day (n 2,513, σ_d 92 bp; power 0.35),
so the spec carries `power.diagnostic: true` and its report says so beside the §6.2 verdict.

**Scope.** `strategies/vwap_mr.py` (R₀), `specs/H1_vwap_mr.yaml` (§8.7 fields; variants per §6.1; splits per §6.2;
execution per §9.2's answer or default), `experiments/spec.py`, `experiments/runner.py` (runs a spec once; writes
`results/cells/<hash>/` with daily returns, cost ledger, trades, `result.json`; ledger row with git SHA, code hash,
data hash; **refuses to render a report until `look` is recorded**), `experiments/report.py` (specification curve,
cost curve, splits, verdict).

**Done when.**
- [ ] Spec committed at a recorded SHA with the MDE line, `expected` from U5 and `power.diagnostic: true`; runner refuses an uncommitted, dirty or over-budget spec
      (tests); report refused before `look` (test).
- [ ] Run once; report written; verdict per §6.2 in §10 with p, lower 90 % bound, floors, coherence; cost curve.
- [ ] R₀ and every variant pass the causality harness and the fills-after-decision test.

**Reviewer focus.** Expected magnitude vs U5's tables (no inflation); the headline fill is the one U2 showed
executable; the `moc` variant present only if §9.2/§9.3 allow it.

**Cost.** 1 session. **Depends on.** U5, §9.2. **Status.** done, merged into `dev` (5f150e0); 3/3
done-when; **rejected (diagnostic)**; K 112 → 139; `results/reports/U06_H1.md`, `U06_status.md`.

---

### U7 — H2: persistence and "is it still true"

**Goal.** The §6.3 report on the H1 headline stream and its `diag` extension.

**Scope.** `results/reports/U07_persistence.md`: by-year and rolling-3-year tables with intervals (Alpaca and
`diag`); Bai–Perron breaks; the Kalman tracker trajectory and **P(μ_T > 0)** at 2025-12-31 with the q profile;
conditional tables (B1 quintile, B2 tercile, prior-month sign, weekday, long vs short side); cross-instrument
coherence; the summary line. The tracker is wired as the U13 edge monitor.

**Done when.**
- [ ] Report written; summary line in §10; `diag` rows labelled and costed with the documented schedule.
- [ ] The conditional tables are recorded as looks (they inform H3's expected magnitudes).

**Reviewer focus.** HAC in the break test; reading persistence off a sample that ends in a specific regime; the
conditional tables not being used to change any F_S (those are fixed in §6.4).

**Cost.** ½ session. **Depends on.** U6. **Status.** done, merged into `dev` (71a227f; branch @ 39b5698), not merged; 2/2
done-when; P(μ_T > 0) = 0.447, no break, q_zero; K 139 → 153; `results/reports/U07_persistence.md`, `U07_status.md`.

---

### U8 — State estimators and their noise (synthetic first)

**Goal.** Implement the state variables of §6.4, measure each estimator's bias and standard deviation at the
windows we could use, and freeze the registered configurations before any market P&L is read.

**Scope.**
- `features/state.py`: B1–B6 (B6 computed from the H1 region's own P&L, available from U6; a stub until then).
- `regime/hurst.py`: DFA-1 on return increments (profile Y = cumsum(r − r̄); segments from both ends; linear
  detrending; α = slope of log F(s) on log s over the registered scale set) with scale sets `standard` [10, 126]
  and `short` [4, 32]; MF-DFA (q ∈ {−5 … 5}, h(q), τ(q), Legendre spectrum, Δα). R/S (Lo's modified) appears in
  the noise study **as the reference everyone quotes**, not as a candidate. No GHE (redundant with DFA).
- `regime/wavelet.py`: Abry–Veitch on the causal MODWT (weighted LS of log₂ μ_j on j over j₁ … j₂; H = (slope + 1)/2
  for increments), levels j = 1–4 (short) and 2–6 (standard). The standard DWT's symmetric boundary reads the
  future at the window's end and is not allowed; a planted boundary peek is a mutant test.
- `regime/calibrate.py` + `results/reports/U08_estimators.md`: on fGn/ARFIMA with H ∈ {0.40, 0.45, 0.50, 0.55,
  0.60} and t₄ innovations, windows n ∈ {252, 504, 1,008}, 500 draws each: bias and SD per estimator and scale set;
  the smallest window at which **SD ≤ 0.025** (a 2σ separation of 0.45 from 0.50); the autocorrelation of
  consecutive rolling estimates (the effective number of independent estimates per decade); Δα under fat tails
  alone (the spurious multifractality a reader must discount); runtime. Decision rule: an estimator enters H3's
  headline or variants only with a window where SD ≤ 0.025; otherwise it is a variant labelled *under-powered*.
- Registered configurations written as a table (estimator, window, scale set, favourable direction, π) into the
  `specs/H3_regime_gates.yaml` draft (not yet registered). The `diag` warm-up policy (§4) means every estimator
  has a value on 2016-01-04.

**Done when.**
- [ ] Cross-checks: DFA h(2) agrees with the `MFDFA` package on three seeded series (|Δ| < 0.01 once the segmenting
      convention is matched: that package segments from one end by default); R/S with `nolds`; Abry–Veitch
      recovers H on fGn with |bias| < 0.03 at n 1,024.
- [ ] Every estimator and state feature passes the causality harness at both decision times; the MODWT boundary
      mutant is caught.
- [ ] Report and the spec-draft table written; all state variables for four symbols over 2,500 sessions in < 60 s.

**Reviewer focus.** DFA on returns vs on the profile (α vs α + 1); the short-scale set's lower bound (s = 4 is
noisy); overlapping-estimate autocorrelation; the trailing-median window (504) for every "below median" set.

**Cost.** 1½ sessions. **Depends on.** U0 (synthetic), U1 (market), U6 for B6. **Status.** not started.

---

### U9 — Causal regime models with a detection study

**Goal.** One-step-ahead regime probabilities and change-point posteriors, tested on synthetic regime-switching
series for delay and false-alarm rate, with configurations frozen before market P&L is read.

**Scope.**
- `regime/hmm.py`: 2-state Gaussian HMM on returns standardized by the trailing-252 σ (R1); EM on the trailing
  1,008 sessions, refit every 21, states ordered by σ; the **Hamilton forward filter and the one-step prediction**
  `P(s_{t+1} | ≤ t) = P(s_t | ≤ t)·A` implemented here (`hmmlearn.predict_proba` is smoothed and not allowed); the
  prediction for t+1 uses parameters fitted on sessions ≤ t − 1 only. MS-AR(1) on `d*_t` (R2; `statsmodels`
  `MarkovAutoregression` for the fit, `switching_ar=True`, `switching_variance=True`; our own filter step);
  regimes ordered by φ at each refit with the swap flag of §6.4. The random-walk null for φ at n = 5 is simulated
  and reported beside the estimates.
- `regime/bocpd.py`: Adams & MacKay (2007) run-length recursion; constant hazard 1/λ averaged over λ ∈ {63, 126,
  252}; Normal–Inverse-Gamma observation model on standardized returns with the prior fixed from `diag`
  1993–2011 (R3); pruning at log-prob < max − 10; outputs P(r_t = 0), E[r_t]. R4 (AR(1)–NIG on `d*_t`) is
  **optional**: build only if time remains after R1–R3 pass their tests.
- `regime/calibrate.py` + `results/reports/U09_detection.md`: on a Markov-switching AR(1) with known regimes
  (φ ∈ {0.3, 0.9}, mean durations {63, 252}) and on the fGn panel: detection delay (sessions to P > 0.5 after a true
  switch), false alarms per 1,000 sessions, Brier score of the one-step-ahead probability vs the true next state;
  the B1/B4 baselines on the same series.
- Compute: one decision for one symbol in < 50 ms.

**Done when.**
- [ ] Our Hamilton filter equals `statsmodels` filtered probabilities on a fixed-parameter example to 1e-10; the
      BOCPD matches a reference implementation on a 200-point example to 1e-8; a refit that sees session t is
      caught by the harness; the standardization window is trailing (a full-sample σ is caught by the harness).
- [ ] Detection report written; registered configurations (window, cadence, λ set, priors, thresholds, π) copied
      into the H3/H4 spec drafts.

**Reviewer focus.** Smoothed vs filtered vs predicted; label switching across refits; EM seeds; the first sessions
of the `diag` warm-up (no model yet) handled as "no gate" and counted; the NIG prior not centred on reversion.

**Cost.** 1½–2 sessions. **Depends on.** U8. **Status.** not started.

---

### U10 — H3: regime predictability test

**Goal.** Register and run §6.4 on the H1 region stream.

**Scope.** `strategies/gates.py`; `specs/H3_regime_gates.yaml` (headlines B1, B4, E2, R2; variants from the §6.4
table and the regime layer's hyper-parameter sensitivity: refit cadence {21, 63}, EM window {756, 1,512}, λ
sets, κ-free; ≤ 12 variants; MDE line per headline at its π); the incremental regressions; the episode count;
`results/reports/U10_regimes.md`.

**Done when.**
- [ ] Spec committed at a recorded SHA with MDE lines and π per gate; run once; Holm verdict over the four
      headlines in §10 with each gate's Δμ, p, p·K, π realized vs registered, episode count.
- [ ] Incremental-value table: for E1–E4 and R1–R4, the HAC t given B1 + B4, and the vol-matched gated Sharpe
      difference.
- [ ] Causality harness on every gate at the decision time.

**Reviewer focus.** Favourable sets from trailing quantiles only; π drift; one-step-ahead probabilities used for
t+1 (not the filtered P(s_t)); B6 at a `t1545` decision would read an incomplete session (it is defined on
completed sessions only).

**Cost.** 1 session. **Depends on.** U6, U8, U9. **Status.** not started.

---

### U11 — Trend region and H4: the regime-weighted blend

**Goal.** Register and run §6.5.

**Scope.** `strategies/trend.py` (TSMOM region; Donchian-55 variant; vol-managed buy-and-hold benchmark),
`strategies/blend.py`, `specs/H4_blend.yaml`, `results/reports/U11_blend.md` with the matched-volatility
comparison, the drawdown difference, the MR–TF correlation by year, and the blend's turnover and cost attributed
to each component.

**Done when.**
- [ ] Spec committed; run once; verdict in §10.
- [ ] Causality harness on the blend and the weights.

**Reviewer focus.** The static blend as the benchmark; the shrink κ; TF parameters untouched from the literature;
the vol-managed core's exposure cap.

**Cost.** 1 session. **Depends on.** U10 (or U6 + U9 if H3 is skipped under §6.2's "rejected" path). **Status.**
not started.

---

### U12 — Holdout read, program verdict, registry, README

**Goal.** One audited read of 2026-01-01 → freeze on the registered configurations; the program verdict; the
registry for the forward test.

**Scope.** `experiments/runner.py --final` (refuses unless every registered spec is included; writes one line to
`results/holdout_access.jsonl`; refuses a second batch); `results/reports/U12_program.md` (H1–H4 verdicts with
Holm, the holdout slice as a robustness row, cost curve, account check); `specs/registry.yaml` (adopted
configurations: spec SHA, code hash, cell hashes, freeze date, registered expectation with its 90 % interval,
the registered BOCPD run length λ = 126 and the kill rule for U13, the H5 rule); README rewritten with the result.

**Done when.**
- [ ] Holdout accessed exactly once by research code (one audit line); the report says what the slice shows and
      that it is not the verdict.
- [ ] Registry committed; freeze date set; §9 decisions recorded.

**Cost.** ½ session. **Depends on.** U11. **Status.** not started.

---

### U13 — Forward test: paper trading with reconciliation and a BOCPD monitor

**Goal.** Trade the registry on an Alpaca paper account from the freeze date; score forward sessions as the
holdout; measure what the paper environment lets us measure.

**Scope.**
- `live/bars.py`: at 16:25 ET the completed session's closing cross (SIP trades), `1DayPrint` volume and the VIX close (both available on the
  Basic plan); for a `moc` configuration (only if registered) the 15:45 snapshot from the real-time feed (SIP with
  Algo Trader Plus; IEX volume is not usable — U2). Every forward read logs an audit row.
- `live/signals.py`: the registered strategy at the frozen code hash; a mismatch refuses to trade.
- `live/orders.py`: `opg` market-on-open submitted at 19:05 ET (inside Alpaca's acceptance window) with a 09:20
  retry; `cls` by 15:48 for `moc`; idempotent on re-run; timeouts on every call; flatten-on-failure.
- `live/reconcile.py`: modelled vs actual fill per order. **Paper fills are simulated by Alpaca from the live
  quote, not auction prints**: the reconciliation measures the simulator. It is recorded as `paper_cost.csv`;
  the `measured` cost column is populated only from real one-share live trades if the user opts in (§9.8).
- `live/monitor.py`: BOCPD (R3 configuration, λ = 126) and the U7 Kalman tracker (`local_level_track`, the
  causal form; it declares its q or refit rule and the false-crossing share read off U4's operating curve, §11) on
  the **modelled** forward stream; erosion trigger = E[run length] < λ/4 for 5 sessions; shock trigger = P(changepoint) > 0.5; either →
  flatten and alert. Because the kill switch edits the traded stream, `live/score.py` scores **two streams**: the
  registered strategy's modelled forward stream (the H5 object) and the actually traded stream (the account).
- Weekly determinism job: every forward session's target recomputed from cached bars equals the live log.

**Done when.**
- [ ] One week of one-share dry runs: orders, fills, reconciliation and scorer rows exist.
- [ ] Monitor tests: BOCPD flags a planted mean shift within 20 sessions; < 1 false flag per 1,000 stationary
      simulations at λ = 126.
- [ ] After 26 weeks: the H5 line (consistent / inconsistent) in §10; the monthly track-record line thereafter.

**Reviewer focus.** Decision time vs the 15-minute SIP delay; the OPG window; an unfilled `opg` leaving a stale
position; the two-stream scoring; no leverage, no day trades.

**Cost.** 2 sessions of code; ≥ 6 months of calendar time. **Depends on.** U12, §9.3, §9.4, §9.8. **Status.** not
started.

---

### U14 (optional, gated) — Intraday session-VWAP reversion family

The inherited rule uses "VWAP" as a multi-session moving average. The mechanism the name refers to — orders
benchmarked to the session VWAP and execution algorithms pulling price back toward it — lives intraday. A family
on 5Min bars (decisions every 30 minutes from 10:30; deviation of price from the session VWAP in units of the
session's realized range; flat overnight; costed at intraday fills with the cost curve deciding) is the natural
second family. **Gate:** H1 at least "promising" *and* §9.7 answered yes. Needs an intraday cost model
(quotes-based half-spread) and, live, the SIP plan. Budget 1 + 8; spec written before any run; the sibling repo's
`session_vwap` and band-stop code is the starting point.

### U15 (optional, gated) — Conditioning overlay with machine learning

Only after a configuration is "consistent" in U13: one pre-registered question — does a calibrated logistic model
on the U8/U9 state variables improve the registered stream's net Sharpe after its extra turnover — purged k-fold,
rolling calibration, linear sizing, no model zoo.

---

## 8. Definitions (compact; SPEC.md elaborates from U3/U4)

**8.1 VWAP and deviations.** `VWAP_n(t) = Σ_{i=0}^{n−1} p_{t−i} v_{t−i} / Σ v_{t−i}` with `p` = `1DayAuction` close and
`v` = `1DayPrint` volume (headline), RTH volume + the closing-cross size or the sessions' own VWAPs (variants). **Trading deviation**
`d_t = close_t / VWAP_n(t) − 1` (includes session t, as the rule does). Cell target `s_t = −1` if `d_t > b`, `+1` if
`d_t < −b`, else 0; region target = mean over cells. **State deviation** `d*_t = close_t / VWAP_5(t−1) − 1`
(anchor over sessions t−5 … t−1, excluding t, so today's close does not sit in both numerator and denominator);
its AR(1) φ measures reversion speed *relative to the simulated random-walk value* reported beside it (a
deviation from any trailing anchor reverts by construction; what varies across time is how fast). Hurst-type
estimators are never run on `d_t` or `d*_t`.

**8.2 Timing.** Decision at the strategy's declared cut-off (`close` = 16:25 ET from the completed session, or
`t1545`) using only the `DecisionView`; fill at the next auction print per §7 U3. Equity marked at `1DayAuction`
closes.

**8.3 Costs.** Per fill: half-spread + slippage by fill kind (registered 1.0 bp round trip → `measured`); SEC §31
and FINRA TAF on sells from the dated table; borrow on short notional per session; cash yield DTB3 ACT/360 on free
cash. Cost curve 0.3 / 1.0 / 2.3 / measured.

**8.4 Statistics.** PSR(SR*) = Φ((SR̂ − SR*)√(T−1) / √(1 − γ₃SR̂ + (γ₄−1)/4·SR̂²)), γ₄ raw kurtosis, per-period SR.
MinTRL(SR*, α) = 1 + (1 − γ₃SR̂ + (γ₄−1)/4·SR̂²)(Φ⁻¹(1−α)/(SR̂ − SR*))². Mean and conditional-mean tests: studentized
circular block bootstrap (block 21, 5,000) with Bartlett HAC SE at the block bandwidth. Paired Sharpe difference:
Ledoit & Wolf (2008). Holm step-down. MDE per §6.6. Kalman tracker: local level, MLE of q = σ²_η/σ²_ε with its
profile, filtered μ_T and variance.

**8.5 Hurst-type estimators** (all on return increments r; the profile is integrated internally). **DFA-1:**
Y = cumsum(r − r̄); for scale s split Y into ⌊n/s⌋ segments from each end, detrend each by a line, F²(s) = mean
squared residual; α = slope of log F(s) on log s over the scale set. **MF-DFA:** F_q(s) = (mean F²(s,ν)^{q/2})^{1/q}
(q = 0 by the log average); h(q) = slope; τ(q) = q·h(q) − 1; α = τ′(q), f(α) = qα − τ(q); Δα = α_max − α_min.
**Abry–Veitch:** detail variances μ_j = mean d²_{j,k} of the causal MODWT; weighted LS of log₂ μ_j on j over
j₁ … j₂; H = (slope + 1)/2. **R/S:** Lo's modified statistic (noise study only). Favourable for reversion: α or H
below its trailing-504 median.

**8.6 Causality test.** For f(view) → frame aligned to the decision index and ≥ 5 seeded cut points c: the output
at decisions ≤ c is unchanged (atol 1e-12, NaN-equal) when every observation **after the decision time of c** is
replaced by an independent random walk, and also when the data are truncated there. For a `close` decision that
means rows > c; for a `t1545` decision it means session c's own close and full-session volume as well. Applies to
features, state estimators, regime models (including refits and standardization), strategies, gates, blends and
the engine. Every unit adds a mutant test proving the harness catches a planted one-bar peek.

**8.7 Spec file (YAML).** `id, mechanism, instruments{headline, splits}, strategy{name, region}, execution
{decision, fill}, cost{registered_bp, curve}, test{kind ∈ {mean_vs_zero, cond_mean_diff, paired_sharpe}, sided,
block, n_boot, seed}, floors{...}, expected{magnitude, source}, power{n, sigma_d, pi | rho, k, mde, diagnostic},
headlines[≤ 4], variants[≤ 8 | ≤ 12], splits[], budget, registered_sha`.

**8.8 Ledger row.** `cell_hash, spec_id, spec_sha, kind ∈ {headline, variant, split, diag, holdout, forward},
git_sha, code_hash, data_hash, vintage, started_at, runtime_s, n_obs, ret_ann, vol_ann, sharpe, psr, mean_bp_day,
p_boot, t_nw, lower90, max_dd, turnover, round_trips_yr, flat_share, cost_bp, status, error_path`.

---

## 9. Decisions needed from the user (defaults in bold)

1. **Secondary daily source for 1993–2015** (`diag`, also used for estimator warm-up): **answered 2026-10-20: Yahoo
   chart API** (no key; the source `VWAP.R` used). Stooq was chosen first but now serves a JavaScript bot check
   and cannot be fetched by a script. Other options were Tiingo (free key) / none (then H2's long tables, the warm-up and the long-sample power rows are lost
   and every gate starts ≈ 2020). Blocks U1.
2. **Headline execution:** **`close` at 16:25 ET → `next_open` via OPG (Basic plan suffices)** / `t1545` → `moc`
   via CLS (captures the close-to-close reversal the R script assumed; needs a real-time SIP snapshot). Decided
   by feasibility (U2's evidence), not by P&L. Blocks U6's spec; the default is used if unanswered.
3. **Data plan for the forward test:** **Basic** / Algo Trader Plus (needed for a faithful `moc` snapshot and for
   U14). Blocks U13 (and the `moc` variant).
4. **Paper account profile:** **answered 2026-10-20: margin, $30k, no leverage** (shorts allowed; profile
   `margin_30k`, |exposure| ≤ 1) / cash (long-only; the short side becomes a variant). Blocked U3's account check
   and U13.
5. **Instruments:** **SPY headline; QQQ, IWM, DIA as splits** / SPY only.
6. **Holdout start:** **2026-01-01**. Fixed in U0; a later change is an amendment.
7. **U14 (intraday session-VWAP family)** wanted once H1's verdict is in: yes / **decide later**.
8. **Real one-share live trades to measure auction costs** during U13 (the paper environment cannot): yes /
   **no**.

---

## 10. Status

| unit | title | status | verdict / key numbers |
|---|---|---|---|
| U0 | scaffold, protocol, hygiene | done, merged | 7 tests; K = 1 |
| U1 | data layer, `diag`, warm-up | done, merged | 43 tests; prints from `1DayAuction` (SIP crosses); own dividend table; splice corr 0.99999996; vintage_2026-10 frozen |
| U2 | live plumbing smoke | not started | |
| U3 | backtest, costs, account; SPEC §1–§5 | done, merged | 86 tests; vectorized = loop ≤ 1e-14 on 4 × 2,514 sessions; `margin_30k` registered; no P&L read |
| U4 | statistics toolkit; SPEC §6 | done, merged | 118 tests; mean-test size 0.050 / 0.056 (iid / GARCH-t, 5,000 sims); Bai–Perron size 0.057 / 0.059; Kalman criterion amended (crossing-lag median 262 on the causal track) |
| U5 | reproduce and diagnose | done, merged | K = 112 (every window; 48 dev rows); README reproduced 0.476 / 0.318 vs 0.468 / 0.314; edge is 2007–09; `diag` R₀ +2.3 bp/day net vs MDE 4.6 → H1 diagnostic |
| U6 | H1 existence | done, merged | **rejected (diagnostic)**: R₀ −0.23 bp/day net @1.0 bp, p 0.575 (p·K 1.0), lower 90 % −1.95; floors: net −0.58 %/yr ✗, gross 2.7 bp/rt ✓, coherence 6/16 ✗; cost curve +0.04 / −0.04 / −0.23 / −0.57 bp/day at 0 / 0.3 / 1.0 / 2.3 bp; 196 tests |
| U7 | H2 persistence | done, merged | last-5-yr NW t 0.19 (+0.40 bp/day); Bai–Perron 0 breaks (p 0.81); P(μ_T > 0) = 0.447 (q_zero; causal track 0.447); `diag` edge = 2006–10 only |
| U8 | state estimators + noise study | not started | |
| U9 | regime models + detection study | not started | |
| U10 | H3 predictability | not started | |
| U11 | H4 blend | not started | |
| U12 | holdout read, verdict, registry | not started | |
| U13 | forward test | not started | |
| U14 | intraday session-VWAP family | gated | |
| U15 | ML conditioning overlay | gated | |

Looks ledger K = 153, every window counted (§11 ruling 2026-10-21): 87 development-window (1 seed + 47 from U5 + 27
from U6's H1 run + 12 from U7) and 66 `diag`-only. Trials counted = 27 (H1: 1 headline, 7 variants, 19 splits). Holdout
accesses = 0.

---

## 11. Handoff notes and amendments (newest first)

- **2026-10-21 — U7 merge and ruling (user).** `unit/07-persistence` merged into `dev` (71a227f).
  - **U8 and U9 are built as planned**, not cut to a B1-only "U10-lite". The user's reason: the estimators and
    regime models may improve on the vanilla rule.
  - Protocol consequences, which stand regardless:
    - U8 and U9 are synthetic-first and freeze their configurations before any market P&L is read (as written).
    - The §6.2 routing still governs the registered H3 / H4 tests (on `diag`, as diagnostics) unless amended.
    - Any "improvement" found later on the development window is a new hypothesis, read under K ≥ 153. It can be
      confirmed only by the holdout read (U12) and the forward test (U13), not by the development window.
  - Next: U8.

- 2026-10-21 — U7 H2 persistence. Branch `unit/07-persistence` @ 39b5698. Done-when: 2/2 ticked
  (`results/reports/U07_status.md`, report `U07_persistence.md`).
  - Looks: K was 139, is 153 (dev 12, diag 2).
  - Reviewer (run before the real run): BREAKING 0 / SEVERE 0 / MINOR 7 (fixed 6, documented 1).
  - Summary: last-5-year t 0.19; no Bai–Perron break; P(μ_T > 0) = 0.447 with q = 0 (every causal refit too).
    There is no moving edge to track.
  - `diag` at the historical cost schedule: the edge is the 2006–10 windows (+10 to +12 bp/day) and ≈ 0 or
    negative elsewhere.
  - The 2016–2025 conditional tables lean high-vol / high-VIX good and low-VIX bad (low-VIX tercile −2.8
    [−5.0, −0.5]), ≈ 3 bp/day apart, far below any H3 MDE.
  - Decisions inside the unit:
    - the `diag` historical cost schedule is one tick / previous close ($1/16 to 2001-04-09, then $0.01; floor
      1.0 bp; no fees), an assumption stated in `experiments/persistence.py`;
    - no `diag` conditional tables (H3's sample).
  - `EdgeMonitor` (live/monitor.py) is the U13 monitor.
  - Open questions:
    - merge `unit/07-persistence`;
    - the scope of U8 / U9 / U10 under the routing: H3 on `diag` needs only B1 (the vol baseline), so U8 / U9
      could be cut to that;
    - whether U2 / U13 still run under a null.
  - Next: U10-lite (H3 on `diag`, B1 only), if the user agrees to the cut. Otherwise U8.

- **2026-10-21 — U6 rulings (user) and merge.** `unit/06-h1` merged into `dev` (5f150e0). **The §6.2 routing after
  "rejected" is followed:** U7 (H2) runs on the H1 headline stream as registered. H3 and H4 then run on `diag`
  with the vol baseline only, as diagnostics, and the program reports a null. Consequence for U7: no conditional
  table is computed on `diag`, because those sessions are H3's test sample. Next: U7.

- 2026-10-21 — U6 H1 registration and existence test. Branch `unit/06-h1` @ 2d39c0a (spec registered at 138a7b2).
  Done-when: 3/3 ticked (`results/reports/U06_status.md`, report `U06_H1.md`).
  - Looks: K was 112, is 139 (one dev row, n 27). Trials: 27 ledger rows.
  - Reviewer (run before the registered run, because a spec runs once): BREAKING 0 / SEVERE 0 / MINOR 6 (fixed 6).
  - **Verdict: rejected (diagnostic).**
    - R₀ on SPY: −0.23 bp/day net excess at 1.0 bp, p 0.575, 90 % interval [−2.49, +2.04].
    - Gross +0.73 bp/day (2.7 bp per round trip); break-even ≈ 0.1 bp.
    - Coherence 6/16.
    - The interval's upper end lies below U5's expected 2.3 bp/day; it does not exclude the 0.7–1.4 ex-crisis
      reference.
    - The long-only variant's +2.40 bp/day is beta 0.52 (alpha −0.8 %/yr), not evidence.
  - Decisions inside the unit:
    - "promising" needs p < 0.10 **and** lower90 > 0 (§6.2 states them as one; the bootstrap can split them by one
      order statistic);
    - the RTH-volume variant adds the closing-cross size (§8.1's wording; applies U1's proposal to the variant);
    - the session-VWAP variant keeps the headline's 1DayPrint weights (one input changed per variant);
    - the `moc` variant is not built (§9.2 / §9.3 at their defaults);
    - U5's deferred row-range check is now `looks.check_window`, enforced by the runner.
  - Open questions:
    - merge `unit/06-h1`;
    - the §6.2 routing after "rejected": U7 (H2) on the headline stream as registered, then H3 and H4 on `diag`
      with the vol baseline only as diagnostics, and a null programme report — or stop at the null.
  - Next: U7, if the routing is accepted. U2 (live plumbing) is still not started, and the forward test it feeds is
    moot under a null.

- **2026-10-21 — U5 rulings (user) and merge.** `unit/05-diagnose` merged into `dev` (7ac429c).
  - **Look counting: every window counts in K** (the conservative option of the proposal below). K = 112.
    The `window` field stays as a label; `looks_total()` counts all rows, `looks_total(window="dev")` the
    development-window share (48). p·K in every report uses 112.
  - **H1 is registered as a diagnostic in U6** (§7 U6 rewritten).
  - **The counted missing-cross fallback moved into `load_long`** (`bars.rth_fallback`, shared with `load_market`;
    listed in the splice report's `fallback_sessions`; a session with no RTH bars raises). Frames identical before
    and after on SPY, QQQ, IWM, DIA.
  - Incident: during the equivalence check, `git worktree remove --force` on a scratch worktree that held a
    junction to `data/cache` deleted the cache. The user restored it from a Windows previous version; both
    manifests re-verify (`2026-10` a2efbd9f…674c, `diag` fe8ff374…7cf) and the built frames equal the
    pre-deletion dump. No result changed.
  - Next: U6 (§9.2 default `next_open` unless answered).

- 2026-10-21 — U5 reproduce and diagnose. Branch `unit/05-diagnose` @ 4025902. Done-when: 3/3 ticked
  (`results/reports/U05_status.md`, report `U05_diagnosis.md`).
  - Looks: K was 1, is 48 (dev); 112 with `diag`-only looks. That includes one aborted-run row and two post-review
    rows.
  - Reviewer: BREAKING 0 / SEVERE 0 / MINOR 7 (fixed 7).
  - Findings:
    - The README figure reproduces (0.476 / 0.318 on Yahoo 2007-01-03 → 2023-01-06). Its edge over SPY is
      ΔSharpe +0.14, p 0.64.
    - The edge comes from 2007–09.
    - On `diag`, R₀ earns +2.3 bp/day net at 1.0 bp, but 0.7–1.4 outside 2007–09.
    - The MDE is 4.6 bp/day.
  - Open questions: the look-window proposal below; whether U6 registers H1 as a diagnostic (recommended);
    whether the counted missing-cross fallback moves into `load_long` (needed by U8 warm-up).
  - Next: merge `unit/05-diagnose`; U6 (needs §9.2, default `next_open`). R₀'s cell code (`strategies/vwap_mr.py`)
    already exists from U5.
- **2026-10-21 — U5 proposal: look windows (ruled 2026-10-21 above: every window counts).**
  - The change: each look row carries `window: "dev" | "diag"`, and K (`looks_total`) counts dev rows only. A row
    without the field counts as dev.
  - Why: the Bonferroni bound p·K guards development-window tests. `diag`-only looks (1993–2015) never touch that
    window, and counting them would put 64 more configurations into H1's p·K.
  - Counting rule: n = the configurations read (strategy cell × execution, or statistic × instrument × window).
    Years, sub-periods, cost levels and legs are splits of one look.
  - Alternatives: count every window in K (112), or count reversal statistics per symbol rather than per statistic
    (dev K would be 1 + 2 + 4 = 7).
  - A row-range check on the window label belongs with U6's runner.

- 2026-10-20 — U4 statistics toolkit; SPEC.md §6. Branch `unit/04-stats` @ c043a47. Done-when: 1 of 3 ticked, 1 met
  within Monte Carlo error, 1 partly met (`results/reports/U04_status.md`, raw output `U04_checks.md`).
  - Looks: K was 1, is 1 (synthetic only).
  - Reviewer: BREAKING 0 / SEVERE 3 / MINOR 6. All SEVERE fixed:
    - Bai–Perron now normalises by the null model's prewhitened-Andrews ω̂².
    - There is one supF(1), not two.
    - A causal `local_level_track` was added.
  - MINOR: one mutant survives (unstudentized roots), deferred.
  - Open questions: the Kalman amendment below; whether a done-when measured at 1,000 simulations should be judged
    on pooled replications (this unit did, and says so).
  - Next: merge `unit/04-stats`. U5 then has all its dependencies (U1, U3, U4); U2 is still open.
- **2026-10-20 — U4 amendment (applied 2026-10-20 by the user's decision, first option below; §7 U4 done-when
  rewritten, `unit/04-stats` merged into `dev`): the Kalman crossing criterion.** §7 U4 asks that P(μ > 0)
  cross 0.5 within 120 sessions of a planted 10 bp/day shift at σ 1 %.
  - **Measured** (−5 → +5 bp/day, n 2,500): median crossing lag 226 at the whole-sample MLE q, 262 on the causal
    track. A fixed q reaches a median under 120 only for q ≥ 3e-5, and then P > 0.5 already holds on 18–25 % of
    pre-shift sessions while the edge is still negative.
  - **Why it cannot be met:** (σ/δ)² = 100 sessions are needed just to separate a 10 bp change by one standard
    error, and the MLE cannot resolve a q that small (q̂ ≈ 5e-6; `q_zero` in 52 % of runs).
  - **Proposal:** replace the criterion with a reported operating curve: crossing lag against pre-shift
    false-crossing share over q, as `scripts/u04_checks.py` §7 prints it. U13's monitor would then declare its q
    (or its refit rule) and the false-alarm share that goes with it. Alternatively, keep a latency criterion but
    plant a shift the tracker can resolve: at 50 bp/day, the test's −25 → +25 bp is crossed within 120 sessions.

- 2026-10-20 — U3 backtest engine, costs, account; SPEC.md §1–§5. Branch `unit/03-backtest` @ 909762d.
  Done-when: 3/3 ticked (`results/reports/U03_status.md`, raw output `U03_checks.md`). Looks: K was 1, is 1 (no
  strategy P&L read). Reviewer: BREAKING 0 / SEVERE 0 / MINOR 10 (fixed 9, deferred 1 to U5: the diag guard
  protects the `"alpaca"` label only). §9.4 answered (margin, $30k, no leverage). U1 deferrals closed: sticky diag
  guard (`Market.source`), `Exo.asof(max_age=)`, raw next-open legs + `dividend`. Open questions: the two proposals
  below; the RTH-volume proposal from U1. Next: merge `unit/03-backtest`; U4 (statistics) is the remaining
  dependency of U5; U2 can run in parallel.
- **2026-10-20 — U3 amendment and proposals.** (1) §7 U3's sign-flip bound is off by a factor 2: under
  `next_open`, g + g′ = 2ab·r_on·r_id exactly, so the bound is Σ|g + g′| ≤ 2·Σ|r_on·r_id| (tested; the reviewer
  measured 0.01076 vs 0.00540 for a constant ±1 target). (2) Conventions fixed in SPEC.md, not in this file before:
  the dividend belongs to the overnight holder; short proceeds earn nothing (a short pays the T-bill in excess
  terms); the engine keeps the exposure at f between fills (an implicit daily rebalance, charged only on |Δf|); a
  missing cross falls back to that session's raw 5Min bar, counted. **Proposal A (not applied):** report
  whole-share tracking error in the account check (a 1/16 region step at $30k is ≈ 3 SPY shares; Alpaca refuses
  fractional OPG/CLS orders). **Proposal B (not applied):** for the `moc` variant, decide in U6 whether the one
  session per SPY / IWM / DIA whose fallback close the 15:45 decision already saw (2019-08-12) is skipped or kept
  and reported (`fallbacks["close_seen_at_decision"]`).
- 2026-10-20 — U1 data layer. Branch `unit/01-data` @ 69e6b7d. Done-when: 4/4 ticked
  (`results/reports/U01_status.md`, raw output `U01_checks.md`). Looks: K was 1, is 1. Reviewer: BREAKING 0 /
  SEVERE 1 / MINOR 6 (fixed 4, deferred 2 to U3/U8: sticky diag guard, VIX staleness bound; raw next-open legs
  documented). Scope widened by the user inside U1 (amendment below): `1DayAuction` from SIP crosses and a
  reconciled dividend table. Open questions: §9.4 (blocks U3); the RTH-volume-variant proposal. Next: merge
  `unit/01-data`, then U3 (needs §9.4 default or answer) and U4 in parallel.
- **2026-10-20 — U1 amendment: auction prints and dividends (user decisions, made inside U1).** The live smoke
  showed two Alpaca data problems the plan had assumed away. (1) `1DayPrint` open/close are not the official
  auction prints on many 2016–2022 sessions: SPY's close differs from the official close on 924 of 2,514 sessions
  (worst 288 bp, 2020-03-12: 255.24 vs 248.11), QQQ's open on 752, IWM's close on 693; from 2023 they agree. (2)
  Alpaca's `adjustment=all` misses SPY 2016-03-18 and 2018-06-15 and DIA 2016-03-18 and doubles QQQ 2022-09-19;
  Yahoo misses QQQ 2020-09-21. Decisions: prints come from a new `1DayAuction` timeframe built from SIP trades
  (listing exchange's `O`/`6` cross, else its `Q`/`M` official report); total-return adjustment from our own
  dividend table (Alpaca × Yahoo, ten disagreements resolved with cited third sources in
  `data/dividends_overrides.csv`), applied to `1DayAuction` and the 5Min family alike; the diag splice is raw vs
  raw. §4, §5, §6.1, §7 U3/U13 and §8.1–8.2 now name `1DayAuction` where they named the `1DayPrint` close.
  **Proposal (not applied):** the RTH-only volume variant should add the closing-cross size, because the 16:00
  cross sits in the 16:00 bar, which the RTH 5Min filter drops (cross ≈ 1.8 % of SPY's consolidated volume).
- 2026-10-20 — U1 decisions (before code). §9.1: Stooq now serves a JavaScript bot check, so the user chose the
  **Yahoo chart API** for `diag` (no key; also `VWAP.R`'s source). DTB3 comes from the **FRED API** with
  `FRED_API_KEY` in `.env`, because fredgraph.csv times out from this machine (curl and requests). The frozen vintage
  is **`vintage_2026-10`**. U1 runs on `unit/01-data`.
- 2026-10-20 — U0 scaffold, protocol and hygiene. Branch `unit/00-scaffold` @ d623674. Done-when: 4/4 ticked
  (`results/reports/U00_status.md`). Looks: K was 0, is 1 (seed). Reviewer: BREAKING 0 / SEVERE 1 / MINOR 5
  (fixed 4, deferred 2: Windows read-during-append `PermissionError`, inherited from upstream; credentials
  migration is the user's). Ported looks reader made strict (raises on a malformed line). Ruff now excludes
  `*.md` (it reformatted §5's code block). Open questions: the user moves `alpaca_api.yaml` into `.env` and
  deletes it; §9.1 and §9.4 still unanswered (defaults stand). Next: merge `unit/00-scaffold` into `dev`, then U1
  (needs §9.1) and, in parallel, U4.
- **2026-10-20 — v1 after adversarial review of the draft.** Sources read: `VWAP.R`, the `dev`-branch Python port,
  wavelet-meta-model's `PLAN*.md`/`REVIEW.md`/`SPEC.md`/`handoff.md` and a survey of its code. No market data was
  read; no look was taken. Changes the review forced: (1) `VWAP.R` pulls Yahoo data from **2007**, not 2012 (no
  `from=`), so the README figure includes 2008–09 and U5 reproduces from 2007 by sub-period; (2) the next-open
  return decomposition is compounded, with the sign-flip invariant relaxed to first order; (3) H3's test is a
  conditional mean difference — an uninformative gate "in" a fraction π of the time has Sharpe √π × the
  ungated, so ΔSharpe = 0 was the wrong null; (4) MOC timing respects Alpaca's 15:50 CLS cutoff (`snap1545`,
  order by 15:48) and OPG orders go in after 19:00 ET; (5) the region R₀ and the execution convention are fixed
  in §6.1 from the mechanism and feasibility, and strategy P&L on the Alpaca development window is not read
  before U6 (the draft's U4 would have chosen the region on the test window); (6) the causality harness cuts at
  the decision time, and strategies receive a `DecisionView`, so a 15:45 decision cannot read the close; (7) `diag`
  bars warm up estimators so gates exist from 2016 (otherwise H3/H4 would start ≈ 2020 on ≈ 1,500 sessions); (8)
  the frozen data vintage (adjusted bars re-scale at every ex-dividend); (9) paper fills are simulated and the
  kill switch edits the traded stream, so U13 scores two streams and the "measured cost" comes only from opt-in
  live one-share trades; (10) cut: the separate walk-forward unit (its grid is variants in U10/U11), GHE, the
  surrogate tests, the 3-state HMM, the DSR/PBO/BH columns, the pooled-ETF variant, most of the H2 battery, the
  account-profile machinery beyond margin/cash and Reg-T; (11) labels B/E/R defined in §6.4; (12) PLAN/results
  edits land on `dev`. Next: U0, after §9.1 and §9.4 are answered (defaults otherwise).

**Handoff-note template** (append one per unit, newest first):
`- YYYY-MM-DD — UNN <title>. Branch <unit/NN-slug> @ <sha>. Done-when: n/m ticked (status note path). Looks: K
  was/is. Reviewer: BREAKING n / SEVERE n / MINOR n (fixed f, deferred d). Open questions: … . Next: … .`

---

## 12. Reading list (the claims above cite these)

- Adams & MacKay (2007), *Bayesian Online Changepoint Detection*; Fearnhead & Liu (2007).
- Hamilton (1989); Kim (1994). `statsmodels` `MarkovAutoregression` docs (filtered vs smoothed probabilities).
- Peng et al. (1994), DFA; Kantelhardt et al. (2002), MF-DFA; Abry & Veitch (1998), wavelet estimator; Lo (1991),
  modified R/S; Percival & Walden (2000), MODWT.
- Campbell, Grossman & Wang (1993), trading volume and serial correlation in stock returns; Lo & MacKinlay
  (1988), variance ratio; Nagel (2012), *Evaporating liquidity*; Jegadeesh (1990) and Lehmann (1990) for the
  cross-sectional reversal literature this is *not*.
- Moskowitz, Ooi & Pedersen (2012), time-series momentum; Moreira & Muir (2017), volatility-managed portfolios.
- Bailey & López de Prado (2012) PSR/MinTRL; (2014) DSR (used only as a PSR test vector). Ledoit & Wolf (2008),
  robust Sharpe tests. Politis & Romano (1992), circular block bootstrap. Harvey & Liu (2015), multiple testing in
  finance. Bai & Perron (1998, 2003), structural breaks. Durbin & Koopman (2012), local level model.
- 0zean/wavelet-meta-model `REVIEW.md` §4–§8 and `PLAN3.md` §0–§4: mechanism-first, region-not-point,
  power-first, forward test as the holdout.
