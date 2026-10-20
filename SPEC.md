# SPEC — interfaces and conventions (v1, U3: §1–§5; U4: §6)

PLAN.md says what is built and why; this file says exactly what the code does. A change to a convention here is a
code change with a test, recorded in PLAN.md §11.

---

## 1. Data the engine consumes

`vwap_mr.backtest.market.Market` is the engine's one input. `load_market(symbol, start, end, *, vintage)` builds it
from the frozen vintage; nothing else may label a frame `source="alpaca"`.

| field | content |
|---|---|
| `sessions` | one row per exchange session, index = tz-aware NY midnight |
| `sessions.open_raw`, `close_raw` | the listing exchange's opening / closing cross (`1DayAuction`, raw $): **fill and mark prices** |
| `sessions.dividend` | cash dividend whose ex-date is that session, else 0 (reconciled table, U1) |
| `sessions.open`, `close` | the same prints × the total-return factor (exact close-to-close); **strategies only** |
| `sessions.volume` | `1DayPrint` consolidated volume (headline VWAP weight) |
| `sessions.open_fallback_raw`, `close_fallback_raw` | present only if some cross is missing: the first 5Min open / last 5Min close of that session's RTH bars (raw 5Min, not `1DayRTH`, which drops a session whose last bar is missing) |
| `snapshot` | `snap1545` (adjusted): `last`, `volume`, `vwap`, `n_bars` at session close − 15 min; needed by `t1545` strategies |
| `state` | state variables; row t = the value as of session t's **close** decision (it may use close_t); a `t1545` view sees rows ≤ t−1 (U8 onwards) |
| `exo` | `Exo` with VIX and DTB3 and their `available_at` rules (U1) |
| `calendar` | the vintage's exchange calendar: decision instants (16:25 / 15:45, early closes 13:25 / 12:45) |
| `source` | `"alpaca"` (needs `attrs["source"] == "alpaca"`, set by `load_market`), `"diag"`, `"synthetic"` |

Missing crosses in `vintage_2026-10` (2016-01-04 → 2025-12-31): SPY open 2023-06-05, close 2019-08-12; QQQ open
2020-03-16; IWM opens 2021-09-02, 2021-09-14, 2023-06-05, closes 2017-03-20, 2019-08-12; DIA open 2023-06-05,
closes 2017-03-20, 2019-08-12. On 2019-08-12 Alpaca's SIP has no SPY/IWM/DIA trades after 15:35 (DIA 15:25): the
fallback close is the last 5Min bar's close, 25–35 minutes early. Each use is printed by `load_market` and counted
in `Result.fallbacks`; a missing price with no fallback raises. `close_fallback_end` records when that bar ended; a
`moc` fill at a fallback that ended by 15:45 is a price the decision already saw and is counted separately as
`fallbacks["close_seen_at_decision"]` (1 session per SPY / IWM / DIA: 2019-08-12). A registered `moc` run reports
that count; it is not silently treated as an ordinary fill.

**diag guard (sticky).** A frame tagged diag (`attrs` or `is_diag`) cannot be passed as `"alpaca"` or `"synthetic"`;
a frame rebuilt from columns loses `attrs["source"]` and cannot be passed as `"alpaca"`; `run` refuses
`source="diag"`. Diagnostic runs (U5) go through `run_diagnostic` and are flagged `Result.diagnostic`.
Limitation: a frame rebuilt from diag columns (attrs and `is_diag` dropped) can still be labelled `"synthetic"`; the
guard protects the `"alpaca"` label only, so U5 must build diag markets from `load_spy_long` frames.

## 2. Strategy protocol

```python
class DecisionView(NamedTuple):
    sessions: pd.DataFrame      # close: sessions ≤ t;  t1545: sessions ≤ t−1
    snapshot: pd.Series | None  # t1545: session t's snap1545 row; close: None
    state: pd.DataFrame | None  # rows ≤ t
    exo: ExoView | None         # .asof(name) at the decision instant only
    when: pd.Timestamp          # the decision instant (tz-aware NY)

class Strategy(Protocol):
    name: str
    decision: DecisionTime      # CLOSE (16:25 ET) or T1545 (15:45 ET)
    fill: str                   # CLOSE → "next_open" (OPG), T1545 → "moc" (CLS); any other pairing raises
    def target(self, view: DecisionView) -> float: ...   # f_t ∈ [−1, 1], fraction of equity
    # optional: def targets(self, market: Market) -> pd.Series   (one f per session; tested equal to target())
```

- `target_series(strategy, market, vectorized=True)` uses `targets` when present, else builds every view; NaN or
  |f| > 1 raises (a strategy returns 0, not NaN, before it has history).
- A **region** (`strategies/region.py`) is a strategy whose target is the equal-weight mean of its cells'. Cells
  must share decision and fill.
- The vectorized path is not protected by the view; it is the path the causality harness (`tests/causality.py`,
  PLAN §8.6) tests, with mutants for a one-bar peek (close) and a 15:45 strategy that reads `close_t`.

## 3. Engine

`run(market, strategy, *, costs, cash_yield, equity0=30_000, vectorized=True) -> Result`. `costs` and
`cash_yield` (a float or a per-session Series of annual rates) are required: nothing defaults silently.

**Timing.** Row t of the ledger is session t: the return from close_{t−1} to close_t, the fill booked in t, and
equity at the closing cross of t.

| fill | decided | filled at | exposure overnight into t | exposure intraday t | cost booked in row |
|---|---|---|---|---|---|
| `next_open` | 16:25 of t | opening cross of t+1 | f_{t−2} | f_{t−1} | t+1 |
| `moc` | 15:45 of t | closing cross of t | f_{t−1} | f_{t−1} | t |
| `same_close` (diagnostic) | 16:25 of t | closing cross of t | f_{t−1} | f_{t−1} | t |

**Legs (raw prints, D = dividend with ex-date t).** r_on = (open_t + D_t) / close_{t−1} − 1 (the dividend belongs to
the overnight holder, who is the holder of record), r_id = close_t / open_t − 1, r_cc = (close_t + D_t) / close_{t−1}
− 1. Row 0 has no previous close: legs 0.

**Equity recursion** (E = equity at close_{t−1}; a = overnight exposure, b = exposure after the fill; carry and
cost below):

- `next_open`: E_fill = E·(1 + a·r_on) + carry; E_t = (E_fill − cost)·(1 + b·r_id).
  `gross` = (1 + a·r_on)(1 + b·r_id) − 1; `carry_ret` = carry/E·(1 + b·r_id); `cost_ret` = cost/E·(1 + b·r_id);
  `net` = E_t/E − 1 = gross + carry_ret − cost_ret exactly.
- `moc` / `same_close`: E_fill = E·(1 + a·r_cc) + carry; E_t = E_fill − cost. `gross` = a·r_cc;
  `net` = gross + carry/E − cost/E.

**Conventions.**
- The exposure is the state variable, not the share count: the engine returns every session to exactly f (an
  implicit daily rebalance at the fill print), and only |Δf| is traded and charged: traded notional = |b − a|·E_fill.
  A held-share account drifts instead: over one session an exposure f becomes f(1 + r)/(1 + f·r), off by
  f(1 − f)·r/(1 + f·r) — ≤ ¼|r| for f ∈ [0, 1], but ≈ 2|r| for a held −1 short. The unbooked rebalancing cost is
  that drift × c (≈ 0.01 bp per session for a −1 short at |r| = 1 %, the registered 0.5 bp per side), and the P&L
  gap to a held-share account is second order (≈ f(1 − f)·r_t·r_{t+1}). Whole-share OPG / CLS orders cannot execute
  the implicit rebalance exactly (PLAN §11 proposal).
- A long → short flip is one fill with |Δf| = 2 (it trades twice the notional).
- Excess return: `excess` = `net` − `rf`, rf = y_{t−1}·days/360 (the full-cash return), so a flat book has
  excess 0.
- Two implementations: vectorized (per-session growth factors × cumprod; it refuses a run where the TAF per-trade
  cap would bind) and a per-session dollar loop (applies the cap). Tested equal to 1e-12 on the fixture quarter;
  `scripts/u03_checks.py` shows ≤ 1e-14 on 2,514 sessions of each symbol.
- `reprice(result, round_trip_bp)` re-runs the equity path from the stored targets and legs at another spread cost
  (fees, borrow and carry unchanged): exact, not an approximation.

**Ledger columns.** `target`, `exp_overnight`, `exp_close`, `traded` (|Δf|), `notional`, `spread`, `sec_fee`,
`taf_fee`, `cost` ($), `interest`, `borrow` ($), `equity` ($, at the close), `gross`, `carry_ret`, `cost_ret`,
`net`, `rf`, `excess`.

**Worked examples** (E₀ = $30,000, registered cost 1.0 bp round trip, fees of 2024-06, cash yield 0, borrow
0.4 %/yr; targets 0.5, −0.5, −0.5 decided on three sessions). The only carry is `moc`'s overnight −0.5 short into
session 3: borrow $0.1675, included in its equity (without borrow it is 30,221.4720, `next_open`'s figure):

| fill | session | close | open | a → b | notional | spread | SEC | TAF | equity | net |
|---|---|---|---|---|---|---|---|---|---|---|
| `next_open` | 1 | 100 | 100 | 0 → 0 | 0 | 0 | 0 | 0 | 30,000.0000 | 0 |
| | 2 | 100 | 100 | 0 → 0.5 | 15,000.00 | 0.7500 | 0 | 0 | 29,999.2500 | −0.25 bp |
| | 3 | 100.5 | 101 | 0.5 → −0.5 | 30,149.25 | 1.5075 | 0.8381 | 0.0496 | 30,221.4720 | +74.08 bp |
| `moc` | 1 | 100 | — | 0 → 0.5 | 15,000.00 | 0.7500 | 0 | 0 | 29,999.2500 | −0.25 bp |
| | 2 | 101 | — | 0.5 → −0.5 | 30,149.25 | 1.5075 | 0.8381 | 0.0496 | 30,146.8511 | +49.20 bp |
| | 3 | 100.5 | — | −0.5 → −0.5 | 0 | 0 | 0 | 0 | 30,221.3045 | +24.70 bp |

`next_open` session 3: E_fill = 29,999.25 × (1 + 0.5 × 1 %) = 30,149.25; the flip trades 1.0 × E_fill; the sale
is 30,149.25 / 101 = 298.5 shares (SEC 27.80 / $1M, TAF $0.000166 / share); then the −0.5 exposure earns
−0.5 × (100.5/101 − 1). `moc` session 2 earns 0.5 × 1 % up to the closing cross, then pays the flip at that cross.

## 4. Costs

`CostModel(round_trip_bp=1.0, borrow_rate=0.004, fees=True)`; `ZERO` = no spread, no borrow, no fees.

| item | rule | source |
|---|---|---|
| spread + slippage | round_trip_bp / 2 one way on each fill's notional; both fills of a daily rule are auctions (`open_auction` for next_open, `close_auction` for moc) | PLAN §6.1: registered 1.0 bp; curve 0.3 / 1.0 / 2.3; `"measured"` reads `data/costs/measured.csv` (U13 opt-in live trades; raises until then) |
| SEC Section 31 | rate × sale proceeds on every sell, a short sale included | `data/costs/fees.csv`, by the fill's session date (row per SEC fee-rate advisory; $0.00 from 2025-05-14) |
| FINRA TAF | $/share sold, capped per trade | same table (SR-FINRA-2020-032 phases: $0.000119 → 0.000130 (2022) → 0.000145 (2023) → 0.000166 (2024)) |
| borrow | borrow_rate × short notional at the close × calendar days to the next session / 360 | default 0.4 %/yr; Alpaca charges easy-to-borrow ETFs less |
| cash | DTB3 / 100 as of each 16:25 decision (H.15 rule, U1) × free cash (1 − |exposure| at the close) × days / 360 | `tbill_yield(market)` raises on a missing or > 10-day-old value (`Exo.asof(max_age=…)`) |

Short-sale proceeds earn nothing (a retail account gets no rebate), so a short pays the T-bill in excess terms:
excess on a −1 book is −r − rf, not −(r − rf). The fee table's 2016 rows reach back to 2015-02-14; the 2026 rows
(holdout only) come from secondary sources and are labelled so in the file. Rounding of fees to the cent per
trade is not modelled (≤ $0.01 per sell).

## 5. Account

`check(result, profile) -> (tradable, reasons)` reads the ledger and never changes a P&L.

| profile | kind | equity | max \|exposure\| | shorts | PDT |
|---|---|---|---|---|---|
| `margin_30k` (registered, PLAN §9.4 answered 2026-10-20) | margin | $30,000 | 1.0 (no leverage, stricter than Reg-T's 2× overnight) | allowed while equity ≥ $2,000 | > 3 day trades in 5 sessions under $25k flags |
| `cash_30k` | cash | $30,000 | 1.0 | refused | — |

Day trades are counted from the fills (a session with both a buy and a sell of the symbol). The daily rules fill
once per session, so the count is 0 by construction; the counter exists so U14 is checked by the same code. Not
modelled (PLAN §11 proposal): integer shares, and Alpaca's refusal of fractional quantities on OPG / CLS orders.

## 6. Statistics (U4)

`vwap_mr/stats/`. Every function takes plain arrays of **daily** returns (per period; `_ann` fields × 252 for
means, × √252 for Sharpe ratios), raises on NaN, on a zero-variance stream and on too short a sample, and takes an
explicit `seed` wherever it draws. Every test returns a dict whose keys a ledger row (PLAN §8.8) reads directly.

### 6.1 Sharpe, PSR, MinTRL (`sharpe.py`)

| function | formula | reference |
|---|---|---|
| `sharpe_ratio(r)` | SR̂ = r̄ / s, s with ddof 1 | — |
| `return_moments(r)` | SR̂, γ₃ (population skew), γ₄ (**raw** kurtosis, 3 for a Normal), T | — |
| `psr(sr, T, γ₃, γ₄, SR*)` | Φ((SR̂ − SR*)·√(T − 1) / √(1 − γ₃SR̂ + (γ₄ − 1)/4·SR̂²)) | Bailey & López de Prado (2012) |
| `min_track_record_length(sr, SR*, γ₃, γ₄, α)` | 1 + (1 − γ₃SR̂ + (γ₄ − 1)/4·SR̂²)·(Φ⁻¹(1 − α)/(SR̂ − SR*))²; ∞ if SR̂ ≤ SR* | same |

SR̂ in `psr` and MinTRL is **per period**. An annualized Sharpe passed by mistake gives PSR ≈ 1 (tested). MinTRL is
the exact continuous inverse of PSR (PSR at MinTRL = 1 − α to 1e-12). Test vector: the 2014 DSR example, where
SR₀ = 0.1132 and PSR(SR₀) = 0.9004 with skew −3 and raw kurtosis 10, reproduced through `psr`. DSR itself is not
in the package (PLAN §3.2).

### 6.2 HAC and the block bootstrap (`bootstrap.py`)

- `hac_bartlett(Y, b)`: Γ₀ + Σ_{j<b}(1 − j/b)(Γ_j + Γ_jᵀ) on demeaned rows, Γ_j = Σ z_t z_{t−j}ᵀ / T. b may be
  fractional. For the bootstrap tests b is the block length (21), which equals Newey–West with b − 1 = 20 lags
  (tested equal).
- `andrews_bandwidth(e)`: Andrews (1991) Bartlett plug-in S_T = 1.1447·(α̂T)^{1/3}, where
  α̂ = 4ρ̂²/((1 − ρ̂)²(1 + ρ̂)²) and ρ̂ is the AR(1) coefficient, clipped to ±0.97.
- `newey_west_ols(y, X, lags)`: OLS with a constant, Bartlett-weighted sandwich with weights 1 − j/(lags + 1).
- `circular_block_indices(n, b, M, rng)`: l = ⌊n/b⌋ blocks with uniform starts, wrapping (Politis & Romano 1992).

Each of the three tests below works the same way:

1. Write θ̂ as a smooth function of sample means with influence values ψ_t.
2. The standard error is se = √(HAC_b(ψ)/T).
3. On each of the M resamples, compute θ* and s* = √(mean_j ζ_j² / L), where ζ_j = b^−½ Σ_{t∈block j} ψ*_t and L = l·b
   (Ledoit & Wolf 2008 §3.2).
4. The roots are (θ* − θ̂)/s*.
5. The p-values are:
   - one-sided (H₁: θ > 0): (1 + #{root ≥ θ̂/se}) / (M′ + 1);
   - two-sided: the same count on |root| ≥ |θ̂/se|.
6. The intervals are:
   - one-sided 1 − α lower bound: θ̂ − q_{1−α}(root)·se, with upper bound +∞;
   - two-sided: θ̂ ± q_{1−α}(|root|)·se.

M′ counts the finite roots. A resample with an undefined statistic is dropped and counted in `n_invalid`.

| test | θ | ψ_t | also returned |
|---|---|---|---|
| `mean_test(r)` (H1) | r̄ | r_t − r̄ | bp/day, annualized; Newey–West t (`nw_lags`, default 5) |
| `cond_mean_test(r, gate)` (H3) | Δμ = r̄_in − r̄_out | (g_t − π̂)·e_t / (π̂(1 − π̂)), e_t = r_t − r̄_{g_t} | μ_in, μ_out, π̂, gate episodes and their median length; Newey–West t of the slope of r on 1[gate] |
| `sharpe_test(a[, b])` (H4) | SR_a − SR_b (population std), or SR_a | delta method on y_t = (a, b, a², b²) | Δ annualized × √252 |

Notes on `cond_mean_test`:

- The session index is resampled, so each resample keeps the gate together with its return.
- Δμ, and its HAC standard error, equal the OLS slope of r on 1[gate] (tested to 1e-10).
- The gate must be boolean. A pre-history NaN is the caller's to decide, not the test's.

Defaults: block 21, M = 5,000 (simulations in U4 use 999), one-sided.

### 6.3 Multiplicity (`multiplicity.py`)

- `holm(p)`: p̃_(i) = max_{j≤i} min(1, (m − j + 1)·p_(j)), returned in input order.
  Hand example: [0.01, 0.04, 0.03, 0.005] → [0.03, 0.06, 0.06, 0.02].
- `bonferroni(p, K)`: min(1, p·K), the looks bound (re-exported by `experiments/looks.py`).

### 6.4 Power (`power.py`)

MDE = (z_{1−α/k} + z_{power})·SE, one-sided at level α/k (Holm's first step). σ_d is the daily volatility.

| kind | SE | PLAN §6.6 check (n 2,500, σ_d 1.05 %) |
|---|---|---|
| `mde_mean` | σ_d/√n | SE 2.1 bp, MDE 5.2 bp/day, Sharpe 0.79 |
| `mde_cond_diff` | σ_d·√(1/(πn) + 1/((1 − π)n)) | π 0.4: SE 4.3 bp, MDE 10.7 bp (k 1) / 13.2 bp (k 4) |
| `mde_delta_sharpe` | √([2(1 − ρ) + SR²(1 − ρ²)]/n·252) (Jobson & Korkie 1981 / Memmel 2003) | ρ 0.9: SE 0.14, MDE 0.35; ρ 0.7: MDE 0.61 |

The table's SE formulas assume iid returns, so the MDEs they give are lower bounds.

`simulate_rejection(kind, n, σ_d, effect, stream)` measures the rejection rate through the real tests:

- `stream` is iid normal, or `garch_t` (GARCH(1,1), α 0.05, β 0.90, variance targeted to σ_d, unit-variance t₅
  innovations, 500-session burn-in).
- For `cond_mean_diff`, the gate is an independent Bernoulli(π) coin and the effect is added on gate-on sessions, so
  Δμ = effect exactly.
- An uninformative gate leaves r unchanged. The null is therefore Δμ = 0, not ΔSharpe = 0; PLAN §6.4 explains why
  the √π scaling makes that the wrong null.

### 6.5 Breaks and the edge tracker (`breaks.py`)

**Bai–Perron, mean shifts** (Bai & Perron 1998, 2003):

- **Model:** y_t = μ_i + u_t, with m ≤ 5 breaks and segments of at least h = ⌈0.15·T⌉ sessions.
- **Partition:** for each m, `segment_dp` gives the exact global SSR minimiser by dynamic programming (tested against
  brute force).
- **supF(m):** (SSR₀ − SSR_m)/(m·ω̂²₀). ω̂²₀ is the long-run variance of the **null (0-break) residuals** (the LM
  form), computed as:
  - AR(1) prewhitening;
  - a Bartlett HAC at Andrews' bandwidth on the whitened series;
  - recolouring by 1/(1 − ρ̂)² (Andrews & Monahan 1992). A fixed bandwidth is an option.
- **Why the null residuals:** normalising by the alternative's residuals made the test oversized: 0.059 / 0.072 at
  AR(1) φ 0 / 0.2 over 2,000 simulations, against 0.053 / 0.064 now. Under a break ω̂²₀ is slightly inflated, costing
  a little power.
- **Null table** (`stats/bp_null.csv`):
  - It comes from `scripts/make_bp_null.py`: N = 2,000 points, 10,000 draws, seed 0. The script applies the same DP
    to iid N(0, 1) series of known variance.
  - It holds 999 quantiles of supF(1…5) and of UDmax = max_m supF(m).
  - With known variance at T = 2,500, the size at its 5 % critical value is 0.0498 (20,000 draws).
  - 95 % values: 8.63 / 7.14 / 5.95 / 4.96 / 3.87, UDmax 8.88.
  - supF(1) is cross-checked against an O(N) single-break formula on fresh draws.
- **Sequential test:** supF(l + 1 | l) is the largest single-break F inside the l-break segments, normalised by the
  l-break residuals' ω̂². Its first step is therefore supF(1) itself (tested). Its p-value is 1 − G(x)^{l+1}, where G
  is the supF(1) null.
  - Implied 5 % critical values for l = 1–4: 10.12 / 11.08 / 11.73 / 12.21.
  - Bai–Perron (2003) report 10.13 / 11.14 / 11.83 / 12.25, a check run by the U4 reviewer.
- **Number of breaks:** the sequential choice at α = 0.05.
- **Date intervals:** k̂ ± c·ω̂²/δ̂², where c is the 1 − α/2 quantile of argmax_s W(s) − |s|/2. That variable has a
  closed-form CDF (Bai 1997): c = 11.03 at 95 %, tested against simulation.
  - ω̂² here comes from the chosen model's residuals.
  - The interval is asymptotic. Its coverage falls short for shifts that are small relative to σ: the reviewer
    measured 0.85 / 0.905 / 0.945 at 20 / 30 / 50 bp/day, σ 1 %, T 1,500.
  - A report labels it "nominal 95 %".
- **Constant input:** a constant series raises.
- **p-value floor:** 0.001 (the table's resolution), reported as `p_floor`.

**Local level** (Durbin & Koopman 2012 §2):

- **Model:** y_t = μ_t + ε_t, μ_{t+1} = μ_t + η_t, with q = σ²_η/σ²_ε.
- **Estimation:** diffuse initialisation; σ²_ε concentrated out; the profile log-likelihood over q ∈ {0} ∪
  10^[−9, 0] (91 points), refined by a bounded search.
- **Agreement with statsmodels** `UnobservedComponents('llevel')`:
  - In the tested regime, the log-likelihood agrees to 1e-5 and the filter to 1e-5.
  - At q ≤ 1e-5 on σ 1 % data, statsmodels' steady-state shortcut drifts by up to 1e-4 in the filtered state and
    0.07 in the log-likelihood. With its tolerance set to 0 the two agree to about 5e-9.
- **`local_level(y)`** fits on all of y. For every t it returns the filtered μ_t, its sd, and P(μ_t > 0) =
  Φ(μ_t/sd_t). Only the values at T are causal, since q̂ and σ̂² use all of y; it is the answer "today" (U7).
- **`local_level_track(y, min_obs=252, refit_every=21)`** is the causal form (the U13 monitor):
  - At each refit session r, q̂ and σ̂² are fitted on y[:r].
  - The sessions from r up to the next refit are filtered with those values.
  - Before min_obs the output is NaN.
  - It passes the perturbation and truncation tests; the whole-sample `local_level` is the caught mutant.
- **q → 0:**
  - Tested by the LR test of q = 0 against the ½χ²₀ + ½χ²₁ mixture (Self & Liang 1987), reported as `q_zero`.
  - P(μ_T > 0) is returned both at q̂ and at q = 0 (`P_mu_T_pos_q0`, the full-sample mean's posterior), because an
    insignificant q̂ > 0 can move the answer by 0.1–0.2 (tested).
  - A report prints both.
