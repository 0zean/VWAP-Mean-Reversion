"""Two-regime Markov-switching models (U9, PLAN.md §6.4 R1 / R2): fits by `statsmodels`, our own Hamilton filter, and
the one-step-ahead regime probability. Never smoothed.

**Models** (`statsmodels` `MarkovRegression`, switching intercept and variance; fitted by its EM-then-BFGS MLE):
- R1: Gaussian HMM on returns standardized by their trailing-252 σ known before the return,
  z_t = r_t / σ̂(r_{t−252 … t−1}); regimes ordered by σ at each refit (regime 1 = high σ).
- R2: MS-AR(1) on d*_t in the **intercept form** y_t = c_s + φ_s·y_{t−1} + ε_t, ε ~ N(0, σ²_s), fitted as a switching
  regression on the lagged value. PLAN §7 U9 names `MarkovAutoregression`, whose mean-adjusted form
  y_t − μ_{s_t} = φ_{s_t}(y_{t−1} − μ_{s_{t−1}}) makes the density depend on the regime pair; the intercept form is
  also an MS-AR(1) (Krolzig 1997's MSIH-AR), keeps the filter on two regimes, and changes nothing a φ-ordered gate
  reads. Regimes ordered by φ (regime 0 = the smaller φ = fast reversion); `swap_flag` when |φ₁ − φ₀| < 0.05 (the
  order is then unreliable).

**Hamilton filter** (Hamilton 1989; `hamilton_filter`): with A[i, j] = P(s_t = j | s_{t−1} = i) and the ergodic
initial distribution (statsmodels' default), predicted_t = filtered_{t−1}·A, filtered_t ∝ predicted_t ⊙ f(y_t | s_t),
and the one-step-ahead probability P(s_{t+1} | ≤ t) = filtered_t·A. Tested equal to statsmodels' filter to 1e-10.

**Causal rolling use** (`rolling_regime`): at refit session r = fit_window, fit_window + refit_every, … the model is
fitted on rows r − fit_window … r − 1 (sessions ≤ r − 1), and rows r … r + refit_every − 1 are filtered with those
parameters (the filter runs from the window's start). Row t's output is the probability of the target regime at
t + 1 given rows ≤ t, from parameters that saw only rows < r ≤ t. Rows before the first refit are NaN ("no gate",
counted by the caller); a refit whose optimizer reports no convergence is kept and counted in `refits`."""

import warnings
from dataclasses import dataclass

import numpy as np
import pandas as pd
from scipy.special import logsumexp

LOG2PI = np.log(2 * np.pi)


@dataclass(frozen=True)
class MSParams:
    const: np.ndarray  # (K,)
    beta: np.ndarray | None  # (K,) coefficient on the exogenous regressor (R2: φ), or None
    sigma2: np.ndarray  # (K,)
    A: np.ndarray  # (K, K), A[i, j] = P(j | i)

    def log_density(self, y: np.ndarray, x: np.ndarray | None = None) -> np.ndarray:
        mean = self.const[None, :] + (
            0 if self.beta is None or x is None else x[:, None] * self.beta[None, :]
        )
        return -0.5 * (
            LOG2PI + np.log(self.sigma2)[None, :] + (y[:, None] - mean) ** 2 / self.sigma2[None, :]
        )

    def ergodic(self) -> np.ndarray:
        K = len(self.const)
        M = np.vstack([np.eye(K) - self.A.T, np.ones(K)])
        return np.linalg.lstsq(M, np.r_[np.zeros(K), 1.0], rcond=None)[0]

    def reorder(self, order: np.ndarray) -> "MSParams":
        return MSParams(self.const[order], None if self.beta is None else self.beta[order], self.sigma2[order],
                        self.A[np.ix_(order, order)])  # fmt: skip


def hamilton_filter(logf: np.ndarray, A: np.ndarray, p0: np.ndarray) -> dict:
    """Filter on log densities logf (T, K): predicted (P(s_t | ≤ t−1)), filtered, one_step (P(s_{t+1} | ≤ t)),
    loglik."""
    T, K = logf.shape
    pred, filt = np.empty((T, K)), np.empty((T, K))
    p, ll = np.asarray(p0, dtype=float), 0.0
    for t in range(T):
        pred[t] = p
        a = np.log(np.maximum(p, 1e-300)) + logf[t]
        c = logsumexp(a)
        ll += c
        filt[t] = np.exp(a - c)
        p = filt[t] @ A
    return {"predicted": pred, "filtered": filt, "one_step": filt @ A, "loglik": float(ll)}


def from_statsmodels(model, params: np.ndarray) -> MSParams:
    names = list(model.param_names)
    P = np.asarray(model.regime_transition_matrix(params))[:, :, 0]  # P[i, j] = P(s_t = i | s_{t−1} = j)
    K = P.shape[0]
    get = lambda stem: np.array([params[names.index(f"{stem}[{k}]")] for k in range(K)])
    beta = get("x1") if "x1[0]" in names else None
    return MSParams(get("const"), beta, get("sigma2"), P.T.copy())


def fit_ms(y: np.ndarray, x: np.ndarray | None = None) -> tuple[MSParams, bool]:
    """Two-regime switching-intercept-and-variance regression (on x when given); (params, converged)."""
    from statsmodels.tsa.regime_switching.markov_regression import MarkovRegression

    model = MarkovRegression(y, k_regimes=2, exog=x, switching_variance=True)
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")  # convergence is reported through the return value, never swallowed
        res = model.fit(disp=False)
    return from_statsmodels(model, np.asarray(res.params)), bool(res.mle_retvals.get("converged", False))


def standardized(r: pd.Series, window: int = 252) -> pd.Series:
    """z_t = r_t / σ̂ of the `window` returns before t (the σ never includes r_t)."""
    return r / r.rolling(window).std(ddof=1).shift(1)


def rolling_regime(y: pd.Series, x: pd.Series | None = None, *, kind: str, fit_window: int = 1008,
                   refit_every: int = 21) -> tuple[pd.DataFrame, list[dict]]:  # fmt: skip
    """One-step-ahead probability of the target regime (R1: high σ; R2: fast reversion = smaller φ) per row, and
    the refit log (session, converged, ordering statistic, swap flag). Module docstring for the timing."""
    if kind not in ("sigma", "phi"):
        raise ValueError("kind is 'sigma' (R1) or 'phi' (R2)")
    if kind == "phi" and x is None:
        raise ValueError("R2 needs the lagged regressor x")
    yv = y.to_numpy(float)
    xv = None if x is None else x.reindex(y.index).to_numpy(float)
    ok = np.isfinite(yv) & (np.isfinite(xv) if xv is not None else True)
    T = len(yv)
    p_next, p_filt = np.full(T, np.nan), np.full(T, np.nan)
    refits = []
    first = int(np.argmax(ok)) + fit_window if ok.any() else T
    for r in range(first, T, refit_every):
        lo, stop = r - fit_window, min(T, r + refit_every)
        if not ok[lo:stop].all():
            refits.append({"session": r, "skipped": "NaN in the window"})
            continue
        params, conv = fit_ms(yv[lo:r], None if xv is None else xv[lo:r])
        stat = params.sigma2 if kind == "sigma" else params.beta
        assert stat is not None
        order = np.argsort(stat)
        params = params.reorder(order)
        target = 1 if kind == "sigma" else 0
        logf = params.log_density(yv[lo:stop], None if xv is None else xv[lo:stop])
        out = hamilton_filter(logf, params.A, params.ergodic())
        p_next[r:stop] = out["one_step"][r - lo :, target]
        p_filt[r:stop] = out["filtered"][r - lo :, target]
        srt = np.sort(stat)
        refits.append({"session": r, "converged": conv, "stat": srt.tolist(),
                       "swap_flag": bool(kind == "phi" and srt[1] - srt[0] < 0.05)})  # fmt: skip
    return pd.DataFrame({"p_next": p_next, "p_filtered": p_filt}, index=y.index), refits
