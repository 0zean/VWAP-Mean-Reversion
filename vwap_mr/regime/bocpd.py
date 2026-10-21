"""Bayesian online changepoint detection (U9, PLAN.md §6.4 R3; Adams & MacKay 2007), in log space.

Observation model: Normal with unknown mean and variance under a Normal–Inverse-Gamma prior (μ₀, κ₀, α₀, β₀); the
predictive of run length r is Student-t with 2α_r degrees of freedom, location μ_r and scale² β_r(κ_r + 1)/(α_r κ_r).
Updates after x: μ' = (κμ + x)/(κ + 1), κ' = κ + 1, α' = α + ½, β' = β + κ(x − μ)²/(2(κ + 1)).

Recursion with constant hazard H = 1/λ: growth P(r_t = r + 1, x₁:t) = P(r_{t−1} = r, x₁:t−1)·π_r(x_t)·(1 − H) and
changepoint P(r_t = 0, x₁:t) = Σ_r P(r_{t−1} = r, x₁:t−1)·π_r(x_t)·H, then normalised. **Note:** with a constant hazard
P(r_t = 0 | x₁:t) equals H exactly (the changepoint mass is H times the evidence), so it carries no information; the
change signal used here is the posterior mass on recent run lengths, `p_recent` = P(r_t ≤ 5 | x₁:t) (a change within the
last five sessions), with E[r_t] and the MAP run length. R3's stability gate (§6.4) reads E[r_t] > 21 and p_recent < 0.2.

Pruning: run lengths whose log posterior falls below the maximum − `prune` (10 by default) are dropped after each step
(`prune=None` keeps every run length: the exact recursion, tested against a direct implementation). `bocpd_avg`
averages the outputs over λ ∈ {63, 126, 252} (equal weights; §6.4). Input: returns standardized by their trailing-252
σ (`hmm.standardized`); the NIG prior is fixed from `diag` 1993–2011 by the U9 script, not centred on reversion. The
output at t uses x₁ … x_t only (online)."""

from dataclasses import dataclass

import numpy as np
from scipy.special import gammaln, logsumexp

RECENT = 5
LAMBDAS = (63, 126, 252)


@dataclass(frozen=True)
class NIG:
    mu0: float = 0.0
    kappa0: float = 1.0
    alpha0: float = 3.0
    beta0: float = 2.0


DEFAULT_PRIOR = NIG()


def _t_logpdf(x: float, mu: np.ndarray, kappa: np.ndarray, alpha: np.ndarray, beta: np.ndarray) -> np.ndarray:
    nu = 2 * alpha
    s2 = beta * (kappa + 1) / (alpha * kappa)
    return (gammaln((nu + 1) / 2) - gammaln(nu / 2) - 0.5 * np.log(nu * np.pi * s2)
            - (nu + 1) / 2 * np.log1p((x - mu) ** 2 / (nu * s2)))  # fmt: skip


def bocpd(
    x, lam: float, prior: NIG = DEFAULT_PRIOR, *, prune: float | None = 10.0, keep_matrix: bool = False
) -> dict:
    """Run-length posterior summaries per observation: p_r0 (= 1/λ by construction), p_recent, e_rl, map_rl; with
    keep_matrix (and prune=None) the full posterior R[t, r] = P(r_t = r | x₁:t)."""
    xs = np.asarray(x, dtype=float)
    if np.isnan(xs).any():
        raise ValueError("NaN in the BOCPD input (drop the warm-up first)")
    T = len(xs)
    logH, log1mH = np.log(1 / lam), np.log1p(-1 / lam)
    rl = np.array([0])  # run lengths currently carried
    logR = np.array([0.0])
    mu, kappa = np.array([prior.mu0]), np.array([prior.kappa0])
    alpha, beta = np.array([prior.alpha0]), np.array([prior.beta0])
    out: dict[str, np.ndarray] = {k: np.empty(T) for k in ("p_r0", "p_recent", "e_rl", "map_rl")}
    R = np.zeros((T, T + 1)) if keep_matrix else None
    for t, xt in enumerate(xs):
        lp = logR + _t_logpdf(xt, mu, kappa, alpha, beta)
        cp = logsumexp(lp) + logH
        logR = np.r_[cp, lp + log1mH]
        logR -= logsumexp(logR)
        rl = np.r_[0, rl + 1]
        beta = np.r_[prior.beta0, beta + kappa * (xt - mu) ** 2 / (2 * (kappa + 1))]
        mu = np.r_[prior.mu0, (kappa * mu + xt) / (kappa + 1)]
        kappa = np.r_[prior.kappa0, kappa + 1]
        alpha = np.r_[prior.alpha0, alpha + 0.5]
        if prune is not None:
            keep = logR >= logR.max() - prune
            rl, logR, mu, kappa, alpha, beta = (
                rl[keep],
                logR[keep],
                mu[keep],
                kappa[keep],
                alpha[keep],
                beta[keep],
            )
            logR -= logsumexp(logR)
        p = np.exp(logR)
        out["p_r0"][t] = p[rl == 0].sum()
        out["p_recent"][t] = p[rl <= RECENT].sum()
        out["e_rl"][t] = float(p @ rl)
        out["map_rl"][t] = float(rl[np.argmax(p)])
        if R is not None:
            R[t, rl] = p
    if R is not None:
        out["R"] = R
    return out


def bocpd_avg(x, prior: NIG = DEFAULT_PRIOR, lams=LAMBDAS, *, prune: float | None = 10.0) -> dict:
    """Equal-weight average of the outputs of one run per hazard (§6.4's "hazard averaged over λ" read as output
    averaging: not a mixture hazard, not evidence-weighted model averaging; in practice close to one run at the
    harmonic-mean λ). `map_rl` is not averaged (an average of modes means nothing)."""
    runs = [bocpd(x, lam, prior, prune=prune) for lam in lams]
    return {k: np.mean([r[k] for r in runs], axis=0) for k in ("p_r0", "p_recent", "e_rl")}


def prior_from(z) -> NIG:
    """The NIG prior fixed from a sample of standardized returns: centred at their mean, prior mean of σ² = their
    variance, weak (κ₀ = 1, α₀ = 3)."""
    z = np.asarray(z, dtype=float)
    z = z[np.isfinite(z)]
    return NIG(mu0=float(z.mean()), kappa0=1.0, alpha0=3.0, beta0=float(z.var(ddof=1)) * 2.0)
