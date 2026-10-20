"""Studentized circular block bootstrap and Bartlett HAC (U4, SPEC.md §6; ported from 0zean/wavelet-meta-model
`families/stats.py`: `circular_block_indices`, `hac_bartlett`, `mean_test`, `sharpe_test`, `newey_west_alpha`).

Three tests share one construction (Politis & Romano 1992; Ledoit & Wolf 2008 §3.2):
- `mean_test`: H0 E[r] = 0 (H1 headline, PLAN §6.2);
- `cond_mean_test`: H0 Δμ = E[r | g] − E[r | ¬g] = 0 for a gate g (H3, §6.4);
- `sharpe_test`: H0 SR_a = SR_b (paired) or SR_a = 0, Ledoit–Wolf (H4, §6.5).
Each statistic θ̂ is a smooth function of sample means; its standard error is the delta-method Bartlett HAC with
bandwidth b = the block length (weights 1 − j/b, j < b: Newey–West with b − 1 lags). On each resample of
l = ⌊T/b⌋ circular blocks the statistic θ* and its block-sum standard error s* (ζ_j = b^−½ Σ_{t∈j} ψ*_t,
s*² = mean ζ_j² / (l·b)) give the studentized root (θ* − θ̂)/s*. One-sided p = (1 + #{root ≥ θ̂/se})/(M' + 1)
against H1: θ > 0; two-sided on |root|. M' counts the finite roots: a resample whose statistic is undefined (a gate
never on, a flat stream) is dropped and counted in `n_invalid`, never silently. Every draw takes an explicit seed.

Conventions: daily returns; per-period statistics, annualized with 252 only in fields named `_ann`; NaN in an input
raises (align streams first)."""

import numpy as np
from scipy.stats import norm

TRADING_DAYS = 252
_CHUNK = 250  # resamples per vectorized chunk (memory: chunk · T · k floats)


def circular_block_indices(n: int, block: int, n_boot: int, rng: np.random.Generator) -> np.ndarray:
    """(n_boot, l·block) indices of circular block-bootstrap samples, l = ⌊n / block⌋ blocks of `block` sessions."""
    if not 1 <= block <= n // 2:
        raise ValueError(f"block must be in [1, n/2]; got block={block}, n={n}")
    n_blocks = n // block
    starts = rng.integers(0, n, size=(n_boot, n_blocks))
    return ((starts[..., None] + np.arange(block)) % n).reshape(n_boot, n_blocks * block)


def hac_bartlett(Y: np.ndarray, bandwidth: float) -> np.ndarray:
    """Bartlett-kernel long-run covariance of the rows of Y (T, k), demeaned: Γ0 + Σ_{j<b} (1 − j/b)(Γj + Γjᵀ),
    Γj = Σ_t z_t z_{t−j}ᵀ / T. b may be fractional (Andrews 1991's S_T)."""
    Y = np.asarray(Y, dtype=float)
    Z = (Y - Y.mean(axis=0)).reshape(len(Y), -1)
    T = len(Z)
    if not 1 <= bandwidth < T:
        raise ValueError(f"bandwidth must be in [1, T); got {bandwidth}, T={T}")
    S = Z.T @ Z / T
    for j in range(1, int(np.ceil(bandwidth))):
        G = Z[j:].T @ Z[:-j] / T
        S += (1 - j / bandwidth) * (G + G.T)
    return S


def andrews_bandwidth(e) -> float:
    """Andrews (1991) Bartlett bandwidth with the AR(1) plug-in: S_T = 1.1447·(α̂·T)^{1/3},
    α̂ = 4ρ̂² / ((1 − ρ̂)²(1 + ρ̂)²), ρ̂ the lag-1 OLS coefficient of the demeaned series (clipped to ±0.97).
    Returned as a bandwidth for `hac_bartlett` (weights 1 − j/S_T, j < S_T; at least 1, i.e. iid)."""
    z = np.asarray(e, dtype=float)
    z = z - z.mean()
    den = float(z[:-1] @ z[:-1])
    if not den > 0:
        raise ValueError("constant series: no bandwidth")
    rho = float(np.clip(z[1:] @ z[:-1] / den, -0.97, 0.97))
    a = 4 * rho**2 / ((1 - rho) ** 2 * (1 + rho) ** 2)
    return float(max(1.0, min(1.1447 * (a * len(z)) ** (1 / 3), len(z) - 1)))


def newey_west_ols(y, X, lags: int) -> dict:
    """OLS y = Xβ + ε (X without a constant column: one is prepended) with Newey–West (Bartlett, `lags`) standard
    errors. Returns β, se, t (two-sided normal p) — index 0 is the intercept."""
    y = np.asarray(y, dtype=float)
    X = np.column_stack([np.ones_like(y), np.asarray(X, dtype=float).reshape(len(y), -1)])
    if np.isnan(y).any() or np.isnan(X).any():
        raise ValueError("NaN in the regression: align the series first")
    XtX_inv = np.linalg.inv(X.T @ X)
    beta = XtX_inv @ X.T @ y
    Xe = X * (y - X @ beta)[:, None]
    S = Xe.T @ Xe
    for j in range(1, lags + 1):
        G = Xe[j:].T @ Xe[:-j]
        S += (1 - j / (lags + 1)) * (G + G.T)
    se = np.sqrt(np.diag(XtX_inv @ S @ XtX_inv))
    t = beta / se
    return {"beta": beta, "se": se, "t": t, "p": 2 * norm.sf(np.abs(t)), "lags": lags}


def _check(x: np.ndarray, block: int, sided: str) -> None:
    if sided not in ("one", "two"):
        raise ValueError(f"sided must be 'one' or 'two', got {sided!r}")
    if x.ndim != 1:
        raise ValueError("need a 1-D stream")
    if np.isnan(x).any():
        raise ValueError("returns contain NaN: align the stream first")
    if len(x) < 2 * block:
        raise ValueError(f"need at least 2 blocks of {block} returns; got {len(x)}")


def _boot_pvalue(stat: float, se: float, roots: np.ndarray, alpha: float, sided: str) -> dict:
    """p-value and 1 − alpha interval of `stat` from the studentized roots (θ* − θ̂)/s*: two-sided on |·|;
    one-sided against H1: θ > 0, the interval then being the lower confidence bound and +inf."""
    t_obs = stat / se
    if sided == "two":
        ab = np.abs(roots)
        q = float(np.quantile(ab, 1 - alpha))
        return {
            "p": float((1 + np.sum(ab >= abs(t_obs))) / (len(ab) + 1)),
            "ci": (stat - q * se, stat + q * se),
        }
    q = float(np.quantile(roots, 1 - alpha))  # P(θ̂ − θ ≤ q·se) = 1 − α → θ ≥ θ̂ − q·se
    return {"p": float((1 + np.sum(roots >= t_obs)) / (len(roots) + 1)), "ci": (stat - q * se, float("inf"))}


def _block_se(psi: np.ndarray, block: int) -> np.ndarray:
    """Block-sum standard error of a mean of the influence values psi (m, L) on each resample (L = l·block)."""
    m, L = psi.shape
    zeta = psi.reshape(m, L // block, block).sum(axis=2) / np.sqrt(block)
    return np.sqrt((zeta**2).mean(axis=1) / L)


def _finish(stat, se, roots, alpha, sided, extra: dict, T, block, n_boot) -> dict:
    ok = np.isfinite(roots)
    out = _boot_pvalue(stat, se, roots[ok], alpha, sided)
    return extra | {"se": se, "t": stat / se, "p": out["p"], "ci": out["ci"], "n_obs": T, "block": block,
                    "n_boot": n_boot, "n_invalid": int((~ok).sum()), "sided": sided}  # fmt: skip


def mean_test(
    r, *, block: int = 21, n_boot: int = 5000, alpha: float = 0.05, seed: int = 0, sided: str = "one",
    nw_lags: int = 5,
) -> dict:  # fmt: skip
    """Studentized circular block-bootstrap test of H0: E[r] = 0 (module docstring), with the Newey–West
    (`nw_lags`) t alongside. Returns the mean per day, in bp per day and annualized (× 252), se, t, p, the 1 − alpha
    interval (per day), t_nw."""
    x = np.asarray(r, dtype=float)
    _check(x, block, sided)
    if not x.std() > 0:
        raise ValueError("the stream has zero variance: nothing to test")
    T = len(x)
    m_hat = float(x.mean())
    se = float(np.sqrt(hac_bartlett(x[:, None], block)[0, 0] / T))
    rng = np.random.default_rng(seed)
    roots = []
    for start in range(0, n_boot, _CHUNK):
        xs = x[circular_block_indices(T, block, min(_CHUNK, n_boot - start), rng)]
        means = xs.mean(axis=1)
        roots.append((means - m_hat) / _block_se(xs - means[:, None], block))
    nw = newey_west_ols(x, np.empty((T, 0)), nw_lags)
    extra = {"mean": m_hat, "mean_bp_day": m_hat * 1e4, "mean_ann": m_hat * TRADING_DAYS,
             "t_nw": float(nw["t"][0]), "nw_lags": nw_lags}  # fmt: skip
    return _finish(m_hat, se, np.concatenate(roots), alpha, sided, extra, T, block, n_boot)


def cond_mean_test(
    r, gate, *, block: int = 21, n_boot: int = 5000, alpha: float = 0.05, seed: int = 0, sided: str = "one",
    nw_lags: int = 5,
) -> dict:  # fmt: skip
    """Studentized circular block-bootstrap test of H0: Δμ = mean(r | gate) − mean(r | ¬gate) = 0 (PLAN §6.4).
    The session index is resampled, so r and the gate move together and both conditional means are recomputed
    per resample. Influence values ψ_t = (g_t − π̂)·e_t / (π̂(1 − π̂)), e_t = r_t − μ̂_{g_t}: the HAC se is that of the
    OLS slope of r on 1[gate]; the Newey–West (`nw_lags`) t of that regression is reported alongside.
    Returns delta, mu_in, mu_out, pi, the gate's number of episodes (runs of consecutive on-sessions) and their
    median length, se, t, p, ci, t_nw, n_invalid (resamples where the gate is never on or always on)."""
    x = np.asarray(r, dtype=float)
    _check(x, block, sided)
    g = np.asarray(gate)
    if g.shape != x.shape or g.dtype != bool:
        raise ValueError(
            "gate must be a boolean array aligned with r (no NaN: decide pre-history explicitly)"
        )
    T = len(x)
    n_in = int(g.sum())
    if n_in < 2 or T - n_in < 2:
        raise ValueError(f"gate must be on and off on at least 2 sessions each; on {n_in} of {T}")
    gf = g.astype(float)

    def stat_psi(xs, gs):
        n1 = gs.sum(axis=-1)
        pi = n1 / gs.shape[-1]
        with np.errstate(invalid="ignore", divide="ignore"):
            mu1 = (gs * xs).sum(axis=-1) / n1
            mu0 = ((1 - gs) * xs).sum(axis=-1) / (gs.shape[-1] - n1)
            e = xs - np.where(gs > 0, mu1[..., None], mu0[..., None])
            psi = (gs - pi[..., None]) * e / (pi * (1 - pi))[..., None]
        return mu1 - mu0, mu1, mu0, pi, psi

    d_hat, mu1, mu0, pi, psi = stat_psi(x, gf)
    se = float(np.sqrt(hac_bartlett(psi[:, None], block)[0, 0] / T))
    if not se > 0:
        raise ValueError("zero standard error: r is constant within both gate states")
    rng = np.random.default_rng(seed)
    roots = []
    for start in range(0, n_boot, _CHUNK):
        idx = circular_block_indices(T, block, min(_CHUNK, n_boot - start), rng)
        d, *_, ps = stat_psi(x[idx], gf[idx])
        with np.errstate(invalid="ignore", divide="ignore"):
            roots.append((d - d_hat) / _block_se(ps, block))
    edges = np.diff(np.r_[0, g.astype(np.int8), 0])
    runs = np.flatnonzero(edges == -1) - np.flatnonzero(edges == 1)
    nw = newey_west_ols(x, gf, nw_lags)
    extra = {"delta": float(d_hat), "delta_bp_day": float(d_hat * 1e4), "mu_in": float(mu1), "mu_out": float(mu0),
             "pi": float(pi), "episodes": len(runs), "episode_median": float(np.median(runs)),
             "t_nw": float(nw["t"][1]), "nw_lags": nw_lags}  # fmt: skip
    return _finish(float(d_hat), se, np.concatenate(roots), alpha, sided, extra, T, block, n_boot)


def _sharpe_f_grad(u: np.ndarray, two: bool) -> tuple[np.ndarray, np.ndarray]:
    """Δ and ∇Δ at moments u (…, k): k = 4 (a, b, a², b²) for a difference, k = 2 (a, a²) for one Sharpe
    (Ledoit & Wolf 2008 eq. 2–3; population std)."""
    if two:
        ma, mb, ga, gb = (u[..., i] for i in range(4))
        va, vb = ga - ma**2, gb - mb**2
        d = ma / np.sqrt(va) - mb / np.sqrt(vb)
        g = np.stack([ga / va**1.5, -gb / vb**1.5, -ma / (2 * va**1.5), mb / (2 * vb**1.5)], axis=-1)
    else:
        ma, ga = u[..., 0], u[..., 1]
        va = ga - ma**2
        d = ma / np.sqrt(va)
        g = np.stack([ga / va**1.5, -ma / (2 * va**1.5)], axis=-1)
    return d, g


def sharpe_test(
    a, b=None, *, block: int = 21, n_boot: int = 5000, alpha: float = 0.05, seed: int = 0, sided: str = "one"
) -> dict:
    """Ledoit–Wolf (2008) studentized circular block-bootstrap test of H0: SR_a = SR_b on paired daily returns, or
    of H0: SR_a = 0 when b is None. Δ = f(u), u the means of y_t = (a, b, a², b²); se = √(∇fᵀ Ψ̂ ∇f / T) with Ψ̂ the
    Bartlett HAC of y; Ψ* the block-sum estimator on each resample (their §3.2). Returns delta (per period) and
    delta_ann (× √252), se (per period), t, p, ci (per period), n_invalid."""
    x = np.asarray(a, dtype=float)
    _check(x, block, sided)
    two = b is not None
    if two:
        z = np.asarray(b, dtype=float)
        if z.shape != x.shape:
            raise ValueError(f"paired streams must have the same length; got {x.shape} and {z.shape}")
        if np.isnan(z).any():
            raise ValueError("returns contain NaN: align the streams first")
        if not z.std() > 0:
            raise ValueError("stream b has zero variance: its Sharpe ratio is undefined")
    if not x.std() > 0:
        raise ValueError("stream a has zero variance: its Sharpe ratio is undefined")
    Y = np.column_stack([x, z, x**2, z**2]) if two else np.column_stack([x, x**2])
    T = len(Y)
    d_hat, g = _sharpe_f_grad(Y.mean(axis=0), two)
    se = float(np.sqrt(g @ hac_bartlett(Y, block) @ g / T))
    rng = np.random.default_rng(seed)
    roots = []
    for start in range(0, n_boot, _CHUNK):
        m = min(_CHUNK, n_boot - start)
        Ys = Y[circular_block_indices(T, block, m, rng)]  # (m, L, k)
        L = Ys.shape[1]
        u = Ys.mean(axis=1)
        with np.errstate(invalid="ignore", divide="ignore"):
            d, gs = _sharpe_f_grad(u, two)
            zeta = (Ys - u[:, None, :]).reshape(m, L // block, block, Y.shape[1]).sum(axis=2) / np.sqrt(block)
            psi = np.einsum("mjk,mjl->mkl", zeta, zeta) / zeta.shape[1]
            roots.append((d - d_hat) / np.sqrt(np.einsum("mk,mkl,ml->m", gs, psi, gs) / L))
    extra = {"delta": float(d_hat), "delta_ann": float(d_hat * np.sqrt(TRADING_DAYS))}
    return _finish(float(d_hat), se, np.concatenate(roots), alpha, sided, extra, T, block, n_boot)
