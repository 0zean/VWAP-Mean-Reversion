"""Causal MODWT and the Abry–Veitch estimator (U8, PLAN.md §8.5). The no-wrap pyramid is ported from 0zean/
wavelet-meta-model `features/causal_modwt.py` (there for the scaling coefficients; here for the details).

**Causal MODWT** (Percival & Walden 2000, p. 177, without the circular wrap): with the MODWT filters g̃ = g/√2,
h̃ = h/√2 (pywt `rec_lo` / `rec_hi`), V₀ = X and for j = 1 … J
    W_j[t] = Σ_l h̃_l · V_{j−1}[t − l·2^{j−1}],   V_j[t] = Σ_l g̃_l · V_{j−1}[t − l·2^{j−1}].
Every coefficient is a combination of X at t and earlier; the first (2^j − 1)(L − 1) values of level j are NaN
(exactly the boundary coefficients a circular MODWT would wrap). The standard DWT / symmetric-boundary transforms read
values after t near a window's end and are not used (a mutant test plants one).

**Abry–Veitch** (Abry & Veitch 1998; Veitch & Abry 1999) on increments: μ_j = 2^j · mean W_j² over a window (the
MODWT coefficient is the orthonormal DWT's divided by 2^{j/2}; white noise then has a flat μ_j), y_j = log₂ μ_j − g_j with the bias correction g_j = ψ(n_j/2)/ln 2 − log₂(n_j/2), weighted least squares of y_j on j over
j₁ … j₂ with weights 1/ζ(2, n_j/2)·ln²2 (the variance of log₂ of a χ²_{n_j}/n_j), n_j = the window's valid
coefficients / 2^j (the effective number of independent coefficients at level j). For fGn the slope is 2H − 1, so
H = (slope + 1)/2. Level sets (§6.4): `short` j = 1–4, `standard` j = 2–6; `sym4` (Daubechies least-asymmetric 8,
L = 8) by default."""

import numpy as np
import pandas as pd
import pywt
from scipy.special import digamma, polygamma

LEVEL_SETS = {"short": (1, 4), "standard": (2, 6)}
WAVELET = "sym4"


def modwt_filters(wavelet: str = WAVELET) -> tuple[np.ndarray, np.ndarray]:
    w = pywt.Wavelet(wavelet)  # ty: ignore[unresolved-attribute]
    return np.asarray(w.rec_lo) / np.sqrt(2), np.asarray(w.rec_hi) / np.sqrt(2)


def _lagged_sum(v: np.ndarray, f: np.ndarray, step: int) -> np.ndarray:
    """Σ_l f_l · v[t − l·step], NaN where any term is before the start."""
    out = np.zeros_like(v)
    for l, c in enumerate(f):
        k = l * step
        out[k:] += c * v[: len(v) - k] if k else c * v
        out[:k] = np.nan
    return out


def causal_modwt(x, levels: int, wavelet: str = WAVELET) -> np.ndarray:
    """Detail coefficients W_1 … W_levels of the causal MODWT, shape (levels, len(x))."""
    g, h = modwt_filters(wavelet)
    v = np.asarray(x, dtype=float).copy()
    out = np.empty((levels, len(v)))
    for j in range(levels):
        step = 2**j
        out[j] = _lagged_sum(v, h, step)
        v = _lagged_sum(v, g, step)
    return out


def boundary_len(j: int, wavelet: str = WAVELET) -> int:
    return (2**j - 1) * (pywt.Wavelet(wavelet).dec_len - 1)  # ty: ignore[unresolved-attribute]


def _av(mu: np.ndarray, n_eff: np.ndarray, js: np.ndarray) -> np.ndarray:
    """H from wavelet variances mu (..., k) at levels js with effective counts n_eff (k,)."""
    half = n_eff / 2
    y = np.log2(mu * 2.0**js) - (digamma(half) / np.log(2) - np.log2(half))  # MODWT → DWT scale: × 2^j
    w = 1 / (polygamma(1, half) / np.log(2) ** 2)
    jw = (w * js).sum() / w.sum()
    slope = ((y - (y * w).sum(axis=-1, keepdims=True) / w.sum()) * w * (js - jw)).sum(axis=-1) / (
        w * (js - jw) ** 2
    ).sum()
    return (slope + 1) / 2


def abry_veitch(r, level_set="short", wavelet: str = WAVELET) -> float:
    """H of one increment series from all its valid causal-MODWT coefficients."""
    j1, j2 = LEVEL_SETS[level_set] if isinstance(level_set, str) else level_set
    x = np.asarray(r, dtype=float)
    W = causal_modwt(x - x.mean(), j2, wavelet)
    js = np.arange(j1, j2 + 1)
    mu = np.array([np.nanmean(W[j - 1] ** 2) for j in js])
    n_eff = np.array([(len(x) - boundary_len(j, wavelet)) / 2**j for j in js])
    if (n_eff < 2).any():
        raise ValueError(f"series too short for level {j2}")
    return float(_av(mu, n_eff, js))


def rolling_av(r: pd.Series, window: int = 512, level_set="short", wavelet: str = WAVELET) -> pd.Series:
    """Abry–Veitch H from the trailing `window` returns: at level j the wavelet variance is the mean of W_j² over the
    window − (2^j − 1)(L − 1) coefficients whose whole filter support lies inside those returns, so the estimate at t
    reaches back exactly `window` returns (as the DFA windows do) and equals `abry_veitch` on that window (tested).
    NaN until the window is full or while a NaN return is in reach."""
    j1, j2 = LEVEL_SETS[level_set] if isinstance(level_set, str) else level_set
    x = r.to_numpy(float)
    W = causal_modwt(x, j2, wavelet)  # a NaN return propagates into every coefficient that reaches it
    js = np.arange(j1, j2 + 1)
    counts = np.array([window - boundary_len(j, wavelet) for j in js])
    if (counts / 2.0**js < 2).any():
        raise ValueError(f"window {window} too short for level {j2}")
    mu = np.stack([pd.Series(W[j - 1] ** 2).rolling(int(c), min_periods=int(c)).mean().to_numpy()
                   for j, c in zip(js, counts, strict=True)], axis=-1)  # fmt: skip
    n_eff = counts / 2.0**js
    out = np.full(len(x), np.nan)
    ok = np.isfinite(mu).all(axis=1) & (mu > 0).all(axis=1)
    out[ok] = _av(mu[ok], n_eff, js)
    return pd.Series(out, index=r.index, name=f"av_{level_set}_{window}")
