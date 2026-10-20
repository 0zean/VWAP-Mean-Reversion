"""Sharpe, PSR, MinTRL, Ledoit-Wolf paired difference (U4, ported from wavelet-meta-model
`validation/stats.py`). U3 ports `psr` (needed by backtest/metrics.py); U4 adds the rest and the
Bailey–López de Prado test vector.

Conventions: Sharpe ratios are **per period** (not annualized); `kurt` is the **raw** (non-excess) kurtosis, 3 for a
Normal; sample moments are the population estimators, as in the papers."""

import numpy as np
from scipy.stats import kurtosis, norm, skew


def _sr_std(sr: float, n_obs: int, skew_: float, kurt: float) -> float:
    if n_obs < 2:
        raise ValueError(f"n_obs must be >= 2; got {n_obs}")
    v = 1 - skew_ * sr + (kurt - 1) / 4 * sr**2
    if not v > 0:
        raise ValueError(f"non-positive Sharpe variance term {v:.4g} (sr={sr}, skew={skew_}, kurt={kurt})")
    return float(np.sqrt(v / (n_obs - 1)))


def psr(sr: float, n_obs: int, skew: float = 0.0, kurt: float = 3.0, sr_star: float = 0.0) -> float:
    """PSR(SR*) = Φ((SR̂ − SR*)·√(T−1) / √(1 − γ₃·SR̂ + (γ₄−1)/4·SR̂²)) (Bailey & López de Prado 2012)."""
    return float(norm.cdf((sr - sr_star) / _sr_std(sr, n_obs, skew, kurt)))


def psr_of(returns, sr_star: float = 0.0) -> float:
    """PSR of a return series from its own per-period Sharpe (ddof 1), skewness and raw kurtosis."""
    r = np.asarray(returns, dtype=float)
    sd = r.std(ddof=1)
    if not sd > 0:
        raise ValueError("PSR needs returns with positive variance")
    return psr(r.mean() / sd, len(r), float(skew(r)), float(kurtosis(r, fisher=False)), sr_star)
