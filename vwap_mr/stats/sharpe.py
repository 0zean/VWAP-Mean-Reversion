"""Sharpe, moments, PSR, MinTRL (U4, SPEC.md §6; ported from 0zean/wavelet-meta-model `validation/stats.py`).
The Ledoit–Wolf paired Sharpe difference is a bootstrap test and lives in `bootstrap.sharpe_test`.

Conventions: Sharpe ratios are **per period** (not annualized); `kurt` is the **raw** (non-excess) kurtosis, 3 for a
Normal; skewness and kurtosis are the population (biased) estimators, the Sharpe's std has ddof 1, as in Bailey &
López de Prado (2012). DSR is not used by this program (PLAN §3.2); its 2014 example is a test vector for `psr`."""

from dataclasses import dataclass

import numpy as np
from scipy.stats import kurtosis, norm, skew


@dataclass(frozen=True)
class ReturnMoments:
    sr: float  # per-period Sharpe ratio (mean / std, ddof 1)
    skew: float
    kurt: float  # raw kurtosis
    n_obs: int


def sharpe_ratio(returns) -> float:
    """Per-period Sharpe ratio mean / std (ddof 1) of excess returns. Raises on NaN or zero variance."""
    r = np.asarray(returns, dtype=float)
    if r.ndim != 1 or r.size < 2:
        raise ValueError("need a 1-D series of at least 2 returns")
    if np.isnan(r).any():
        raise ValueError("returns contain NaN")
    sd = r.std(ddof=1)
    if not sd > 0:
        raise ValueError("returns have zero variance; the Sharpe ratio is undefined")
    return float(r.mean() / sd)


def return_moments(returns) -> ReturnMoments:
    """Sharpe ratio, skewness and raw kurtosis of a return series (the PSR / MinTRL inputs)."""
    r = np.asarray(returns, dtype=float)
    sr = sharpe_ratio(r)
    return ReturnMoments(sr, float(skew(r)), float(kurtosis(r, fisher=False)), r.size)


def _sr_var_term(sr: float, skew_: float, kurt: float) -> float:
    v = 1 - skew_ * sr + (kurt - 1) / 4 * sr**2
    if not v > 0:
        raise ValueError(f"non-positive Sharpe variance term {v:.4g} (sr={sr}, skew={skew_}, kurt={kurt})")
    return v


def _sr_std(sr: float, n_obs: int, skew_: float, kurt: float) -> float:
    if n_obs < 2:
        raise ValueError(f"n_obs must be >= 2; got {n_obs}")
    return float(np.sqrt(_sr_var_term(sr, skew_, kurt) / (n_obs - 1)))


def psr(sr: float, n_obs: int, skew: float = 0.0, kurt: float = 3.0, sr_star: float = 0.0) -> float:
    """PSR(SR*) = Φ((SR̂ − SR*)·√(T−1) / √(1 − γ₃·SR̂ + (γ₄−1)/4·SR̂²)) (Bailey & López de Prado 2012)."""
    return float(norm.cdf((sr - sr_star) / _sr_std(sr, n_obs, skew, kurt)))


def psr_of(returns, sr_star: float = 0.0) -> float:
    """PSR of a return series from its own per-period Sharpe (ddof 1), skewness and raw kurtosis."""
    m = return_moments(returns)
    return psr(m.sr, m.n_obs, m.skew, m.kurt, sr_star)


def min_track_record_length(
    sr: float, sr_star: float = 0.0, skew: float = 0.0, kurt: float = 3.0, alpha: float = 0.05
) -> float:
    """MinTRL = 1 + (1 − γ₃·SR̂ + (γ₄−1)/4·SR̂²)·(Φ⁻¹(1−α) / (SR̂ − SR*))²: the number of returns at which
    PSR(SR*) = 1 − α (continuous; PSR uses T − 1, so this is its exact inverse). Infinite when SR̂ ≤ SR*."""
    if not 0 < alpha < 1:
        raise ValueError(f"alpha must be in (0, 1); got {alpha}")
    if sr <= sr_star:
        return float("inf")
    return float(1 + _sr_var_term(sr, skew, kurt) * (norm.ppf(1 - alpha) / (sr - sr_star)) ** 2)
