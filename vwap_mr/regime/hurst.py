"""DFA-1, MF-DFA and R/S (U8, PLAN.md §8.5). R/S is for the noise study only (the reference everyone quotes).

All estimators take **return increments** r (never the deviation d_t / d*_t, never prices); the profile is integrated
internally, Y = cumsum(r − r̄), so α ≈ H for fractional Gaussian noise (α on the profile would be H + 1).

**DFA-1.** For scale s the window's profile is cut into m = ⌊n/s⌋ segments from the start and m from the end (2m in
all; Kantelhardt et al. 2002), each detrended by its least-squares line; F²(s, ν) is the mean squared residual of
segment ν, F(s) = (mean_ν F²(s, ν))^½, α = the OLS slope of log F(s) on log s over the scale set. The residuals of a
linear detrend do not change when a constant or a linear function of time is added to the profile, so the
window's own mean (r̄) and the profile's level drop out: the rolling version cuts every window from the global
cumulative sum and is vectorized across windows (closed-form segment regressions, no loop over windows).

**MF-DFA.** F_q(s) = (mean_ν F²(s, ν)^{q/2})^{1/q} (q = 0: exp(½ mean log F²)); h(q) = slope of log F_q on log s;
τ(q) = q·h(q) − 1; α(q) = τ′(q) (finite differences on the q grid), f(α) = q·α − τ; Δα = max α − min α. Fat tails
alone widen Δα (U8's noise study measures how much a reader must discount).

**Scale sets** (§6.4): `standard` s ∈ [10, 126], `short` s ∈ [4, 32]; 12 log-spaced integers each (duplicates
dropped). Rolling outputs at session t use r over the `window` sessions ending at t (trailing; NaN until the window
is full or while it holds a NaN)."""

import numpy as np
import pandas as pd
from numpy.lib.stride_tricks import sliding_window_view

SCALE_SETS = {"standard": (10, 126), "short": (4, 32)}
Q_GRID = tuple(range(-5, 6))


def scales(name_or_range, k: int = 12) -> np.ndarray:
    lo, hi = SCALE_SETS[name_or_range] if isinstance(name_or_range, str) else name_or_range
    return np.unique(np.round(np.geomspace(lo, hi, k)).astype(int))


def _segment_f2(Y: np.ndarray, s: int, both_ends: bool) -> np.ndarray:
    """F²(s, ν) for the segments of the profiles Y (..., n): (..., 2m) or (..., m)."""
    n = Y.shape[-1]
    m = n // s
    if m < 1:
        raise ValueError(f"scale {s} exceeds the window {n}")
    parts = [Y[..., : m * s]] + ([Y[..., n - m * s :]] if both_ends else [])
    seg = np.concatenate([p.reshape(*p.shape[:-1], m, s) for p in parts], axis=-2)
    x = np.arange(s, dtype=float)
    xc = x - x.mean()
    yc = seg - seg.mean(axis=-1, keepdims=True)
    beta = (yc @ xc) / (xc @ xc)
    resid = yc - beta[..., None] * xc
    return (resid**2).mean(axis=-1)


def _slope(logx: np.ndarray, logy: np.ndarray) -> np.ndarray:
    """OLS slope of logy (..., k) on logx (k,)."""
    xc = logx - logx.mean()
    return ((logy - logy.mean(axis=-1, keepdims=True)) @ xc) / (xc @ xc)


def _fluct(Y: np.ndarray, sc: np.ndarray, both_ends: bool) -> list[np.ndarray]:
    return [_segment_f2(Y, int(s), both_ends) for s in sc]


def dfa(r, scale_set="standard", *, both_ends: bool = True) -> dict:
    """DFA-1 of one series: α, the scales and F(s)."""
    x = np.asarray(r, dtype=float)
    if np.isnan(x).any():
        raise ValueError("NaN in the returns")
    sc = scales(scale_set)
    Y = np.cumsum(x - x.mean())
    F = np.sqrt(np.array([f.mean() for f in _fluct(Y, sc, both_ends)]))
    return {"alpha": float(_slope(np.log(sc), np.log(F))), "scales": sc, "F": F}


def _hq(f2s: list[np.ndarray], q: np.ndarray) -> np.ndarray:
    """F_q(s) for each q: (..., len(q), len(scales))."""
    out = []
    for f2 in f2s:
        f2 = np.maximum(f2, 1e-300)
        cols = []
        for qq in q:
            cols.append(np.exp(0.5 * np.log(f2).mean(axis=-1)) if qq == 0 else
                        (f2 ** (qq / 2)).mean(axis=-1) ** (1 / qq))  # fmt: skip
        out.append(np.stack(cols, axis=-1))
    return np.stack(out, axis=-1)


def _spectrum(h: np.ndarray, q: np.ndarray) -> dict:
    tau = q * h - 1
    alpha = np.gradient(tau, q, axis=-1)
    return {"h": h, "tau": tau, "alpha": alpha, "f": q * alpha - tau,
            "delta_alpha": alpha.max(axis=-1) - alpha.min(axis=-1)}  # fmt: skip


def mfdfa(r, scale_set="standard", q=Q_GRID, *, both_ends: bool = True) -> dict:
    """MF-DFA of one series: h(q), τ(q), the Legendre spectrum α, f(α), Δα; h(2) equals DFA-1's α."""
    x = np.asarray(r, dtype=float)
    if np.isnan(x).any():
        raise ValueError("NaN in the returns")
    sc, qa = scales(scale_set), np.asarray(q, dtype=float)
    Fq = _hq(_fluct(np.cumsum(x - x.mean()), sc, both_ends), qa)
    h = _slope(np.log(sc), np.log(Fq))
    return {"q": qa, "scales": sc, "Fq": Fq, **_spectrum(h, qa), "h2": float(h[list(qa).index(2.0)])}


def _windows(r: pd.Series, window: int) -> tuple[np.ndarray, np.ndarray]:
    """Profiles of every trailing window (W, window) cut from the global cumulative sum, and a validity mask."""
    x = r.to_numpy(float)
    bad = np.isnan(x)
    C = np.cumsum(np.where(bad, 0.0, x))
    if len(x) < window:
        return np.empty((0, window)), np.zeros(0, bool)
    Y = sliding_window_view(C, window)
    ok = sliding_window_view(bad, window).sum(axis=1) == 0
    return Y - Y[:, :1], ok


def _place(r: pd.Series, window: int, vals: np.ndarray, ok: np.ndarray, name: str) -> pd.Series:
    out = np.full(len(r), np.nan)
    v = np.where(ok, vals, np.nan) if vals.ndim == 1 else vals
    out[window - 1 :] = v
    return pd.Series(out, index=r.index, name=name)


def rolling_dfa(r: pd.Series, window: int = 504, scale_set="standard", *, both_ends: bool = True,
                chunk: int = 512) -> pd.Series:  # fmt: skip
    """DFA-1 α over the trailing `window` returns at each session."""
    Y, ok = _windows(r, window)
    sc = scales(scale_set)
    vals = np.full(len(Y), np.nan)
    for i in range(0, len(Y), chunk):
        F = np.sqrt(np.stack([f.mean(axis=-1) for f in _fluct(Y[i : i + chunk], sc, both_ends)], axis=-1))
        vals[i : i + chunk] = _slope(np.log(sc), np.log(F))
    return _place(r, window, vals, ok, f"dfa_{window}")


def rolling_mfdfa(r: pd.Series, window: int = 1008, scale_set="standard", q=Q_GRID, *, both_ends: bool = True,
                  chunk: int = 256) -> pd.DataFrame:  # fmt: skip
    """MF-DFA h(2) and Δα over the trailing `window` returns at each session."""
    Y, ok = _windows(r, window)
    sc, qa = scales(scale_set), np.asarray(q, dtype=float)
    i2 = list(qa).index(2.0)
    h2, da = np.full(len(Y), np.nan), np.full(len(Y), np.nan)
    for i in range(0, len(Y), chunk):
        h = _slope(np.log(sc), np.log(_hq(_fluct(Y[i : i + chunk], sc, both_ends), qa)))
        h2[i : i + chunk], da[i : i + chunk] = h[:, i2], _spectrum(h, qa)["delta_alpha"]
    return pd.DataFrame({"mfdfa_h2": _place(r, window, h2, ok, "h2"),
                         "mfdfa_dalpha": _place(r, window, da, ok, "da")})  # fmt: skip


def rs_hurst(x, nvals=None) -> float:
    """Classic rescaled-range Hurst exponent with `nolds.hurst_rs`'s conventions (non-overlapping subseries of each
    length n, unbiased σ, R/S averaged over subseries with R > 0, slope of log R/S on log n; no Anis–Lloyd correction).
    Noise study only."""
    x = np.asarray(x, dtype=float)
    N = len(x)
    if nvals is None:
        nvals = np.unique(np.logspace(np.log10(10), np.log10(N // 2), 15).astype(int))
    rs = []
    for n in nvals:
        m = N // n
        seg = x[: m * n].reshape(m, n)
        y = seg - seg.mean(axis=1, keepdims=True)
        z = np.cumsum(y, axis=1)
        R = z.max(axis=1) - z.min(axis=1)
        S = seg.std(axis=1, ddof=1)
        keep = (S > 0) & (R > 0)
        rs.append((R[keep] / S[keep]).mean())
    return float(np.polyfit(np.log(nvals), np.log(rs), 1)[0])


def lo_modified_rs(x, q: int | None = None) -> float:
    """Lo's (1991) modified R/S statistic V = R / (σ̂_q √N), σ̂²_q with Bartlett weights over q lags (q from Andrews'
    rule when None). Under short memory V's limit has a 95 % range of [0.809, 1.862]."""
    x = np.asarray(x, dtype=float)
    N = len(x)
    y = x - x.mean()
    z = np.cumsum(y)
    R = max(z.max(), 0.0) - min(z.min(), 0.0)
    if q is None:
        rho = float(np.corrcoef(y[1:], y[:-1])[0, 1])
        q = int((1.5 * N) ** (1 / 3) * (2 * abs(rho) / (1 - rho**2)) ** (2 / 3))  # Andrews (1991): |ρ̂|
    s2 = y @ y / N
    for j in range(1, q + 1):
        s2 += 2 * (1 - j / (q + 1)) * (y[j:] @ y[:-j]) / N
    return float(R / np.sqrt(s2 * N))
