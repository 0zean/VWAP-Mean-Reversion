"""Bai–Perron breaks in the mean (HAC) and the local-level Kalman edge tracker with its MLE profile (U4, SPEC.md §6;
PLAN §6.3). Own implementations; `statsmodels` is used only in tests, as a check on the Kalman likelihood.

**Bai–Perron (1998, 2003), mean-shift model** y_t = μ_i + u_t on m + 1 segments of ≥ h = ⌈εT⌉ sessions. For each
m ≤ m_max the global SSR minimiser is found by dynamic programming. supF(m) = (SSR₀ − SSR_m)/(m·ω̂²₀), with ω̂²₀ the
Bartlett HAC long-run variance of the **null (0-break) residuals** (bandwidth: AR(1)-prewhitened Andrews 1991 by
default, or fixed). Normalising by the null model is the LM form: under an alternative ω̂²₀ is inflated (≈ b·δ²/4,
a few % of F for a 10 bp shift at σ 1 %), costing a little power; the alternative's residuals made the test
oversized (U4 status note: size 0.059–0.072 vs 0.045–0.064). Its null limit is the same functional of a Brownian motion as (SSR₀ − SSR_m)/m on
iid N(0, 1) data of known variance, which `scripts/make_bp_null.py` simulates by the same DP (N = 2,000 points,
10,000 draws, seed 0) into `bp_null.csv` (quantiles of supF(1…5) and UDmax = max_m supF(m)). The sequential test
supF(l+1 | l) is the largest single-break F inside the l-break segments, normalised by ω̂² of the l-break fit (its
null; so its first step is supF(1)); its p-value is 1 − G(x)^{l+1}, G the supF(1) null (Bai & Perron 1998 Prop. 9, with
every segment's trimming taken as ε). The number of breaks is the sequential choice at level α. A break date's interval is
k̂ ± c·ω̂²/δ̂², δ̂ the mean shift across it and c the 1 − α/2 quantile of argmax_s W(s) − |s|/2, whose CDF is closed
form (Bai 1997; c = 11.03 at 95 %). That interval is asymptotic in a shrinking shift; its coverage is below
nominal for shifts small relative to σ (the reviewer measured 0.85 at 20 bp/day, σ 1 %, T 1,500).

**Local level** (Durbin & Koopman 2012 §2): y_t = μ_t + ε_t, ε ~ N(0, σ²_ε); μ_{t+1} = μ_t + η_t, η ~ N(0, q·σ²_ε).
Diffuse initialisation (μ₁ = y₁ after the first update); σ²_ε concentrated out; the profile log-likelihood over q
on a log grid plus q = 0 is returned, its maximiser refined by a bounded search. **q → 0** (the edge is a
constant; the filter is the recursive sample mean) is detected by the likelihood-ratio test of q = 0, whose null is
the ½χ²₀ + ½χ²₁ mixture (Self & Liang 1987), and reported as `q_zero`, with P(μ_T > 0) at q̂ and at q = 0.
`local_level` fits on all of y (the answer at T); `local_level_track` refits on the past only (the causal track)."""

from itertools import pairwise
from pathlib import Path

import numpy as np
from scipy.optimize import minimize_scalar
from scipy.stats import chi2, norm

from vwap_mr.stats.bootstrap import andrews_bandwidth, hac_bartlett

BP_NULL_PATH = Path(__file__).with_name("bp_null.csv")
BP_EPS = 0.15
BP_MMAX = 5


# ── Bai–Perron ────────────────────────────────────────────────────────────────────────────────────────────────────


def _cums(y: np.ndarray) -> tuple[np.ndarray, np.ndarray]:
    return np.r_[0.0, np.cumsum(y)], np.r_[0.0, np.cumsum(y * y)]


def _ssr(s1, s2, i, j):
    """SSR of segment [i, j) about its mean (vectorised over i or j)."""
    n = j - i
    return s2[j] - s2[i] - (s1[j] - s1[i]) ** 2 / n


def _dp(Y: np.ndarray, m_max: int, h: int) -> tuple[np.ndarray, list[np.ndarray]]:
    """Exact DP for a batch Y (R, T): SSR_m (R, m_max + 1) of the global minimiser with m breaks and segments of
    ≥ h points, and the back-pointers (one (R, T + 1) array per m) for `segment_dp`. O(m_max·T²) per row."""
    R, T = Y.shape
    if h < 2 or (m_max + 1) * h > T:
        raise ValueError(f"cannot fit {m_max} breaks with segments >= {h} in {T} points")
    s1 = np.concatenate([np.zeros((R, 1)), np.cumsum(Y, axis=1)], axis=1)
    s2 = np.concatenate([np.zeros((R, 1)), np.cumsum(Y * Y, axis=1)], axis=1)

    def ssr(i, j):  # segments [i, j) for index arrays broadcast against the batch
        i, j = np.atleast_1d(i), np.atleast_1d(j)
        return s2[:, j] - s2[:, i] - (s1[:, j] - s1[:, i]) ** 2 / (j - i)

    cost = np.full((R, T + 1), np.inf)
    cost[:, h:] = ssr(0, np.arange(h, T + 1))
    out = np.empty((R, m_max + 1))
    out[:, 0] = cost[:, T]
    backs = []
    rows = np.arange(R)
    for m in range(1, m_max + 1):
        new = np.full((R, T + 1), np.inf)
        arg = np.zeros((R, T + 1), dtype=np.int64)
        for j in range((m + 1) * h, T + 1):
            i = np.arange(m * h, j - h + 1)
            c = cost[:, i] + ssr(i, j)
            k = np.argmin(c, axis=1)
            new[:, j], arg[:, j] = c[rows, k], i[k]
        backs.append(arg)
        cost = new
        out[:, m] = cost[:, T]
    return out, backs


def segment_dp(y, m_max: int, h: int) -> list[tuple[float, list[int]]]:
    """Global SSR minimisers of one series for m = 0 … m_max breaks with segments of ≥ h points:
    [(SSR_m, break ends)], a break end k meaning the new regime starts at index k."""
    y = np.asarray(y, dtype=float)
    ssr, backs = _dp(y[None, :], m_max, h)
    out = [(float(ssr[0, 0]), [])]
    for m in range(1, m_max + 1):
        ends, j = [], len(y)
        for mm in range(m, 0, -1):
            j = int(backs[mm - 1][0, j])
            ends.append(j)
        out.append((float(ssr[0, m]), sorted(ends)))
    return out


def sup_f1(Y: np.ndarray, h: int) -> np.ndarray:
    """supF(1) of each row of Y (known variance 1; the O(T) check on the DP null table): max_k k(T−k)/T·(ȳ₁ − ȳ₂)²."""
    T = Y.shape[1]
    s = np.cumsum(Y, axis=1)
    k = np.arange(h, T - h + 1)
    tot = s[:, -1:]
    m1 = s[:, k - 1] / k
    m2 = (tot - s[:, k - 1]) / (T - k)
    return (k * (T - k) / T * (m1 - m2) ** 2).max(axis=1)


def _segments(T: int, ends: list[int]) -> list[tuple[int, int]]:
    b = [0, *ends, T]
    return list(pairwise(b))


def _resid(y: np.ndarray, ends: list[int]) -> np.ndarray:
    e = y.copy()
    for i, j in _segments(len(y), ends):
        e[i:j] -= y[i:j].mean()
    return e


def bai_perron_dates_cdf(x) -> np.ndarray:
    """CDF of V = argmax_s W(s) − |s|/2 (two-sided Brownian motion; Bai 1997): for x ≥ 0
    P(V ≤ x) = 1 + √(x/2π)·e^{−x/8} − (x + 5)/2·Φ(−√x/2) + 3/2·e^x·Φ(−3√x/2); symmetric about 0."""
    x = np.asarray(x, dtype=float)
    a = np.abs(x)
    with np.errstate(over="ignore", invalid="ignore"):
        g = (1 + np.sqrt(a / (2 * np.pi)) * np.exp(-a / 8) - (a + 5) / 2 * norm.cdf(-np.sqrt(a) / 2)
             + 1.5 * np.exp(a) * norm.cdf(-1.5 * np.sqrt(a)))  # fmt: skip
    g = np.where(a > 200, 1.0, g)  # e^x·Φ(−1.5√x) underflows to nan·0 far in the tail
    return np.where(x >= 0, g, 1 - g)


def _date_quantile(level: float) -> float:
    lo, hi = 0.0, 200.0
    for _ in range(100):
        mid = (lo + hi) / 2
        lo, hi = (mid, hi) if bai_perron_dates_cdf(mid) < level else (lo, mid)
    return (lo + hi) / 2


def bp_null(path: Path = BP_NULL_PATH) -> dict[str, np.ndarray]:
    """The simulated null quantile table: {'prob': (Q,), 'supF1'…'supF5', 'UDmax': (Q,)}."""
    if not path.exists():
        raise FileNotFoundError(f"{path} missing: run scripts/make_bp_null.py")
    rows = np.loadtxt(path, delimiter=",", skiprows=1)
    with open(path, encoding="utf-8") as fh:
        cols = fh.readline().strip().split(",")
    return {c: rows[:, i] for i, c in enumerate(cols)}


def _cdf_from_table(tab: dict, col: str, x: float) -> float:
    """Null CDF at x by linear interpolation of the quantile table; clipped to its first / last probability."""
    return float(np.interp(x, tab[col], tab["prob"], left=tab["prob"][0], right=tab["prob"][-1]))


def _omega2(e: np.ndarray, bandwidth) -> tuple[float, float]:
    """(ω̂², bandwidth used) of residuals e: Bartlett HAC at a fixed `bandwidth`, or "andrews_pw": AR(1)
    prewhitening (ρ̂ clipped to ±0.97), Andrews' plug-in bandwidth on the whitened series, recoloured by
    1/(1 − ρ̂)² (Andrews & Monahan 1992)."""
    if bandwidth != "andrews_pw":
        return float(hac_bartlett(e[:, None], float(bandwidth))[0, 0]), float(bandwidth)
    z = e - e.mean()
    rho = float(np.clip(z[1:] @ z[:-1] / (z[:-1] @ z[:-1]), -0.97, 0.97))
    u = z[1:] - rho * z[:-1]
    b = andrews_bandwidth(u)
    return float(hac_bartlett(u[:, None], b)[0, 0] / (1 - rho) ** 2), b


def bai_perron(
    y, *, m_max: int = BP_MMAX, eps: float = BP_EPS, bandwidth: float | str = "andrews_pw", alpha: float = 0.05,
    null_path: Path = BP_NULL_PATH,
) -> dict:  # fmt: skip
    """Bai–Perron breaks in the mean of y (module docstring). Returns n_breaks (sequential at alpha), the chosen
    break dates (index of the first session of each new regime), their 1 − alpha intervals, segment means,
    supF(m) and its p for m = 1 … m_max, UDmax and its p, the sequential supF(l+1 | l) with p, ω̂² and the
    bandwidth of the chosen model (used for the date intervals), h. Every F uses ω̂² of the residuals of its null
    model (so the sequential step l = 0 is supF(1) exactly). p-values are floored at the table's resolution (`p_floor`)."""
    y = np.asarray(y, dtype=float)
    if y.ndim != 1 or np.isnan(y).any():
        raise ValueError("need a 1-D series without NaN")
    if not y.std() > 0:
        raise ValueError("constant series: no break test (a stream that never trades has nothing to test)")
    if m_max > BP_MMAX or eps != BP_EPS:
        raise ValueError(f"the null table covers m_max <= {BP_MMAX} at eps = {BP_EPS} only")
    T = len(y)
    h = int(np.ceil(eps * T))
    fits = segment_dp(y, m_max, h)
    tab = bp_null(null_path)
    p_floor = float(1 - tab["prob"][-1])
    ssr0 = fits[0][0]
    w2s = [_omega2(_resid(y, ends), bandwidth)[0] for _, ends in fits]
    supf = [float((ssr0 - fits[m][0]) / (m * w2s[0])) for m in range(1, m_max + 1)]
    supf_p = [max(p_floor, 1 - _cdf_from_table(tab, f"supF{m}", f)) for m, f in enumerate(supf, start=1)]
    udmax = max(supf)
    udmax_p = max(p_floor, 1 - _cdf_from_table(tab, "UDmax", udmax))
    s1, s2 = _cums(y)
    seq = []
    n_breaks = 0
    for l in range(m_max):
        best = 0.0
        for i, j in _segments(T, fits[l][1]):
            if j - i < 2 * h:
                continue
            k = np.arange(i + h, j - h + 1)
            gain = _ssr(s1, s2, i, j) - _ssr(s1, s2, i, k) - _ssr(s1, s2, k, j)
            best = max(best, float(gain.max()) / w2s[l])
        p = max(p_floor, 1 - _cdf_from_table(tab, "supF1", best) ** (l + 1))
        seq.append({"l": l, "supF": best, "p": p})
        if p >= alpha:
            break
        n_breaks = l + 1
    ends = fits[n_breaks][1]
    means = [float(y[i:j].mean()) for i, j in _segments(T, ends)]
    w2, b = _omega2(_resid(y, ends), bandwidth)
    c = _date_quantile(1 - alpha / 2)
    intervals = []
    for n, k in enumerate(ends):
        half = int(np.ceil(c * w2 / (means[n + 1] - means[n]) ** 2))
        intervals.append((max(0, k - half - 1), min(T, k + half + 1)))
    return {"n_breaks": n_breaks, "breaks": ends, "intervals": intervals, "segment_means": means, "h": h,
            "eps": eps, "bandwidth": b, "alpha": alpha, "supF": supf, "supF_p": supf_p, "UDmax": udmax,
            "UDmax_p": udmax_p, "sequential": seq, "omega2": w2, "date_c": c, "p_floor": p_floor}  # fmt: skip


# ── Local level ───────────────────────────────────────────────────────────────────────────────────────────────────


def _local_level_filter(y: np.ndarray, q: np.ndarray):
    """Scaled (σ²_ε = 1) diffuse local-level filter for every q at once. Returns the concentrated log-likelihood
    (nq,), σ̂²_ε (nq,), and the filtered mean and scaled variance (T, nq)."""
    T, nq = len(y), len(q)
    a = np.full(nq, y[0])  # μ₁ | y₁ under the diffuse prior: mean y₁, scaled variance 1
    p = np.ones(nq)
    af = np.empty((T, nq))
    pf = np.empty((T, nq))
    af[0], pf[0] = a, p
    sv = np.zeros(nq)
    slf = np.zeros(nq)
    for t in range(1, T):
        p_pred = p + q
        f = p_pred + 1.0
        v = y[t] - a
        k = p_pred / f
        a = a + k * v
        p = p_pred * (1 - k)
        af[t], pf[t] = a, p
        sv += v * v / f
        slf += np.log(f)
    n = T - 1
    s2 = sv / n
    ll = -0.5 * n * (np.log(2 * np.pi) + np.log(s2) + 1) - 0.5 * slf
    return ll, s2, af, pf


def local_level(y, *, q_grid=None, alpha: float = 0.05) -> dict:
    """MLE local-level tracker of a daily P&L stream (module docstring), fitted on the whole of y: the answer *at T*.
    Returns q (MLE), the profile (q_grid, loglik, including q = 0 first), σ²_ε, the filtered μ_t, its sd and
    P(μ_t > 0) for every t — **not causal before T** (q̂ and σ̂²_ε use all of y; a monitor uses
    `local_level_track`) — the same at T, the same at q = 0 (`P_mu_T_pos_q0`: the full-sample mean's posterior), the LR statistic of q = 0 with its
    mixture p-value, `q_zero` (True: q = 0 is not rejected at alpha — the edge is not shown to move, and a q̂ > 0
    that the data do not support can still put P(μ_T > 0) far from the constant-mean value, so both are returned
    and a report prints both) and `q_at_grid_edge`."""
    y = np.asarray(y, dtype=float)
    if y.ndim != 1 or len(y) < 10 or np.isnan(y).any():
        raise ValueError("need a 1-D series of at least 10 observations without NaN")
    if not y.std() > 0:
        raise ValueError("constant series: the local-level likelihood is degenerate")
    grid = np.r_[0.0, np.logspace(-9, 0, 91) if q_grid is None else np.asarray(q_grid, dtype=float)]
    ll, *_ = _local_level_filter(y, grid)
    i = int(np.argmax(ll))
    q_hat, ll_hat = float(grid[i]), float(ll[i])
    if 0 < i < len(grid) - 1:
        lo, hi = np.log(grid[max(i - 1, 1)]), np.log(grid[i + 1])
        res = minimize_scalar(lambda z: -_local_level_filter(y, np.array([np.exp(z)]))[0][0], bounds=(lo, hi),
                              method="bounded", options={"xatol": 1e-6})  # fmt: skip
        if -res.fun > ll_hat:
            q_hat, ll_hat = float(np.exp(res.x)), float(-res.fun)
    ll_hat, s2, af, pf = (x[0] if x.ndim == 1 else x[:, 0] for x in _local_level_filter(y, np.array([q_hat])))
    _, s20, af0, pf0 = _local_level_filter(y, np.array([0.0]))
    p_pos_q0 = float(norm.cdf(af0[-1, 0] / np.sqrt(s20[0] * pf0[-1, 0])))
    lr = max(0.0, 2 * (float(ll_hat) - float(ll[0])))
    p_q0 = 0.5 * float(chi2.sf(lr, 1)) if lr > 0 else 1.0
    sd = np.sqrt(s2 * pf)
    with np.errstate(divide="ignore", invalid="ignore"):
        p_pos = norm.cdf(af / sd)
    return {"q": q_hat, "loglik": float(ll_hat), "sigma2_eps": float(s2), "profile_q": grid, "profile_ll": ll,
            "mu": af, "sd": sd, "p_pos": p_pos, "mu_T": float(af[-1]), "sd_T": float(sd[-1]),
            "P_mu_T_pos": float(p_pos[-1]), "P_mu_T_pos_q0": p_pos_q0, "lr_q0": lr, "p_q0": p_q0, "q_zero": bool(p_q0 >= alpha),
            "q_at_grid_edge": bool(i in (1, len(grid) - 1))}  # fmt: skip


def local_level_track(
    y, *, min_obs: int = 252, refit_every: int = 21, q_grid=None, alpha: float = 0.05
) -> dict:
    """Causal local-level track (the U13 monitor's form): at each refit session r = min_obs, min_obs + refit_every,
    … q̂ and σ̂²_ε are estimated by `local_level` on y[:r] (sessions < r); sessions r … r + refit_every − 1 are then
    filtered with those parameters, so P(μ_t > 0) uses only y[:t + 1]. Sessions < min_obs are NaN (no fit yet,
    stated, not filled). Returns mu, sd, p_pos (T,), and per refit: the session r, q̂, q_zero."""
    y = np.asarray(y, dtype=float)
    T = len(y)
    if min_obs < 10 or refit_every < 1 or T <= min_obs:
        raise ValueError(f"need min_obs >= 10, refit_every >= 1 and more than min_obs observations; got {T}")
    mu, sd = np.full(T, np.nan), np.full(T, np.nan)
    refits = []
    for r in range(min_obs, T, refit_every):
        fit = local_level(y[:r], q_grid=q_grid, alpha=alpha)
        stop = min(T, r + refit_every)
        _, _, af, pf = _local_level_filter(y[:stop], np.array([fit["q"]]))
        mu[r:stop] = af[r:stop, 0]
        sd[r:stop] = np.sqrt(fit["sigma2_eps"] * pf[r:stop, 0])
        refits.append({"session": r, "q": fit["q"], "q_zero": fit["q_zero"]})
    with np.errstate(divide="ignore", invalid="ignore"):
        p_pos = norm.cdf(mu / sd)
    return {
        "mu": mu,
        "sd": sd,
        "p_pos": p_pos,
        "refits": refits,
        "min_obs": min_obs,
        "refit_every": refit_every,
    }
