"""Calibration studies on synthetic series (U8 estimator noise; U9 adds regime detection). No market data.

Generators:
- `fgn(n, H, rng)`: fractional Gaussian noise by the Davies–Harte circulant embedding (exact; unit variance);
- `arfima_t(n, H, rng)`: ARFIMA(0, d, 0), d = H − ½, driven by Student-t₄ innovations scaled to unit variance (MA(∞)
  weights ψ_k = ψ_{k−1}(k − 1 + d)/k truncated at `burn` lags, applied by FFT convolution): long memory with fat tails;
  `arfima_gauss` is the same with Gaussian innovations, so the study separates ARFIMA's short-lag structure (it differs
  from fGn's at the same H) from the tails.

U8 study (`noise_table`, `rolling_persistence`, `delta_alpha_tails`, `rw_phi_null`): bias and SD of each Hurst-type
estimator over H × window × innovation, the smallest window with SD ≤ 0.025 (PLAN.md §7 U8's decision rule), the
autocorrelation of consecutive rolling estimates and the effective number of independent estimates per decade, the
spurious Δα from fat tails alone, and the random-walk value of B4's AR(1) φ of d*_t."""

import time
from collections.abc import Callable

import numpy as np
import pandas as pd

from vwap_mr.features.deviation import state_deviation
from vwap_mr.regime import hurst
from vwap_mr.regime.wavelet import abry_veitch

SD_MAX = 0.025


def fgn(n: int, H: float, rng: np.random.Generator) -> np.ndarray:
    k = np.arange(n + 1, dtype=float)
    gamma = 0.5 * ((k + 1) ** (2 * H) - 2 * k ** (2 * H) + np.abs(k - 1) ** (2 * H))
    c = np.r_[gamma, gamma[-2:0:-1]]
    lam = np.fft.fft(c).real
    if (lam < -1e-10).any():
        raise ValueError("circulant embedding is not non-negative definite")
    m = len(c)
    z = rng.standard_normal(m) + 1j * rng.standard_normal(m)
    return np.fft.fft(np.sqrt(np.maximum(lam, 0) / m) * z).real[:n]


def arfima_t(n: int, H: float, rng: np.random.Generator, *, df: float = 4.0, burn: int = 2000) -> np.ndarray:
    d = H - 0.5
    k = np.arange(1, burn + n)
    psi = np.r_[1.0, np.cumprod((k - 1 + d) / k)]
    e = (
        rng.standard_normal(burn + n)
        if np.isinf(df)
        else rng.standard_t(df, burn + n) / np.sqrt(df / (df - 2))
    )
    size = 1 << int(np.ceil(np.log2(2 * (burn + n))))
    x = np.fft.irfft(np.fft.rfft(e, size) * np.fft.rfft(psi, size), size)[: burn + n]
    x = x[burn:]
    return x / x.std()


def arfima_gauss(n: int, H: float, rng: np.random.Generator, *, burn: int = 2000) -> np.ndarray:
    """ARFIMA(0, H − ½, 0) with Gaussian innovations: separates ARFIMA's short-lag structure from t₄'s tails."""
    return arfima_t(n, H, rng, df=np.inf, burn=burn)


GENERATORS: dict[str, Callable[..., np.ndarray]] = {"gaussian fGn": fgn, "ARFIMA gaussian": arfima_gauss,
                                                    "ARFIMA t4": arfima_t}  # fmt: skip

ESTIMATORS: dict[str, Callable[[np.ndarray], float]] = {
    "DFA-1 standard [10, 126]": lambda x: hurst.dfa(x, "standard")["alpha"],
    "DFA-1 short [4, 32]": lambda x: hurst.dfa(x, "short")["alpha"],
    "Abry–Veitch short j 1–4": lambda x: abry_veitch(x, "short"),
    "Abry–Veitch standard j 2–6": lambda x: abry_veitch(x, "standard"),
    "R/S classic (reference)": hurst.rs_hurst,
}  # MF-DFA h(2) on the standard set is DFA-1 standard exactly (tested), so it is not listed twice


def _feasible(name: str, n: int) -> bool:
    return not (name.startswith("DFA-1 standard") and n < 252) and not ("standard j" in name and n < 512)


def noise_table(
    Hs=(0.40, 0.45, 0.50, 0.55, 0.60), ns=(252, 504, 1008), draws: int = 500, seed: int = 0
) -> pd.DataFrame:
    """Bias, SD and runtime per estimator × generator × H × n (the same draws for every estimator)."""
    rows = []
    for gi, (gname, gen) in enumerate(GENERATORS.items()):
        for hi, H in enumerate(Hs):
            for n in ns:
                rng = np.random.default_rng([seed, gi, hi, n])
                xs = [gen(n, H, rng) for _ in range(draws)]
                for ename, est in ESTIMATORS.items():
                    if not _feasible(ename, n):
                        continue
                    t0 = time.perf_counter()
                    est_v = np.array([est(x) for x in xs])
                    ms = (time.perf_counter() - t0) / draws * 1e3
                    rows.append({"estimator": ename, "innovations": gname, "H": H, "n": n,
                                 "bias": float(est_v.mean() - H), "sd": float(est_v.std(ddof=1)), "ms": ms})  # fmt: skip
    return pd.DataFrame(rows)


def attenuation(tab: pd.DataFrame) -> pd.DataFrame:
    """Per estimator × generator × n: the slope of the mean estimate on the true H (1 = unattenuated)."""
    rows = []
    for _, grp in tab.groupby(["estimator", "innovations", "n"]):
        if grp["H"].nunique() > 1:
            slope = float(np.polyfit(grp["H"], grp["H"] + grp["bias"], 1)[0])
            r0 = grp.iloc[0]
            rows.append({"estimator": r0["estimator"], "innovations": r0["innovations"], "n": int(r0["n"]),
                         "slope": slope})  # fmt: skip
    return pd.DataFrame(rows)


def min_window(tab: pd.DataFrame, sd_max: float = SD_MAX) -> pd.DataFrame:
    """Per estimator and generator: the smallest n whose SD (worst over H) is ≤ sd_max, else NaN (under-powered) —
    the §7 U8 rule, on the raw SD. `min_n_h` / `sdH@n` apply it to the SD in H units (SD / attenuation slope), which
    is stricter when the estimator responds to H by less than one for one."""
    worst = tab.groupby(["estimator", "innovations", "n"])["sd"].max().reset_index()
    att = attenuation(tab)
    slope = {(e, g, int(n)): float(v) for e, g, n, v in
             zip(att["estimator"], att["innovations"], att["n"], att["slope"], strict=True)} if len(att) else {}  # fmt: skip
    out = []
    for _, grp in worst.groupby(["estimator", "innovations"]):
        e, g = grp.iloc[0]["estimator"], grp.iloc[0]["innovations"]
        ok = grp[grp["sd"] <= sd_max]
        row = {"estimator": e, "innovations": g, "min_n": int(ok["n"].min()) if len(ok) else np.nan,
               **{f"sd@{int(n)}": float(s) for n, s in zip(grp["n"], grp["sd"], strict=True)}}  # fmt: skip
        if slope:
            sd_h = {
                int(n): float(s) / slope[(e, g, int(n))] for n, s in zip(grp["n"], grp["sd"], strict=True)
            }
            okh = [n for n, s in sd_h.items() if s <= sd_max]
            row |= {"min_n_h": min(okh) if okh else np.nan, **{f"sdH@{n}": s for n, s in sd_h.items()}}
        out.append(row)
    return pd.DataFrame(out)


def lo_rs_rejection(
    Hs=(0.40, 0.45, 0.50, 0.55, 0.60), n: int = 1008, draws: int = 500, seed: int = 4
) -> pd.DataFrame:
    """Share of fGn draws where Lo's modified R/S V falls outside its 95 % short-memory range [0.809, 1.862]: the size
    at H = 0.5 and the power elsewhere (the reference test the literature quotes; PLAN §7 U8)."""
    rows = []
    for i, H in enumerate(Hs):
        rng = np.random.default_rng([seed, i])
        v = np.array([hurst.lo_modified_rs(fgn(n, H, rng)) for _ in range(draws)])
        rows.append({"H": H, "n": n, "reject": float(((v < 0.809) | (v > 1.862)).mean())})
    return pd.DataFrame(rows)


def _iact(x: np.ndarray, max_lag: int) -> float:
    """Integrated autocorrelation time 1 + 2 Σ ρ_k, summed until ρ_k first drops below 0.05."""
    x = x - x.mean()
    v = x @ x
    tau = 1.0
    for k in range(1, max_lag):
        rho = (x[k:] @ x[:-k]) / v
        if rho < 0.05:
            break
        tau += 2 * rho
    return tau


def rolling_persistence(
    n_total: int = 7560, windows=(252, 504, 1008), H: float = 0.5, seed: int = 1
) -> pd.DataFrame:
    """Lag-1 autocorrelation of consecutive daily rolling estimates and the effective number of independent
    estimates per decade (2,520 / integrated autocorrelation time), on one long fGn path."""
    r = pd.Series(fgn(n_total, H, np.random.default_rng(seed)))
    rows = []
    for w in windows:
        series = {"DFA-1 short [4, 32]": hurst.rolling_dfa(r, w, "short")}
        if w >= 252:
            series["DFA-1 standard [10, 126]"] = hurst.rolling_dfa(r, w, "standard")
        from vwap_mr.regime.wavelet import rolling_av

        series["Abry–Veitch short j 1–4"] = rolling_av(r, w, "short")
        for name, s in series.items():
            x = s.dropna().to_numpy()
            rows.append({"estimator": name, "window": w, "rho1": float(np.corrcoef(x[1:], x[:-1])[0, 1]),
                         "n_eff_per_decade": 2520 / _iact(x, 3 * w)})  # fmt: skip
    return pd.DataFrame(rows)


def delta_alpha_tails(n: int = 1008, draws: int = 200, seed: int = 2) -> pd.DataFrame:
    """MF-DFA Δα (standard scales, q −5 … 5) on iid Gaussian vs iid Student-t₄ (both H = ½, monofractal)."""
    rng = np.random.default_rng(seed)
    rows = []
    for name, draw in (
        ("iid Gaussian", lambda: rng.standard_normal(n)),
        ("iid t4", lambda: rng.standard_t(4, n)),
    ):
        da = np.array([hurst.mfdfa(draw())["delta_alpha"] for _ in range(draws)])
        rows.append({"series": name, "n": n, "delta_alpha_mean": float(da.mean()), "delta_alpha_sd": float(da.std(ddof=1)),
                     "p95": float(np.quantile(da, 0.95))})  # fmt: skip
    return pd.DataFrame(rows)


def ar1_phi(x: pd.Series, window: int) -> pd.Series:
    """Rolling OLS slope of x_t on x_{t−1} over the trailing `window` pairs."""
    lag = x.shift(1)
    return x.rolling(window).cov(lag) / lag.rolling(window).var()


def rw_phi_null(n_sessions: int = 5000, window: int = 126, draws: int = 50, seed: int = 3) -> dict:
    """B4's random-walk reference: φ of d*_t (anchor VWAP_5 over t−5 … t−1) on random-walk closes with lognormal
    volume; d* reverts by construction, so the reference is well above 0."""
    rng = np.random.default_rng(seed)
    phis = []
    for _ in range(draws):
        close = pd.Series(100 * np.exp(np.cumsum(rng.normal(0, 0.01, n_sessions))))
        vol = pd.Series(rng.lognormal(18, 0.3, n_sessions))
        phis.append(ar1_phi(state_deviation(close, vol, 5), window).dropna().to_numpy())
    v = np.concatenate(phis)
    return {"window": window, "mean": float(v.mean()), "sd": float(v.std(ddof=1)),
            "q05": float(np.quantile(v, 0.05)), "q95": float(np.quantile(v, 0.95))}  # fmt: skip


# ── U9: regime detection on synthetic switching series ──────────────────────


def ms_series(n: int, dur: float, rng: np.random.Generator, *, kind: str) -> tuple[np.ndarray, np.ndarray]:
    """A two-regime Markov-switching series with mean regime duration `dur` (stay probability 1 − 1/dur) and the
    indicator of the target regime: kind "phi" is AR(1) with φ ∈ {0.3 (target: fast reversion), 0.9}, unit
    innovations; kind "sigma" is iid N(0, σ²) with σ ∈ {1, 2 (target: high σ)}."""
    s = np.empty(n, dtype=int)
    s[0] = rng.integers(2)
    flips = rng.random(n) < 1 / dur
    for t in range(1, n):
        s[t] = 1 - s[t - 1] if flips[t] else s[t - 1]
    e = rng.standard_normal(n)
    if kind == "phi":
        phi = np.where(s == 0, 0.3, 0.9)
        x = np.empty(n)
        x[0] = e[0]
        for t in range(1, n):
            x[t] = phi[t] * x[t - 1] + e[t]
        return x, s == 0
    if kind == "sigma":
        return e * np.where(s == 1, 2.0, 1.0), s == 1
    raise ValueError(kind)


def detection_metrics(p_next: np.ndarray, target: np.ndarray, threshold: float = 0.5) -> dict:
    """p_next[t] predicts target[t + 1]. The signal at t + 1 is p_next[t] > threshold. Delay: sessions from a switch
    into the target regime to the first signal inside that spell (spells that end first count as missed). False
    alarms: signal onsets while the target regime is off, per 1,000 evaluated sessions. Brier: mean (p − target)²."""
    p = np.asarray(p_next, dtype=float)[:-1]
    y = np.asarray(target, dtype=bool)[1:]
    ok = np.isfinite(p)
    p, y = p[ok], y[ok]
    sig = p > threshold
    delays, missed = [], 0
    starts = np.flatnonzero(y & ~np.r_[True, y[:-1]])
    for s0 in starts:
        k = s0
        while k < len(y) and y[k]:
            if sig[k]:
                delays.append(k - s0)
                break
            k += 1
        else:
            missed += 1
    onsets = sig & ~np.r_[False, sig[:-1]]
    return {"n": len(y), "switches": len(starts), "delay_median": float(np.median(delays)) if delays else np.nan,
            "missed": missed, "false_alarms_per_1000": float((onsets & ~y).sum() / len(y) * 1000),
            "brier": float(np.mean((p - y) ** 2)), "share_on": float(sig.mean())}  # fmt: skip


def baseline_prob(x: pd.Series, kind: str) -> pd.Series:
    """0/1 'probabilities' of the target regime from the §6.4 baselines, stamped at t (predicting t + 1): kind "phi" is
    B4 (rolling-126 AR(1) φ below its trailing-504 median → fast); kind "sigma" is B1 (rolling-21 σ in the top two
    quintiles of its trailing 504 → high)."""
    from vwap_mr.experiments.persistence import trailing_quantile

    if kind == "phi":
        q = trailing_quantile(ar1_phi(x, 126), 504)
        return (q <= 0.5).astype(float).where(q.notna())
    q = trailing_quantile(x.rolling(21).std(ddof=1), 504)
    return (q > 0.6).astype(float).where(q.notna())


def change_metrics(
    p_recent: np.ndarray, regime_switch: np.ndarray, threshold: float = 0.5, horizon: int = 21
) -> dict:
    """For a change detector (R3): delay from each switch to the first p_recent > threshold within `horizon` sessions
    (else missed); false alarms = onsets with no switch in the previous `horizon` sessions, per 1,000 sessions."""
    p = np.asarray(p_recent, dtype=float)
    ok = np.isfinite(p)
    sig = (p > threshold) & ok
    sw = np.flatnonzero(regime_switch & ok)
    delays, missed = [], 0
    for s0 in sw:
        hit = np.flatnonzero(sig[s0 : s0 + horizon])
        if len(hit):
            delays.append(int(hit[0]))
        else:
            missed += 1
    onsets = np.flatnonzero(sig & ~np.r_[False, sig[:-1]])
    recent = np.zeros(len(p), bool)
    for s0 in sw:
        recent[s0 : s0 + horizon] = True
    n = int(ok.sum())
    return {"n": n, "switches": len(sw), "delay_median": float(np.median(delays)) if delays else np.nan,
            "missed": missed, "false_alarms_per_1000": float((~recent[onsets]).sum() / n * 1000),
            "share_on": float(sig[ok].mean())}  # fmt: skip
