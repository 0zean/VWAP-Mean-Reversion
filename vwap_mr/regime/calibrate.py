"""Calibration studies on synthetic series (U8 estimator noise; U9 adds regime detection). No market data.

Generators:
- `fgn(n, H, rng)`: fractional Gaussian noise by the Davies–Harte circulant embedding (exact; unit variance);
- `arfima_t(n, H, rng)`: ARFIMA(0, d, 0), d = H − ½, driven by Student-t₄ innovations scaled to unit variance (MA(∞)
  weights ψ_k = ψ_{k−1}(k − 1 + d)/k truncated at `burn` lags, applied by FFT convolution): long memory with fat tails.

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
    e = rng.standard_t(df, burn + n) / np.sqrt(df / (df - 2))
    size = 1 << int(np.ceil(np.log2(2 * (burn + n))))
    x = np.fft.irfft(np.fft.rfft(e, size) * np.fft.rfft(psi, size), size)[: burn + n]
    x = x[burn:]
    return x / x.std()


GENERATORS: dict[str, Callable[..., np.ndarray]] = {"gaussian fGn": fgn, "ARFIMA t4": arfima_t}

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


def min_window(tab: pd.DataFrame, sd_max: float = SD_MAX) -> pd.DataFrame:
    """Per estimator and generator: the smallest n whose SD (worst over H) is ≤ sd_max, else NaN (under-powered)."""
    worst = tab.groupby(["estimator", "innovations", "n"])["sd"].max().reset_index()
    out = []
    for (e, g), grp in worst.groupby(["estimator", "innovations"]):
        ok = grp[grp["sd"] <= sd_max]
        out.append({"estimator": e, "innovations": g, "min_n": int(ok["n"].min()) if len(ok) else np.nan,
                    **{f"sd@{int(n)}": float(s) for n, s in zip(grp["n"], grp["sd"], strict=True)}})  # fmt: skip
    return pd.DataFrame(out)


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
