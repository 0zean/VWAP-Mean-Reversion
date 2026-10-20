"""Minimum detectable effects and simulated power (U4, SPEC.md §6; ported from 0zean/wavelet-meta-model
`families/power.py`: `mde_alpha`, `mde_delta_sharpe`, `garch_t`, `simulate_power`).

Analytic MDE of a one-sided test at level α/k (k = the hypotheses Holm holds at once; its first step is
Bonferroni's α/k) and power 1 − β: MDE = (z_{1−α/k} + z_{1−β})·SE, with
- mean:                   SE = σ_d / √n;
- conditional difference: SE = σ_d·√(1/(πn) + 1/((1 − π)n))  (iid r, a gate on a share π of sessions);
- paired Sharpe:          SE = √([2(1 − ρ) + SR²(1 − ρ²)] / n) per day, × √252 annualized (Jobson & Korkie 1981 as
                          corrected by Memmel 2003, iid normal, at the null SR_a = SR_b = SR).
σ_d is the daily volatility (PLAN §6.6 states power in σ_d, not annualized). These are iid lower bounds; fat tails
and clustering are checked by `simulate_rejection` through the real tests under GARCH(1,1)-t."""

import numpy as np
from scipy.stats import norm

from vwap_mr.stats.bootstrap import cond_mean_test, mean_test

TRADING_DAYS = 252


def _z(alpha: float, power: float, k: int, sided: str) -> tuple[float, float]:
    if not 0 < alpha < 1 or not 0 < power < 1 or k < 1 or sided not in ("one", "two"):
        raise ValueError(
            f"need 0 < alpha, power < 1, k >= 1, sided one|two; got {alpha}, {power}, {k}, {sided}"
        )
    a = alpha / k
    return float(norm.ppf(1 - a) if sided == "one" else norm.ppf(1 - a / 2)), float(norm.ppf(power))


def mde_mean(
    n: int, sigma_d: float, *, alpha: float = 0.05, power: float = 0.8, k: int = 1, sided: str = "one"
):
    """Minimum detectable daily mean (fraction and bp per day, annualized × 252, the Sharpe it implies)."""
    if n < 2 or not sigma_d > 0:
        raise ValueError(f"need n >= 2 and sigma_d > 0; got {n}, {sigma_d}")
    za, zp = _z(alpha, power, k, sided)
    se = sigma_d / np.sqrt(n)
    mde = (za + zp) * se
    return {"kind": "mean", "n": n, "sigma_d": sigma_d, "k": k, "alpha": alpha, "power": power, "sided": sided,
            "se_bp_day": se * 1e4, "mde": mde, "mde_bp_day": mde * 1e4, "mde_ann": mde * TRADING_DAYS,
            "mde_sharpe_ann": mde / sigma_d * np.sqrt(TRADING_DAYS)}  # fmt: skip


def mde_cond_diff(
    n: int,
    sigma_d: float,
    pi: float,
    *,
    alpha: float = 0.05,
    power: float = 0.8,
    k: int = 1,
    sided: str = "one",
) -> dict:
    """Minimum detectable conditional mean difference Δμ (bp per day) for a gate on a share π of n sessions."""
    if n < 2 or not sigma_d > 0 or not 0 < pi < 1:
        raise ValueError(f"need n >= 2, sigma_d > 0, 0 < pi < 1; got {n}, {sigma_d}, {pi}")
    za, zp = _z(alpha, power, k, sided)
    se = sigma_d * np.sqrt(1 / (pi * n) + 1 / ((1 - pi) * n))
    mde = (za + zp) * se
    return {"kind": "cond_mean_diff", "n": n, "sigma_d": sigma_d, "pi": pi, "k": k, "alpha": alpha,
            "power": power, "sided": sided, "se_bp_day": se * 1e4, "mde": mde, "mde_bp_day": mde * 1e4}  # fmt: skip


def mde_delta_sharpe(
    n: int, rho: float, sharpe_ref: float = 0.0, *, alpha: float = 0.05, power: float = 0.8, k: int = 1,
    sided: str = "one",
) -> dict:  # fmt: skip
    """Minimum detectable annualized Sharpe difference of a paired test at stream correlation ρ, evaluated at the
    null SR_a = SR_b = `sharpe_ref` (annualized)."""
    if n < 2 or not -1 < rho < 1:
        raise ValueError(f"need n >= 2 and -1 < rho < 1; got {n}, {rho}")
    za, zp = _z(alpha, power, k, sided)
    s = sharpe_ref / np.sqrt(TRADING_DAYS)
    se_ann = float(np.sqrt((2 * (1 - rho) + s * s * (1 - rho * rho)) / n * TRADING_DAYS))
    return {"kind": "paired_sharpe", "n": n, "rho": rho, "sharpe_ref": sharpe_ref, "k": k, "alpha": alpha,
            "power": power, "sided": sided, "se_ann": se_ann, "mde_delta_sharpe": (za + zp) * se_ann}  # fmt: skip


def garch_t(
    n: int, sigma_d: float, rng: np.random.Generator, *, size: int = 1, nu: float = 5.0, a: float = 0.05,
    b: float = 0.90, burn: int = 500,
) -> np.ndarray:  # fmt: skip
    """(size, n) GARCH(1,1) daily returns with variance targeting (unconditional σ = sigma_d) and unit-variance
    Student-t(ν) innovations; `burn` draws discarded. Defaults: ν 5, α 0.05, β 0.90 (persistence 0.95)."""
    if not (nu > 2 and a >= 0 and b >= 0 and a + b < 1):
        raise ValueError(f"need nu > 2 and a + b < 1; got nu={nu}, a={a}, b={b}")
    omega = sigma_d**2 * (1 - a - b)
    z = rng.standard_t(nu, (size, n + burn)) * np.sqrt((nu - 2) / nu)
    r = np.empty((size, n + burn))
    v = np.full(size, sigma_d**2)
    for t in range(n + burn):
        r[:, t] = np.sqrt(v) * z[:, t]
        v = omega + a * r[:, t] ** 2 + b * v
    return r[:, burn:]


def simulate_rejection(
    kind: str, n: int, sigma_d: float, effect: float = 0.0, *, n_sims: int = 1000, stream: str = "iid",
    pi: float = 0.4, alpha: float = 0.05, k: int = 1, block: int = 21, n_boot: int = 999, seed: int = 0,
) -> dict:  # fmt: skip
    """Rejection rate of the one-sided test (`kind` "mean" or "cond_mean_diff") at level α/k over `n_sims` streams
    (`stream` "iid" normal or "garch_t") with the planted daily `effect` (0 → the test's size). For
    "cond_mean_diff" the gate is an independent Bernoulli(π) coin, and the effect is added on gate-on sessions:
    r_t + effect·g_t has Δμ = effect exactly, and an uninformative gate (effect 0) leaves r untouched."""
    if kind not in ("mean", "cond_mean_diff") or stream not in ("iid", "garch_t"):
        raise ValueError(f"kind mean|cond_mean_diff, stream iid|garch_t; got {kind}, {stream}")
    rng = np.random.default_rng(seed)
    r = rng.normal(0.0, sigma_d, (n_sims, n)) if stream == "iid" else garch_t(n, sigma_d, rng, size=n_sims)
    gates = rng.random((n_sims, n)) < pi
    level = alpha / k
    p = np.empty(n_sims)
    for i in range(n_sims):
        if kind == "mean":
            p[i] = mean_test(r[i] + effect, block=block, n_boot=n_boot, seed=seed + i)["p"]
        else:
            g = gates[i]
            p[i] = cond_mean_test(r[i] + effect * g, g, block=block, n_boot=n_boot, seed=seed + i)["p"]
    rate = float((p < level).mean())
    return {"kind": kind, "stream": stream, "n": n, "sigma_d": sigma_d, "effect_bp_day": effect * 1e4,
            "n_sims": n_sims, "level": level, "rejection_rate": rate,
            "mc_se": float(np.sqrt(rate * (1 - rate) / n_sims)), "p": p}  # fmt: skip
