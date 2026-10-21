"""U5 diagnostics (PLAN.md §7 U5): the inherited claim reproduced, and the reversal structure the rule depends on.

- `diag_market(frame)`: a `Market(source="diag")` from a `load_long` frame (adjusted prices as the prints, dividend
  0, so every leg is an exact total-return leg), or with `price="close_split"` the split-adjusted close `VWAP.R`
  read (price return only: ex-dividend drops stay in it). Only `run_diagnostic` accepts it.
- `r_sharpe(r)`: PerformanceAnalytics' `SharpeRatio.annualized` at its defaults (Rf 0, scale 252, geometric):
  ((Π(1 + r))^(252/T) − 1) / (s·√252).
- `vwap_r_streams(market)`: `VWAP.R`'s two streams on the same rows as the script: strategy f_t·r_{t+1} and index
  r_t over t = 2 … T−1 (`na.omit` of the `cbind` drops the first and the last row).
- `autocorr(r, k)`: lag-k autocorrelation (demeaned, full-sample denominator) and its heteroskedasticity-robust
  standard error √(Σ e_t² e_{t−k}²) / Σ e_t² (Lo & MacKinlay 1988, δ_k; the SE of ρ̂_k without iid).
- `variance_ratio(r, q)`: VR(q) = 1 + 2 Σ_{j<q} (1 − j/q) ρ̂_j and Lo–MacKinlay's robust z* = (VR − 1)/√θ,
  θ = Σ_{j<q} (2(1 − j/q))² δ_j (the autocorrelation form; asymptotically equal to the overlapping-sum form).
- `cross_corr(x, y)`: corr(x_t, y_{t+1}) with the same robust SE (x on y's next value).
- `leg_contributions(result)`: per session t+1, f_t·r_on (the overnight leg a same-close fill captures and a
  next-open fill cannot), f_t·r_id (shared), f_{t−1}·r_on (the overnight leg next_open holds instead).

No function here records a look; the U5 script builds every table through `looks.looked`."""

import numpy as np
import pandas as pd

from vwap_mr.backtest.market import Market
from vwap_mr.data.exo import Exo
from vwap_mr.stats.bootstrap import newey_west_ols

ANN = 252


def diag_market(frame: pd.DataFrame, *, price: str = "close", dtb3: pd.DataFrame | None = None) -> Market:
    """`Market(source="diag")` over the rows of `frame` (a `load_long` frame or a slice of one)."""
    if frame.attrs.get("source") != "diag":
        raise ValueError("diag_market needs a load_long frame (attrs['source'] == 'diag')")
    if price not in ("close", "close_split"):
        raise ValueError(f"price must be 'close' or 'close_split', got {price!r}")
    px = frame[price].astype(float)
    opn = frame["open"] * (px / frame["close"])  # the open on the same price basis as the close
    s = pd.DataFrame({"open_raw": opn, "close_raw": px, "dividend": 0.0, "open": opn, "close": px,
                      "volume": frame["volume"].astype(float), "is_diag": True}, index=frame.index)  # fmt: skip
    s.attrs["source"] = "diag"
    return Market(s, "diag", exo=None if dtb3 is None else Exo({"DTB3": dtb3}), symbol="diag")


def r_sharpe(r) -> float:
    x = np.asarray(r, dtype=float)
    geo = np.prod(1 + x) ** (ANN / len(x)) - 1
    return float(geo / (x.std(ddof=1) * np.sqrt(ANN)))


def vwap_r_streams(market: Market, targets: pd.Series) -> pd.DataFrame:
    """Columns `strategy` (f_t·r_{t+1}) and `index` (r_t), indexed by t, on VWAP.R's rows t = 2 … T−1."""
    close = market.sessions["close"]
    ret = close / close.shift(1) - 1
    out = pd.DataFrame({"strategy": targets * ret.shift(-1), "index": ret})
    return out.iloc[1:-1]


def autocorr(r, k: int) -> tuple[float, float]:
    """(ρ̂_k, robust se)."""
    e = np.asarray(r, dtype=float)
    e = e - e.mean()
    den = float(e @ e)
    prod = e[k:] * e[:-k]
    return float(prod.sum() / den), float(np.sqrt((prod**2).sum()) / den)


def variance_ratio(r, q: int) -> tuple[float, float]:
    """(VR(q), robust z*)."""
    vr, theta = 1.0, 0.0
    for j in range(1, q):
        rho, se = autocorr(r, j)
        w = 2 * (1 - j / q)
        vr += w * rho
        theta += w**2 * se**2
    return vr, (vr - 1) / np.sqrt(theta)


def cross_corr(x, y) -> tuple[float, float]:
    """(corr(x_t, y_{t+1}), robust se), demeaned, full-sample normalisation."""
    a, b = np.asarray(x, dtype=float)[:-1], np.asarray(y, dtype=float)[1:]
    a, b = a - a.mean(), b - b.mean()
    prod = a * b
    den = np.sqrt((a @ a) * (b @ b))
    return float(prod.sum() / den), float(np.sqrt((prod**2).sum()) / den)


def nw_mean(x, lags: int = 5) -> tuple[float, float]:
    """(mean, Newey–West t)."""
    x = np.asarray(x, dtype=float)
    return float(x.mean()), float(newey_west_ols(x, np.empty((len(x), 0)), lags)["t"][0])


def legs_of(sessions: pd.DataFrame) -> pd.DataFrame:
    """Overnight, intraday and close-to-close total-return legs of session t (adjusted open / close; row 0 NaN)."""
    o, c = sessions["open"], sessions["close"]
    return pd.DataFrame({"r_on": o / c.shift(1) - 1, "r_id": c / o - 1, "r_cc": c / c.shift(1) - 1})


def leg_contributions(result) -> pd.DataFrame:
    """Per-session contributions from a `next_open` result's legs and targets (row t+1 holds session t+1's legs)."""
    if "r_on" not in result.legs:
        raise ValueError("leg_contributions needs a next_open result (r_on, r_id legs)")
    f = result.targets
    lg = result.legs
    out = pd.DataFrame({"on_same_close": f.shift(1) * lg["r_on"], "intraday": f.shift(1) * lg["r_id"],
                        "on_next_open": f.shift(2) * lg["r_on"]})  # fmt: skip
    return out.iloc[2:]
