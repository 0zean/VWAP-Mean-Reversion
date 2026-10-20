"""`run(market, strategy, *, costs, cash_yield) -> Result` (U3, SPEC.md §3): the position-series backtest of one symbol.

Exposure f_t ∈ [−1, 1] (fraction of equity) is decided at session t and filled at an auction print, never at a
price seen before the decision:
- `next_open` (close decision): shares change at the opening cross of t+1. Session t+1's price return is compounded,
  `(1 + f_{t−1}·r_on)(1 + f_t·r_id) − 1`, with r_on = (open_{t+1} + D_{t+1}) / close_t − 1 (the dividend goes to the
  overnight holder, who is the holder of record) and r_id = close_{t+1} / open_{t+1} − 1, on raw prints; the cost
  of |f_t − f_{t−1}| is booked at the open of t+1.
- `moc` (t1545 decision): shares change at the closing cross of t; session t+1's price return is
  f_t·((close_{t+1} + D_{t+1}) / close_t − 1); the cost is booked at the close of t (the fill).
- `same_close` (diagnostic only, `run_diagnostic`): the R convention, a close decision filled at the same close; the
  `moc` accounting with a `close` strategy. No spec path reaches it (`run` refuses it).

Equity is marked at the closing cross; a missing cross is replaced by the RTH 5Min close (open) and counted in
`Result.fallbacks`, never silently; a missing price with no fallback raises. Each fill resets the exposure to the
target (drift between fills is not traded or charged; SPEC.md §3 bounds it). Overnight carry: T-bill on free cash
and borrow on short notional (costs.py). The per-session ledger keeps the legs and targets, so `reprice` re-runs the
equity path exactly at another cost.

Two implementations: a vectorized one (growth factors × cumprod; refuses a session where the TAF per-trade cap
binds) and a per-session dollar loop (the reference; applies the cap). They are tested equal."""

from dataclasses import dataclass

import numpy as np
import pandas as pd

from vwap_mr.backtest.costs import CostModel, fee_rates
from vwap_mr.backtest.market import Market
from vwap_mr.config import DecisionTime
from vwap_mr.data.alpaca import midnight
from vwap_mr.data.secondary import DiagDataError
from vwap_mr.strategies.base import check_strategy, target_series

FILLS = ("next_open", "moc")
DIAGNOSTIC_FILLS = ("same_close",)
FILL_KIND = {"next_open": "open_auction", "moc": "close_auction", "same_close": "close_auction"}
LEDGER_COLUMNS = [
    "target", "exp_overnight", "exp_close", "traded", "notional", "spread", "sec_fee", "taf_fee", "cost",
    "interest", "borrow", "equity", "gross", "carry_ret", "cost_ret", "net", "rf", "excess",
]  # fmt: skip


@dataclass(frozen=True)
class Result:
    ledger: pd.DataFrame
    targets: pd.Series
    legs: pd.DataFrame
    cash_yield: pd.Series
    strategy: str
    fill: str
    costs: CostModel
    equity0: float
    fallbacks: dict
    diagnostic: bool
    source: str
    symbol: str

    @property
    def net(self) -> pd.Series:
        return self.ledger["net"]

    @property
    def excess(self) -> pd.Series:
        return self.ledger["excess"]


def _price(s: pd.DataFrame, side: str, need: np.ndarray) -> tuple[np.ndarray, int]:
    """Raw cross price of `side`, the RTH 5Min fallback where it is missing; counts the fallbacks used where `need`."""
    px = s[f"{side}_raw"].to_numpy(float).copy()
    miss = np.isnan(px)
    if miss.any() and f"{side}_fallback_raw" in s:
        px[miss] = s[f"{side}_fallback_raw"].to_numpy(float)[miss]
    bad = np.isnan(px) & need
    if bad.any():
        days = [str(d.date()) for d in s.index[bad][:5]]
        raise ValueError(f"no {side} cross and no RTH fallback for sessions {days}")
    used = int((miss & need & ~np.isnan(px)).sum())
    if used:
        print(f"[ENGINE] {used} session(s) marked/filled at the RTH 5Min {side} (missing {side} cross)")
    return px, used


def legs(market: Market, fill: str) -> tuple[pd.DataFrame, dict]:
    """Per-session price legs on raw prints: r_on, r_id (next_open only), r_cc, calendar days since the previous
    session. Row 0 has no previous close: its legs are 0."""
    s, n = market.sessions, len(market.sessions)
    close, fb_close = _price(s, "close", np.ones(n, bool))
    div = s["dividend"].to_numpy(float)
    if np.isnan(div).any():
        raise ValueError("Market.sessions.dividend has NaN (0 means no ex-date)")
    prev = np.r_[np.nan, close[:-1]]
    out = {"close_px": close, "dividend": div, "r_cc": np.r_[0.0, ((close + div) / prev - 1)[1:]]}
    fb_open = 0
    if fill == "next_open":
        opn, fb_open = _price(s, "open", np.r_[False, np.ones(n - 1, bool)])
        out |= {"open_px": opn, "r_on": np.r_[0.0, ((opn + div) / prev - 1)[1:]],
                "r_id": np.r_[0.0, (close / opn - 1)[1:]]}  # fmt: skip
    naive = midnight(market.index.tz_convert("America/New_York").tz_localize(None))
    days = np.r_[0, np.diff(naive).astype("timedelta64[D]").astype(int)]
    out["days"] = days.astype(float)
    return pd.DataFrame(out, index=market.index), {"open": fb_open, "close": fb_close}


def _shift(x: np.ndarray, k: int) -> np.ndarray:
    return np.r_[np.zeros(k), x[:-k]] if k else x


def _inputs(lg, f, fill, costs, y):
    n = len(lg)
    a, b = (_shift(f, 2), _shift(f, 1)) if fill == "next_open" else (_shift(f, 1), f)
    fee = (
        fee_rates(lg.index)
        if costs.fees
        else pd.DataFrame(0.0, index=lg.index, columns=["sec", "taf", "taf_max"])
    )
    px = lg["open_px"].to_numpy() if fill == "next_open" else lg["close_px"].to_numpy()
    y_prev = np.r_[0.0, y[:-1]]
    return {"n": n, "a": a, "b": b, "c1": costs.one_way(FILL_KIND[fill]), "br": costs.borrow_rate, "px": px,
            "sec": fee["sec"].to_numpy(), "taf": fee["taf"].to_numpy(), "taf_max": fee["taf_max"].to_numpy(),
            "y_prev": y_prev, "days": lg["days"].to_numpy()}  # fmt: skip


def _vectorized(lg, f, fill, costs, y, equity0) -> pd.DataFrame:
    p = _inputs(lg, f, fill, costs, y)
    a, b, days, y_prev = p["a"], p["b"], p["days"], p["y_prev"]
    interest_k = days / 360 * y_prev * (1 - np.abs(a))
    borrow_k = days / 360 * p["br"] * np.maximum(-a, 0)
    k = interest_k - borrow_k
    sells = np.maximum(a - b, 0)
    # TAF per dollar sold; only where something is sold (the price of a session without a fill may be absent)
    taf_k = np.divide(p["taf"], p["px"], out=np.zeros(p["n"]), where=sells > 0)
    c = np.abs(b - a) * p["c1"] + sells * (p["sec"] + taf_k)
    if fill == "next_open":
        r_on, r_id = lg["r_on"].to_numpy(), lg["r_id"].to_numpy()
        m = 1 + a * r_on + k
        after = 1 + b * r_id  # the intraday leg after the opening fill
        gross = (1 + a * r_on) * after - 1
    else:
        m = 1 + a * lg["r_cc"].to_numpy() + k
        after = np.ones(p["n"])
        gross = a * lg["r_cc"].to_numpy()
    growth = m * (1 - c) * after
    equity = equity0 * np.cumprod(growth)
    e_prev = np.r_[equity0, equity[:-1]]
    taf_fee = sells * m * e_prev * taf_k
    if (taf_fee > p["taf_max"] + 1e-12).any():
        raise ValueError("the FINRA TAF per-trade cap binds; use vectorized=False")
    ledger = {
        "target": f, "exp_overnight": a, "exp_close": b, "traded": np.abs(b - a),
        "notional": np.abs(b - a) * m * e_prev, "spread": np.abs(b - a) * m * e_prev * p["c1"],
        "sec_fee": sells * m * e_prev * p["sec"], "taf_fee": taf_fee, "interest": interest_k * e_prev,
        "borrow": borrow_k * e_prev, "equity": equity, "gross": gross, "carry_ret": k * after,
        "cost_ret": m * c * after, "net": growth - 1, "rf": y_prev * days / 360,
    }  # fmt: skip
    return _frame(ledger, lg.index)


def _loop(lg, f, fill, costs, y, equity0) -> pd.DataFrame:
    """Reference implementation in dollars, one session at a time."""
    p = _inputs(lg, f, fill, costs, y)
    r_cc = lg["r_cc"].to_numpy()
    r_on = lg["r_on"].to_numpy() if fill == "next_open" else None
    r_id = lg["r_id"].to_numpy() if fill == "next_open" else None
    rows, e = [], equity0
    for i in range(p["n"]):
        a, b, d = p["a"][i], p["b"][i], p["days"][i]
        interest = e * d / 360 * p["y_prev"][i] * (1 - abs(a))
        borrow = e * d / 360 * p["br"] * max(-a, 0.0)
        move = a * e * (r_on[i] if r_on is not None else r_cc[i])  # the price P&L up to the fill
        at_fill = e + move + interest - borrow
        notional = abs(b - a) * at_fill
        sold = max(a - b, 0.0) * at_fill
        spread, sec = notional * p["c1"], sold * p["sec"][i]
        taf = min(sold / p["px"][i] * p["taf"][i], p["taf_max"][i]) if sold > 0 else 0.0
        cost = spread + sec + taf
        intraday = b * r_id[i] if r_id is not None else 0.0
        e_new = (at_fill - cost) * (1 + intraday)
        rows.append({"target": f[i], "exp_overnight": a, "exp_close": b, "traded": abs(b - a), "notional": notional,
                     "spread": spread, "sec_fee": sec, "taf_fee": taf, "interest": interest, "borrow": borrow,
                     "equity": e_new, "gross": ((e + move) * (1 + intraday) - e) / e,
                     "carry_ret": (interest - borrow) * (1 + intraday) / e, "cost_ret": cost * (1 + intraday) / e,
                     "net": e_new / e - 1, "rf": p["y_prev"][i] * d / 360})  # fmt: skip
        e = e_new
    return _frame(pd.DataFrame(rows).to_dict("list"), lg.index)


def _frame(cols: dict, index) -> pd.DataFrame:
    out = pd.DataFrame(cols, index=index)
    out["cost"] = out["spread"] + out["sec_fee"] + out["taf_fee"]
    out["excess"] = out["net"] - out["rf"]
    return out[LEDGER_COLUMNS]


def _yield(cash_yield, index) -> pd.Series:
    y = pd.Series(cash_yield, index=index, dtype=float) if np.isscalar(cash_yield) else \
        pd.Series(cash_yield, dtype=float).reindex(index)  # fmt: skip
    if y.isna().any():
        raise ValueError(f"cash_yield missing for {int(y.isna().sum())} session(s)")
    return y


def simulate(lg, targets, fill, costs, cash_yield, equity0, *, vectorized=True) -> pd.DataFrame:
    f, y = targets.to_numpy(float), cash_yield.to_numpy(float)
    return (_vectorized if vectorized else _loop)(lg, f, fill, costs, y, float(equity0))


def _run(market, strategy, fill, costs, cash_yield, equity0, vectorized, diagnostic) -> Result:
    if not equity0 > 0:
        raise ValueError("equity0 must be > 0")
    targets = target_series(strategy, market, vectorized=vectorized)
    lg, fallbacks = legs(market, fill)
    if fill == "moc" and "close_fallback_end" in market.sessions:
        # a fallback close from a bar that ended by 15:45 is a price the decision already saw (2019-08-12)
        end = pd.to_datetime(market.sessions["close_fallback_end"])
        used = market.sessions["close_raw"].isna().to_numpy()
        seen = used & (end <= market.decision_instants(DecisionTime.T1545)).to_numpy()
        fallbacks["close_seen_at_decision"] = int(seen.sum())
        if seen.any():
            print(f"[ENGINE] {int(seen.sum())} moc fill(s) at a fallback close the 15:45 decision already saw: "
                  f"{[str(d.date()) for d in market.index[seen]]}")  # fmt: skip
    y = _yield(cash_yield, market.index)
    ledger = simulate(lg, targets, fill, costs, y, equity0, vectorized=vectorized)
    return Result(ledger, targets, lg, y, strategy.name, fill, costs, float(equity0), fallbacks, diagnostic,
                  market.source, market.symbol)  # fmt: skip


def run(market: Market, strategy, *, costs: CostModel, cash_yield, equity0: float = 30_000.0,
        vectorized: bool = True) -> Result:  # fmt: skip
    """Backtest `strategy` on `market` at its own fill (`next_open` for close decisions, `moc` for t1545)."""
    check_strategy(strategy)
    if strategy.fill not in FILLS:
        raise ValueError(f"{strategy.name}: fill {strategy.fill!r} is not a spec fill {FILLS}")
    if market.source == "diag":
        raise DiagDataError("diag (secondary-source) sessions cannot be backtested as Alpaca fills; "
                            "use run_diagnostic")  # fmt: skip
    return _run(market, strategy, strategy.fill, costs, cash_yield, equity0, vectorized, False)


def run_diagnostic(market: Market, strategy, *, fill: str, costs: CostModel, cash_yield, equity0: float = 30_000.0,
                   vectorized: bool = True) -> Result:  # fmt: skip
    """Diagnostic runs (U5): `same_close` for a close strategy, or any fill on `diag` sessions. The result is
    flagged `diagnostic` and never enters a registered statistic."""
    if fill not in FILLS + DIAGNOSTIC_FILLS:
        raise ValueError(f"unknown fill {fill!r}")
    if fill == "same_close" and strategy.decision != DecisionTime.CLOSE:
        raise ValueError("same_close is defined for close decisions only")
    if fill in FILLS and fill != strategy.fill:
        raise ValueError(f"{strategy.name} fills {strategy.fill!r}, not {fill!r}")
    return _run(market, strategy, fill, costs, cash_yield, equity0, vectorized, True)


def reprice(result: Result, round_trip_bp: float | str, *, vectorized: bool = True) -> Result:
    """The same targets and legs at another spread + slippage cost (fees, borrow and carry unchanged)."""
    costs = result.costs.with_round_trip(round_trip_bp)
    ledger = simulate(result.legs, result.targets, result.fill, costs, result.cash_yield, result.equity0,
                      vectorized=vectorized)  # fmt: skip
    return Result(ledger, result.targets, result.legs, result.cash_yield, result.strategy, result.fill, costs,
                  result.equity0, result.fallbacks, result.diagnostic, result.source, result.symbol)  # fmt: skip
