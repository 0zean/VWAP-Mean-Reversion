"""Causality harness (PLAN.md §8.6; ported from wavelet-meta-model `tests/test_methodology.py::perturb_after`).

`assert_causal(fn, market, decision)`: for ≥ 5 seeded cut points c, the output of `fn(market)` (a Series / DataFrame
on the session index) at sessions ≤ c is unchanged (atol 1e-12, NaN-equal) when every observation after the decision
time of c is replaced by an independent random walk, and also when the data are truncated there. For a `close`
decision that means rows > c; for `t1545` also session c's own close and full-session volume.

`assert_fill_after(gross, f, c, earned)`: the return a decision could not trade on (the overnight gap before a
`next_open` fill; the session itself before a `moc` fill) must not move with that decision's target."""

from dataclasses import replace

import numpy as np
import pandas as pd

from vwap_mr.backtest.market import Market
from vwap_mr.config import DecisionTime
from vwap_mr.data.dividends import adjustment_factor
from vwap_mr.data.exo import Exo

PRICE_COLUMNS = (
    "open",
    "close",
    "open_raw",
    "close_raw",
    "open_fallback_raw",
    "close_fallback_raw",
    "rth_vwap",
)
CLOSE_COLUMNS = ("close", "close_raw", "close_fallback_raw")
VOLUME_COLUMNS = ("volume", "rth_volume")  # rth_volume: the U6 variant weight
ADJUSTED_EXTRA = ("rth_vwap",)  # adjusted price levels not rebuilt from raw: rescaled with the factor


def _exo_cut(market: Market, c: int, decision: DecisionTime, rng, drop: bool) -> Exo | None:
    if market.exo is None:
        return None
    when = market.decision_instants(decision)[c]
    frames = {}
    for name, f in market.exo._frames.items():
        late = np.asarray(pd.DatetimeIndex(f["available_at"]) > when)
        g = f.copy()
        if drop:
            g = g[~late]
        else:
            g.loc[late, "value"] = g.loc[late, "value"].to_numpy() * np.exp(
                rng.normal(0, 0.2, int(late.sum()))
            )
        frames[name] = g
    return Exo(frames)


def _factor(s: pd.DataFrame) -> np.ndarray:
    return s["factor"].to_numpy() if "factor" in s else (s["close"] / s["close_raw"]).to_numpy()


def _rescale_snapshot(snap, old: pd.DataFrame, new: pd.DataFrame):
    """snap1545 prices sit on the sessions' adjusted scale: move them with the recomputed factor."""
    if snap is None:
        return None
    ratio = pd.Series(_factor(new) / _factor(old[: len(new)]), index=new.index).reindex(snap.index)
    out = snap.copy()
    for col in ("last", "vwap"):
        out[col] = out[col].to_numpy() * ratio.to_numpy()
    return out


def _readjust(s: pd.DataFrame, known: int) -> None:
    """Recompute the backward total-return factor (U1 convention) from the raw closes and dividends of rows < `known`
    only, as a vintage ending there would, and rewrite the adjusted `open` / `close`: the levels of rows <= c embed
    every later dividend, so a feature on adjusted levels (not ratios) reads the future unless this is redone."""
    raw = s["close_raw"].copy()
    if "close_fallback_raw" in s:
        raw = raw.fillna(s["close_fallback_raw"])
    head = s.iloc[:known]
    days = pd.DatetimeIndex(head.index).tz_convert("America/New_York").tz_localize(None)
    ex = head["dividend"].to_numpy() > 0
    divs = pd.DataFrame({"amount": head["dividend"].to_numpy()[ex]}, index=days[ex])
    factor = np.ones(len(s))
    factor[:known] = adjustment_factor(raw.iloc[:known], divs).to_numpy()
    old = _factor(s)
    for col in ADJUSTED_EXTRA:
        if col in s:
            s[col] = s[col].to_numpy() * factor / old
    for col in ("open", "close"):
        if col in s:
            s[col] = s[f"{col}_raw"].to_numpy() * factor
    if "factor" in s:
        s["factor"] = factor


def perturb_after(market: Market, c: int, decision: DecisionTime, seed: int = 0) -> Market:
    rng = np.random.default_rng(seed)
    s = market.sessions.copy()
    k = len(s) - (c + 1)
    shock = np.exp(np.cumsum(rng.normal(0, 0.01, k)))
    for col in PRICE_COLUMNS:
        if col in s:
            s.iloc[c + 1 :, s.columns.get_loc(col)] *= shock
    for col in VOLUME_COLUMNS:
        if col in s:
            s.iloc[c + 1 :, s.columns.get_loc(col)] = rng.uniform(1e6, 1e8, k)
    s.iloc[c + 1 :, s.columns.get_loc("dividend")] = np.where(
        rng.random(k) < 0.05, rng.uniform(0.2, 0.6, k), 0.0
    )
    if decision == DecisionTime.T1545:
        jolt = float(np.exp(rng.normal(0, 0.01)))
        for col in (*CLOSE_COLUMNS, *ADJUSTED_EXTRA):
            if col in s:
                s.iloc[c, s.columns.get_loc(col)] *= jolt
        for col in VOLUME_COLUMNS:
            if col in s:
                s.iloc[c, s.columns.get_loc(col)] = rng.uniform(1e6, 1e8)
    _readjust(s, len(s))
    snap = _rescale_snapshot(market.snapshot, market.sessions, s)
    if snap is not None:
        late = snap.index > market.index[c]
        for col in ("last", "vwap"):
            snap.loc[late, col] = snap.loc[late, col].to_numpy() * np.exp(
                rng.normal(0, 0.01, int(late.sum()))
            )
        snap.loc[late, "volume"] = rng.uniform(1e5, 1e7, int(late.sum()))
    state = market.state
    if state is not None:
        state = state.copy()
        # state row c is as of c's close decision, so a t1545 decision must not depend on it either
        late = (
            state.index > market.index[c]
            if decision == DecisionTime.CLOSE
            else state.index >= market.index[c]
        )
        state.loc[late] = rng.normal(size=(int(late.sum()), state.shape[1]))
    return replace(
        market, sessions=s, snapshot=snap, state=state, exo=_exo_cut(market, c, decision, rng, False)
    )


def truncate_after(market: Market, c: int, decision: DecisionTime) -> Market:
    t = market.index[c]
    s = market.sessions.iloc[: c + 1].copy()
    if decision == DecisionTime.T1545:
        for col in (*CLOSE_COLUMNS, *ADJUSTED_EXTRA, *VOLUME_COLUMNS):
            if col in s:
                s.iloc[c, s.columns.get_loc(col)] = np.nan
        s.iloc[c, s.columns.get_loc("dividend")] = 0.0  # its factor step needs close_c, unknown at 15:45
        _readjust(s, c)
        s.iloc[c, s.columns.get_loc("close")] = np.nan
    else:
        _readjust(s, c + 1)
    cut = (lambda f: None if f is None else f.loc[:t])  # fmt: skip
    state = None if market.state is None else market.state.loc[:t] if decision == DecisionTime.CLOSE else         market.state[market.state.index < t]  # fmt: skip
    snap = _rescale_snapshot(cut(market.snapshot), market.sessions, s)
    return replace(market, sessions=s, snapshot=snap, state=state,
                   exo=_exo_cut(market, c, decision, None, True))  # fmt: skip


def cut_points(n: int, seed: int, k: int = 5) -> list[int]:
    rng = np.random.default_rng(seed)
    return sorted(rng.choice(np.arange(n // 4, n - 2), size=k, replace=False).tolist())


def _compare(a, b, upto: int) -> None:
    x = np.asarray(a.iloc[:upto], dtype=float)
    y = np.asarray(b.iloc[:upto], dtype=float)
    np.testing.assert_allclose(x, y, rtol=0, atol=1e-12, equal_nan=True)


def assert_causal(fn, market: Market, decision: DecisionTime, *, seed: int = 0, k: int = 5,
                  truncate: bool = True) -> None:  # fmt: skip
    base = fn(market)
    for c in cut_points(len(market.index), seed, k):
        _compare(base, fn(perturb_after(market, c, decision, seed + c)), c + 1)
        if truncate:
            _compare(base, fn(truncate_after(market, c, decision)), c + 1)


def assert_fill_after(gross, f: np.ndarray, c: int, rows: slice) -> None:
    """`gross(f)` → per-session gross returns. Changing f_c must leave rows `rows` unchanged: the returns realised
    before f_c's fill price."""
    g = f.copy()
    g[c] = -f[c] if f[c] != 0 else 1.0
    np.testing.assert_allclose(gross(f)[rows], gross(g)[rows], rtol=0, atol=1e-15)
