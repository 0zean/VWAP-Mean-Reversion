"""`DecisionView` and the `Strategy` protocol (U3, PLAN.md §5, SPEC.md §2).

Contract: a strategy never sees a bar it could not have seen at its decision time; the engine builds the
view and knows fill prices, the strategy does not.

- `close` decision (16:25 ET): `sessions` = completed sessions ≤ t; `snapshot` None; fill `next_open`.
- `t1545` decision: `sessions` ≤ t−1; `snapshot` = session t's snap1545 row; fill `moc`.
- `state` rows ≤ t (row t is defined as the state at t's decision time); `exo` answers only at the decision instant.

A strategy may also implement `targets(market) -> pd.Series` (one target per session, the decision of that session)
for speed; `target_series(..., vectorized=False)` evaluates `target` per session through the views, and the two are
tested equal. The vectorized path is not protected by the view, so it is the one the causality harness tests.
"""

from collections.abc import Iterator
from typing import NamedTuple, Protocol, runtime_checkable

import numpy as np
import pandas as pd

from vwap_mr.backtest.market import Market
from vwap_mr.config import DecisionTime
from vwap_mr.data.exo import Exo

FILL_FOR = {DecisionTime.CLOSE: "next_open", DecisionTime.T1545: "moc"}


class ExoView:
    """Exo frozen at one decision instant: `asof(name)` cannot be asked about a later time."""

    def __init__(self, exo: Exo, when: pd.Timestamp):
        self._exo, self.when = exo, when

    def asof(self, name: str, **kw) -> float:
        return float(self._exo.asof(name, self.when, **kw)[0])


class DecisionView(NamedTuple):
    sessions: pd.DataFrame
    snapshot: pd.Series | None
    state: pd.DataFrame | None
    exo: ExoView | None
    when: pd.Timestamp  # the decision instant (tz-aware NY)


@runtime_checkable
class Strategy(Protocol):
    name: str
    decision: DecisionTime
    fill: str

    def target(self, view: DecisionView) -> float: ...


def views(market: Market, decision: DecisionTime) -> Iterator[DecisionView]:
    """The DecisionView of every session, in order."""
    when = market.decision_instants(decision)
    s, idx = market.sessions, market.index
    for i, t in enumerate(idx):
        if decision == DecisionTime.CLOSE:
            sess, snap = s.iloc[: i + 1], None
        else:
            if market.snapshot is None:
                raise ValueError("a t1545 decision needs Market.snapshot (snap1545)")
            sess = s.iloc[:i]
            snap = market.snapshot.loc[t] if t in market.snapshot.index else None
        state = None if market.state is None else market.state.loc[:t]
        exo = None if market.exo is None else ExoView(market.exo, when[i])
        yield DecisionView(sess, snap, state, exo, when[i])


def check_strategy(strategy) -> None:
    if not isinstance(strategy, Strategy):
        raise TypeError(f"{strategy!r} does not implement the Strategy protocol")
    if strategy.decision not in FILL_FOR:
        raise ValueError(f"{strategy.name}: unknown decision {strategy.decision!r}")
    if strategy.fill != FILL_FOR[strategy.decision]:
        raise ValueError(f"{strategy.name}: decision {strategy.decision} fills {FILL_FOR[strategy.decision]!r}, "
                         f"not {strategy.fill!r}")  # fmt: skip


def target_series(strategy, market: Market, *, vectorized: bool = True) -> pd.Series:
    """Target exposure per session (decided at that session), finite and in [−1, 1]; anything else raises."""
    if vectorized and hasattr(strategy, "targets"):
        out = pd.Series(strategy.targets(market), dtype=float)
        if not out.index.equals(market.index):
            raise ValueError(f"{strategy.name}.targets must return one value per session")
    else:
        out = pd.Series(
            [float(strategy.target(v)) for v in views(market, strategy.decision)], index=market.index
        )
    v = out.to_numpy()
    if not np.isfinite(v).all() or (np.abs(v) > 1 + 1e-12).any():
        bad = out.index[~np.isfinite(v) | (np.abs(v) > 1 + 1e-12)]
        raise ValueError(
            f"{strategy.name}: targets must be finite in [-1, 1]; bad at {[str(d.date()) for d in bad[:5]]}"
        )
    return out.rename(strategy.name)
