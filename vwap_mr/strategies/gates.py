"""Regime gates (U10, PLAN.md §6.4): the regime columns R1–R3, the rules that turn a state into a favourable set,
and the gated strategy.

Timing: every column row t is as of session t's close decision (`features.state`); R1 / R2 are the one-step-ahead
probabilities P(s_{t+1} | ≤ t) of `regime.hmm.rolling_regime` (never the filtered P(s_t)); R3 is online. A gate row
t is 1.0 (favourable), 0.0, or NaN ("no gate": a state not yet defined, never filled). The H3 statistic conditions
the return of row t + 1 on gate row t (`experiments.h3`); `Gated` trades the region's target × gate at the same
decision, flat where the gate is NaN.

Rules (`rule` in the spec's `gates` map; quantiles are `persistence.trailing_quantile`, trailing window from the
spec, today included):
- `q_above`: trailing quantile > threshold (B1: top two quintiles, threshold 0.6; B2: top tercile, 2/3);
- `q_at_or_below`: trailing quantile ≤ threshold (B4, E1–E4: below the median, 0.5);
- `below` / `above`: the raw state < / > threshold (B3 < 0, B5 < 1, B6 > 0, R1 / R2 P > 0.5);
- `stable`: R3's E[run length] > threshold and P(r_t ≤ 5) < `p_recent_max`."""

from dataclasses import dataclass

import numpy as np
import pandas as pd

from vwap_mr.config import DecisionTime
from vwap_mr.experiments.persistence import trailing_quantile
from vwap_mr.features.deviation import state_deviation
from vwap_mr.regime.bocpd import NIG, bocpd_avg
from vwap_mr.regime.hmm import rolling_regime, standardized
from vwap_mr.strategies.base import DecisionView
from vwap_mr.strategies.region import Region

RULES = ("q_above", "q_at_or_below", "below", "above", "stable")


def regime_frame(frame: pd.DataFrame, rc: dict, *, price: str = "close") -> tuple[pd.DataFrame, dict]:
    """R1 / R2 (and each R2 hyper-parameter variant in `rc['R2_variants']`) one-step-ahead probabilities and R3's
    stability inputs on `frame` (columns `price`, volume), with the refit logs. `rc` is the spec's
    `regime_configuration`."""
    r = frame[price].astype(float).pct_change()
    z = standardized(r, rc["std_window"])
    d = state_deviation(frame[price].astype(float), frame["volume"].astype(float), rc["R2"]["n"])
    cols, logs = {}, {}
    p, logs["R1"] = rolling_regime(z, kind="sigma", fit_window=rc["R1"]["fit_window"],
                                   refit_every=rc["R1"]["refit_every"])  # fmt: skip
    cols["R1_p"] = p["p_next"]
    for name, c in {"R2": rc["R2"], **rc.get("R2_variants", {})}.items():
        p, logs[name] = rolling_regime(d, pd.Series(d.shift(1)), kind="phi", fit_window=c["fit_window"],
                                       refit_every=c["refit_every"])  # fmt: skip
        cols[f"{name}_p"] = p["p_next"]
    ok = z.notna()
    first = int(np.argmax(ok.to_numpy()))
    if not ok.iloc[first:].all():
        raise ValueError("R3: a NaN return after the standardization warm-up (BOCPD cannot skip it)")
    b = bocpd_avg(z.iloc[first:].to_numpy(), NIG(**rc["R3"]["prior"]), tuple(rc["R3"]["hazards"]))
    for k in ("e_rl", "p_recent"):
        cols[f"R3_{k}"] = pd.Series(np.nan, index=frame.index)
        cols[f"R3_{k}"].iloc[first:] = b[k]
    return pd.DataFrame(cols, index=frame.index), logs


def gate(state: pd.DataFrame, g: dict, q_window: int) -> pd.Series:
    """One gate (a spec `gates` entry: column, rule, threshold[, p_recent_max]) as 1.0 / 0.0 / NaN."""
    rule = g["rule"]
    if rule not in RULES:
        raise ValueError(f"unknown gate rule {rule!r}")
    if rule == "stable":
        e, pr = state["R3_e_rl"], state["R3_p_recent"]
        return ((e > g["threshold"]) & (pr < g["p_recent_max"])).astype(float).where(e.notna() & pr.notna())
    x = state[g["column"]]
    if rule in ("q_above", "q_at_or_below"):
        x = trailing_quantile(x, q_window)
    on = x > g["threshold"] if rule in ("q_above", "above") else x <= g["threshold"] if rule == "q_at_or_below" \
        else x < g["threshold"]  # fmt: skip
    return on.astype(float).where(x.notna())


def gate_frame(state: pd.DataFrame, gates: dict[str, dict], q_window: int) -> pd.DataFrame:
    return pd.DataFrame({k: gate(state, g, q_window) for k, g in gates.items()}, index=state.index)


@dataclass(frozen=True)
class Gated:
    """The region traded only where its gate (the `Market.state` column `gate`) is 1; flat where 0 or NaN."""

    region: Region
    gate: str

    @property
    def name(self) -> str:
        return f"{self.region.name}|{self.gate}"

    @property
    def decision(self) -> DecisionTime:
        return self.region.decision

    @property
    def fill(self) -> str:
        return self.region.fill

    def target(self, view: DecisionView) -> float:
        assert view.state is not None
        g = view.state[self.gate].iloc[-1] if view.state.index[-1] == view.sessions.index[-1] else np.nan
        return float(self.region.target(view)) * (1.0 if g == 1.0 else 0.0)

    def targets(self, market) -> pd.Series:
        if market.state is None:
            raise ValueError(f"{self.name}: the market carries no gate state")
        g = market.state[self.gate].reindex(market.index)
        return self.region.targets(market) * (g == 1.0).astype(float)
