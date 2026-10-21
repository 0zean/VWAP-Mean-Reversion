"""State variables (U8, PLAN.md §6.4): the baselines B1–B6 and the Hurst-type estimators E1–E4.

Contract: row t is the state **as of session t's close decision** (it may use close_t and session t's volume; the
`DecisionView` hands a t1545 decision rows ≤ t−1 only, base.py). Trailing windows only; NaN until a window is full
(the `diag` warm-up of §4 fills them before 2016-01-04 when the long series is used, `long_state`). Returns are the
close-to-close total returns of the adjusted close. Estimators run on returns, never on d_t / d*_t (§8.1); B4 is the
AR(1) φ of d*_t, read against its random-walk reference (`regime.calibrate.rw_phi_null`), not against 0.

| col | §6.4 | definition (window in sessions) |
|---|---|---|
| B1_vol | B1 | sample σ of returns over 21 |
| B2_vix | B2 | VIX close as of the decision instant (`Exo.asof`) |
| B3_ac1 | B3 | lag-1 autocorrelation of returns over 63 |
| B4_phi | B4 | AR(1) φ of d*_t over 126 |
| B5_vr5 | B5 | Lo–MacKinlay VR(5) over 252 (overlapping 5-session sums; no small-sample adjustment) |
| B6_mom | B6 | the region's net P&L summed over 63 (needs the H1 stream; absent otherwise) |
| E1_dfa_std | E1 | DFA-1 α over 504, scales [10, 126] |
| E2_dfa_short | E2 | DFA-1 α over 504, scales [4, 32] |
| E3_av_short | E3 | Abry–Veitch H, causal MODWT j 1–4, from the last 512 returns (coefficients fully inside) |
| E4_h2, E4_dalpha | E4 | MF-DFA h(2) and Δα over 1,008, q −5 … 5, scales [10, 126] |

The quantile and median rules that turn a state into a gate (trailing 504) belong to U10's gates."""

from dataclasses import dataclass, replace

import pandas as pd

from vwap_mr.config import DecisionTime
from vwap_mr.features.deviation import state_deviation
from vwap_mr.regime.calibrate import ar1_phi
from vwap_mr.regime.hurst import rolling_dfa, rolling_mfdfa
from vwap_mr.regime.wavelet import rolling_av


@dataclass(frozen=True)
class StateConfig:
    vol: int = 21
    ac: int = 63
    phi: int = 126
    vr_q: int = 5
    vr: int = 252
    mom: int = 63
    dfa: int = 504
    av: int = 512
    mf: int = 1008


REGISTERED = StateConfig()


def realized_vol(r: pd.Series, window: int) -> pd.Series:
    return r.rolling(window).std(ddof=1)


def lag1_autocorr(r: pd.Series, window: int) -> pd.Series:
    return r.rolling(window).corr(r.shift(1))


def variance_ratio(r: pd.Series, q: int, window: int) -> pd.Series:
    """Var of the overlapping q-sums inside the window / (q · Var of single returns)."""
    return r.rolling(q).sum().rolling(window - q + 1).var() / (q * r.rolling(window).var())


def state_frame(sessions: pd.DataFrame, *, vix: pd.Series | None = None, net: pd.Series | None = None,
                cfg: StateConfig = REGISTERED) -> pd.DataFrame:  # fmt: skip
    """All state columns on `sessions` (columns close, volume) — see the module table."""
    close, vol = sessions["close"].astype(float), sessions["volume"].astype(float)
    r = close.pct_change()
    out = {
        "B1_vol": realized_vol(r, cfg.vol),
        "B3_ac1": lag1_autocorr(r, cfg.ac),
        "B4_phi": ar1_phi(state_deviation(close, vol, 5), cfg.phi),
        "B5_vr5": variance_ratio(r, cfg.vr_q, cfg.vr),
        "E1_dfa_std": rolling_dfa(r, cfg.dfa, "standard"),
        "E2_dfa_short": rolling_dfa(r, cfg.dfa, "short"),
        "E3_av_short": rolling_av(r, cfg.av, "short"),
    }
    mf = rolling_mfdfa(r, cfg.mf, "standard")
    out |= {"E4_h2": mf["mfdfa_h2"], "E4_dalpha": mf["mfdfa_dalpha"]}
    if vix is not None:
        out["B2_vix"] = vix.reindex(sessions.index)
    if net is not None:
        out["B6_mom"] = net.reindex(sessions.index).rolling(cfg.mom).sum()
    return pd.DataFrame(out, index=sessions.index)


def market_state(market, *, net: pd.Series | None = None, cfg: StateConfig = REGISTERED) -> pd.DataFrame:
    """`state_frame` on a Market, VIX read as of each session's close-decision instant."""
    vix = None
    if market.exo is not None and "VIX" in market.exo._frames:
        when = market.decision_instants(DecisionTime.CLOSE)
        vix = pd.Series(market.exo.asof("VIX", when), index=market.index)
    return state_frame(market.sessions, vix=vix, net=net, cfg=cfg)


def long_state(
    symbol: str, vintage: str, *, net: pd.Series | None = None, cfg: StateConfig = REGISTERED
) -> pd.DataFrame:
    """`state_frame` on `long_inputs` (below)."""
    frame, vix = long_inputs(symbol, vintage)
    return state_frame(frame, vix=vix, net=net, cfg=cfg)


def long_inputs(symbol: str, vintage: str) -> tuple[pd.DataFrame, pd.Series]:
    """The spliced long series (`load_long`: `diag` before 2016-01-04, Alpaca after) and VIX as of each session's
    close decision, so every state window is full on 2016-01-04. VIX timing: on sessions the vintage's exchange calendar covers (2015 on) VIX is read at the
    calendar's close-decision instant (early closes respected); before that there is no calendar, so it is read at
    13:00 on the session date, i.e. the previous session's close (one day stale, never a value an early-close
    decision could not have seen). B6 needs the region's net P&L (`net`, the H1 stream: development window only, so
    B6 has no `diag` warm-up and is NaN for its first 63 sessions)."""
    from vwap_mr.data.calendar import decision_times, get_calendar
    from vwap_mr.data.exo import Exo, load_series
    from vwap_mr.data.secondary import load_long
    from vwap_mr.data.store import vintage_dir

    frame = load_long(symbol, vintage)[0]
    days = frame.index
    naive = days.tz_localize(None)
    when = pd.Series(pd.DatetimeIndex(naive + pd.Timedelta(hours=13)).tz_localize(days.tz), index=days)
    cal = get_calendar(vintage_dir(vintage), naive[-1] + pd.Timedelta(days=1))
    covered = days[naive >= cal.index[0]]
    when.loc[covered] = decision_times(covered, cal, DecisionTime.CLOSE)
    vix_f = load_series("VIX", naive[0] - pd.Timedelta(days=10), "2026-01-01", vintage=vintage)
    vix = pd.Series(Exo({"VIX": vix_f}).asof("VIX", pd.DatetimeIndex(when.to_numpy())), index=days)
    return frame, vix


SMALL = replace(
    REGISTERED, vol=10, ac=20, phi=30, vr=40, mom=20, dfa=260, av=200, mf=280
)  # tests: short series
