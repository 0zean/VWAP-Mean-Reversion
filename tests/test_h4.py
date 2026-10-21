"""U11: the trend region, the vol-managed benchmark, the blend and its weights (causality at the decision time), the
H4 spec and runner. No network; synthetic markets."""

import copy
import json
from dataclasses import replace
from datetime import date
from pathlib import Path

import numpy as np
import pandas as pd
import pytest
import yaml
from causality import assert_causal

from vwap_mr.backtest.costs import ZERO
from vwap_mr.backtest.engine import run
from vwap_mr.backtest.metrics import summary
from vwap_mr.config import SPECS_DIR, DecisionTime
from vwap_mr.data.synthetic import daily_market
from vwap_mr.experiments import h3, h4
from vwap_mr.experiments.looks import looks
from vwap_mr.experiments.persistence import trailing_quantile
from vwap_mr.experiments.report import ReportRefused
from vwap_mr.experiments.runner import RunError
from vwap_mr.experiments.spec import SpecError, load_spec
from vwap_mr.features.state import realized_vol
from vwap_mr.stats.power import mde_delta_sharpe
from vwap_mr.strategies.base import target_series
from vwap_mr.strategies.blend import Blend, weight
from vwap_mr.strategies.gates import gate
from vwap_mr.strategies.trend import TSMOM_LOOKBACKS, donchian, trend_state, tsmom, vol_managed
from vwap_mr.strategies.vwap_mr import r0

CLOSE = DecisionTime.CLOSE
SPEC = SPECS_DIR / "H4_blend.yaml"
Q = 60


def spec_dict() -> dict:
    with open(SPEC, encoding="utf-8") as f:
        return yaml.safe_load(f)


def small_wc(wc: dict) -> dict:
    wc = copy.deepcopy(wc) | {"trailing_quantile_window": Q}
    wc["R2"] |= {"fit_window": 120, "refit_every": 20}
    return wc


# ── components ──────────────────────────────────────────────────────────────


def test_trend_state_and_donchian_by_hand():
    close = pd.Series([10.0, 11, 12, 11, 9, 8, 10, 13])
    st = trend_state(close, lookbacks=(2,), donchian=3)
    np.testing.assert_allclose(
        st["TF_2"].iloc[2:], close.iloc[2:].to_numpy() / close.iloc[:-2].to_numpy() - 1
    )
    # channel of the previous 3 closes: row 3 (11 inside [10, 12]) flat; 9 < 11 short; 8 short; 10 holds; 13 > 11 long
    assert st["DC_3"].iloc[:3].isna().all() and st["DC_3"].iloc[3:].tolist() == [0.0, -1.0, -1.0, -1.0, 1.0]
    with pytest.raises(ValueError, match="NaN close"):
        trend_state(close.where(close != 9))


def test_tsmom_vol_managed_and_nan_refusal():
    m = daily_market("SPY", "2016-01-04", "2016-12-30", 1)
    st = pd.DataFrame({"TF_21": 0.1, "TF_63": -0.2, "TF_126": 0.3, "TF_252": 0.0,
                       "B1_vol": np.r_[0.005, 0.02, np.zeros(len(m.index) - 2)]}, index=m.index)  # fmt: skip
    sm = replace(m, state=st)
    assert (target_series(tsmom(), sm) == 0.25).all()  # (1 − 1 + 1 + 0) / 4
    with pytest.raises(ValueError, match="finite"):  # σ̂ = 0: never a silent full exposure
        target_series(vol_managed(), sm)
    v = vol_managed().targets(sm)
    assert v.iloc[0] == 1.0 and v.iloc[1] == pytest.approx(0.15 / (0.02 * np.sqrt(252)))
    with pytest.raises(ValueError, match="finite"):
        target_series(tsmom(), replace(m, state=st.assign(TF_252=np.nan)))
    with pytest.raises(ValueError, match="lacks"):
        target_series(tsmom(), replace(m, state=st[["TF_21"]]))


def test_weight_bounds():
    p = pd.Series([0.0, 1.0, 0.3])
    assert weight(p, 0.5).tolist() == [0.25, 0.75, 0.4] and weight(p, 1.0).tolist() == [0.0, 1.0, 0.3]
    with pytest.raises(ValueError, match="κ"):
        weight(p, 1.5)
    with pytest.raises(ValueError, match="outside"):
        weight(pd.Series([1.2]), 0.5)


def blend_state(m, *, peek: bool = False, r2: bool = True) -> pd.DataFrame:
    """The blend's state on a market's own sessions, small windows (the runner reads the long series). `r2=False`
    builds B1 / B1_pct the way `weight_state` does without the MS fits (the target-level tests)."""
    wc = small_wc(spec_dict()["weight_configuration"])
    if r2:
        frame = m.sessions.assign(close=m.sessions["close_raw"])  # U9's convention for the MS fits (test_h3)
        st, _ = h4.weight_state(frame, wc)
        st = st.drop(columns=[c for c in st if c.startswith(("TF_", "DC_"))])
    else:
        st = pd.DataFrame({"B1_vol": realized_vol(m.sessions["close"].pct_change(), 21)})
        st["P_B1"] = gate(st, wc["B1"], Q)
        st["P_B1_pct"] = trailing_quantile(st["B1_vol"], Q)
    st = st.join(trend_state(m.sessions["close"]))
    for p in ("B1", "B1_pct", "R2")[: 3 if r2 else 2]:
        w = weight(st[f"P_{p}"], 0.5)
        st[f"w_{p}"] = w.shift(-1) if peek else w
    return st


def blend_targets(m, *, sigma=None, tf=tsmom, peek=False) -> pd.DataFrame:
    st = blend_state(m, peek=peek, r2=False).fillna({"w_B1": 0.5, "w_B1_pct": 0.5}).fillna(0.0)
    st["B1_vol"] = st["B1_vol"].replace(0.0, 0.01)
    sm = replace(m, state=st)
    return pd.DataFrame({p: target_series(Blend(r0(), tf(), f"w_{p}", sigma), sm) for p in ("B1", "B1_pct")})


@pytest.fixture(scope="module")
def mk():
    return daily_market("SPY", "2015-01-01", "2016-06-30", 3)


def test_blend_vectorized_equals_views_and_static_is_the_mean(mk):
    st = blend_state(mk, r2=False).fillna(0.0).assign(w_static=0.5)
    st["B1_vol"] = st["B1_vol"].replace(0.0, 0.01)
    sm = replace(mk, state=st)
    for b in (Blend(r0(), tsmom(), "w_B1"), Blend(r0(), donchian(), "w_B1_pct", 0.10)):
        np.testing.assert_allclose(target_series(b, sm), target_series(b, sm, vectorized=False), atol=1e-15)
    stat = target_series(Blend(r0(), tsmom(), "w_static"), sm)
    np.testing.assert_allclose(stat, 0.5 * (target_series(r0(), sm) + target_series(tsmom(), sm)), atol=1e-15)


# ── causality (done-when 2) ─────────────────────────────────────────────────


def test_blend_and_weights_are_causal_at_the_decision_time(mk):
    W = blend_state(mk)
    assert (W[["P_B1", "P_B1_pct", "P_R2"]].notna().sum() > 20).all(), (
        "each weight must exist for the test to bite"
    )
    assert_causal(lambda m: blend_state(m)[["P_B1", "P_B1_pct", "P_R2", "DC_55", "TF_21", "TF_252", "B1_vol"]],
                  mk, CLOSE, k=3)  # fmt: skip
    assert_causal(lambda m: blend_targets(m, sigma=0.10), mk, CLOSE, k=3)
    assert_causal(lambda m: blend_targets(m, tf=donchian), mk, CLOSE, k=3)


def test_b6_diff_weight_is_causal_given_causal_streams(mk):
    def b6(m):
        st = trend_state(m.sessions["close"])
        y = pd.Series(0.0, index=m.index)
        d = (run(m, r0(), costs=ZERO, cash_yield=y).net
             - run(replace(m, state=st.fillna(0.0)), tsmom(), costs=ZERO, cash_yield=y).net).rolling(20).sum()  # fmt: skip
        return (d > 0).astype(float).where(d.notna())

    assert_causal(b6, mk, CLOSE, k=3)


def test_harness_catches_a_weight_that_peeks_one_session(mk):
    with pytest.raises(AssertionError):
        assert_causal(lambda m: blend_targets(m, peek=True), mk, CLOSE, k=3)


def test_leg_turnover_is_the_engines_when_one_leg_is_empty():
    m = daily_market("SPY", "2016-01-04", "2016-12-30", 2)
    res = run(m, r0(), costs=ZERO, cash_yield=0.0)
    n = len(m.index)
    t = h4.leg_turnover(res, np.ones(n), r0().targets(m).to_numpy(), np.zeros(n), np.ones(n))
    assert (
        t["mr_leg"] == pytest.approx(summary(res)["turnover"])
        and t["tf_leg"] == 0
        and abs(t["netted"]) < 1e-9
    )


# ── the registered spec (done-when 1) ───────────────────────────────────────


def test_registered_spec_matches_plan_and_h3():
    s = load_spec(SPEC)
    h4._validate_h4(s)
    assert s["headlines"] == ["B1"] and len(s["variants"]) == 8 and s["budget"] == 9
    assert s["test"]["kind"] == "paired_sharpe" and s["power"]["diagnostic"] is True
    assert s["window"] == {"start": date(2016, 1, 4), "end": date(2025, 12, 31)}
    p = s["power"]
    assert p["mde_delta_sharpe"] == pytest.approx(
        mde_delta_sharpe(p["n"], p["rho"])["mde_delta_sharpe"], abs=1e-3
    )
    assert s["expected"]["delta_sharpe"] < p["mde_delta_sharpe"]
    assert s["configurations"]["B1"] == {"P": "B1", "kappa": 0.5, "tf": "TSMOM"}
    assert s["blend"]["tf"]["TSMOM"] == list(TSMOM_LOOKBACKS)
    h3s = load_spec(SPECS_DIR / "H3_regime_gates.yaml")
    b1, wb = h3s["gates"]["B1"], s["weight_configuration"]["B1"]
    assert (wb["column"], wb["rule"], wb["threshold"], wb["pi"]) == (
        b1["column"],
        b1["rule"],
        b1["threshold"],
        b1["pi"],
    )
    assert (
        s["weight_configuration"]["trailing_quantile_window"]
        == h3s["state_configuration"]["trailing_quantile_window"]
    )
    r2, r2h = s["weight_configuration"]["R2"], h3s["regime_configuration"]["R2"]
    assert all(r2[k] == r2h[k] for k in ("n", "fit_window", "refit_every"))


# ── the runner on synthetic data ────────────────────────────────────────────


def fast_spec(tmp_path, **conf) -> Path:
    d = spec_dict()
    d["test"]["n_boot"] = 99
    d["window"]["end"] = date(2016, 12, 30)
    d["weight_configuration"] = small_wc(d["weight_configuration"])
    for k, v in conf.items():
        d["configurations"][k] = v
    p = tmp_path / "H4_blend.yaml"
    p.write_text(yaml.safe_dump(d, sort_keys=False, allow_unicode=True), encoding="utf-8")
    return p


def dry(tmp_path, spec, inputs=None):
    return h4.run_h4(spec, inputs=inputs or (lambda s: h3.synthetic_inputs(s, start="2014-09-01")),
                     require_registered=False, ledger_path=tmp_path / "ledger.jsonl",
                     looks_path=tmp_path / "looks.jsonl", cells_dir=tmp_path / "cells")  # fmt: skip


@pytest.fixture(scope="module")
def dryrun(tmp_path_factory):
    tmp = tmp_path_factory.mktemp("h4")
    return tmp, dry(tmp, fast_spec(tmp))


def test_dry_run_records_one_look_then_one_row_per_configuration(dryrun):
    tmp, rec = dryrun
    (look,) = looks(tmp / "looks.jsonl")
    assert look["n"] == 12 and look["label"].startswith("U11 run H4_blend") and look["window"] == "dev"
    rows = [json.loads(x) for x in (tmp / "ledger.jsonl").read_text().splitlines()]
    assert len(rows) == 9 and sum(r["kind"] == "headline" for r in rows) == 1
    assert all(r["started_at"] == look["at"] for r in rows)
    assert set(rec["references"]) == {"R0", "TSMOM", "DC55", "vol_managed"}
    assert rec["configurations"]["B6_diff"]["n_undefined"] == 62


def test_headline_weight_is_b1s_gate_and_the_difference_is_the_tilt(dryrun):
    tmp, rec = dryrun
    o = rec["configurations"]["B1"]
    assert set(o["w_values"]) == {"0.25", "0.75"}
    spec = load_spec(tmp / "H4_blend.yaml")
    m, frame, _ = h3.synthetic_inputs(spec, start="2014-09-01")
    st, _ = h4.weight_state(frame, spec["weight_configuration"])
    daily = pd.read_parquet(tmp / "cells" / o["cell_hash"] / "daily.parquet")
    np.testing.assert_array_equal(daily["w"], weight(st["P_B1"], 0.5).reindex(m.index).iloc[1:])
    hs = rec["configurations"]["hard_switch"]
    assert set(hs["w_values"]) == {"0", "1"} and hs["rho"] < o["rho"]
    assert o["d_sharpe_ann"] == pytest.approx(o["blend"]["sharpe"] - o["static"]["sharpe"], abs=0.02)
    # the matched-mean static: E[w] at the registered π for B1 weights, the realized mean otherwise
    assert o["matched_mean"]["w"] == pytest.approx(0.45) and hs["matched_mean"]["w"] == pytest.approx(0.4)
    r2 = rec["configurations"]["R2"]
    assert r2["matched_mean"]["source"] == "realized mean w" and r2["matched_mean"]["w"] == pytest.approx(
        r2["w_mean"]
    )
    assert o["matched_mean"]["d_sharpe_ann"] == pytest.approx(
        o["blend"]["sharpe"] - o["matched_mean"]["sharpe"], abs=0.02
    )
    assert o["floor"] == (o["net_ann"] >= o["net_ann_static"] - 0.1 * abs(o["net_ann_static"]))


def test_runner_refuses_a_second_run_undefined_weights_scratch_breaches_and_real_data(dryrun, tmp_path):
    tmp, _ = dryrun
    with pytest.raises(RunError, match="already run"):
        dry(tmp, tmp / "H4_blend.yaml")
    with pytest.raises(RunError, match="scratch"):
        h4.run_h4(fast_spec(tmp_path), require_registered=False)
    real = lambda s: (replace(daily_market("SPY", "2016-01-04", "2016-12-30", 1), source="diag"), None, None)
    with pytest.raises(RunError, match="synthetic"):
        dry(tmp_path, fast_spec(tmp_path), inputs=real)
    assert len(looks(tmp_path / "looks.jsonl")) == 0  # refused before the look
    with pytest.raises(SpecError, match="undefined"):  # B6_diff's undefined head is known before the look
        dry(tmp_path, fast_spec(tmp_path, B6_diff={"P": "B6_diff", "kappa": 0.5, "tf": "TSMOM"}))
    assert len(looks(tmp_path / "looks.jsonl")) == 0
    with pytest.raises(SpecError, match="configuration"):
        dry(tmp_path / "x", fast_spec(tmp_path, R2={"P": "R4", "kappa": 0.5, "tf": "TSMOM"}))


def test_report_refuses_without_the_look_and_renders_with_it(dryrun, tmp_path):
    tmp, rec = dryrun
    with pytest.raises(ReportRefused):
        h4.render_h4(rec, tmp_path / "empty.jsonl")
    md = h4.render_h4(rec, tmp / "looks.jsonl")
    assert "K = 12" in md and "Verdict" in md and "correlation by year" in md and "swap-flagged" in md
