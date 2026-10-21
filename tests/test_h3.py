"""U10: gate rules, the gated strategy, causality of every gate at the decision time, the H3 spec and runner. No network;
synthetic markets."""

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

from vwap_mr.config import SPECS_DIR, DecisionTime
from vwap_mr.data.synthetic import daily_market
from vwap_mr.experiments import h3
from vwap_mr.experiments.looks import looks
from vwap_mr.experiments.report import ReportRefused
from vwap_mr.experiments.runner import RunError
from vwap_mr.experiments.spec import SpecError, load_spec
from vwap_mr.features.state import SMALL, state_frame
from vwap_mr.stats.power import mde_cond_diff
from vwap_mr.strategies.base import target_series
from vwap_mr.strategies.gates import Gated, gate, gate_frame, regime_frame
from vwap_mr.strategies.vwap_mr import r0

CLOSE = DecisionTime.CLOSE
SPEC = SPECS_DIR / "H3_regime_gates.yaml"
Q = 60  # trailing-quantile window in the small configuration


def spec_dict() -> dict:
    with open(SPEC, encoding="utf-8") as f:
        return yaml.safe_load(f)


def small_rc(rc: dict) -> dict:
    rc = copy.deepcopy(rc) | {"std_window": 40}
    for k in ("R1", "R2"):
        rc[k] |= {"fit_window": 120, "refit_every": 20}
    rc["R2_variants"] = {"R2_refit63": {"fit_window": 120, "refit_every": 40},
                         "R2_fit756": {"fit_window": 100, "refit_every": 20},
                         "R2_fit1512": {"fit_window": 160, "refit_every": 20}}  # fmt: skip
    return rc


def all_gates(m, *, peek: bool = False, regimes: bool = True) -> pd.DataFrame:
    """Every registered gate on a market, small windows; regimes on raw closes (dividend re-adjustment moves adjusted
    levels by ~1e-16, which the MS optimizer amplifies past the harness tolerance: U9's convention)."""
    s = m.sessions
    vix = pd.Series(m.exo.asof("VIX", m.decision_instants(CLOSE)), index=m.index)
    st = state_frame(s, vix=vix, cfg=SMALL)
    gates = {k: v for k, v in spec_dict()["gates"].items() if k != "B6"}
    if peek:
        st = st.shift(-1)
    if not regimes:
        return gate_frame(st, {k: v for k, v in gates.items() if not k.startswith("R")}, Q)
    reg, _ = regime_frame(s, small_rc(spec_dict()["regime_configuration"]), price="close_raw")
    return gate_frame(st.join(reg), gates, Q)


# ── rules ───────────────────────────────────────────────────────────────────


def test_gate_rules_by_hand():
    st = pd.DataFrame({"x": [np.nan, -1.0, 0.0, 2.0], "R3_e_rl": [np.nan, 30, 30, 10],
                       "R3_p_recent": [0.1, 0.1, 0.5, 0.1]})  # fmt: skip
    g = lambda rule, th, **kw: gate(st, {"column": "x", "rule": rule, "threshold": th, **kw}, 2).tolist()
    assert g("below", 0.0)[1:] == [1.0, 0.0, 0.0] and np.isnan(g("below", 0.0)[0])
    assert g("above", 0.0)[1:] == [0.0, 0.0, 1.0]
    assert g("stable", 21, p_recent_max=0.2)[1:] == [1.0, 0.0, 0.0]  # E[run] > 21 and P(r ≤ 5) < 0.2
    # trailing quantile over 2 (today included): rows 2, 3 rank 1.0 (top); row 1's window holds a NaN
    assert g("q_above", 0.6)[2:] == [1.0, 1.0] and np.isnan(g("q_above", 0.6)[1])
    assert g("q_at_or_below", 0.5)[2:] == [0.0, 0.0]
    with pytest.raises(ValueError, match="unknown gate rule"):
        g("between", 0.0)


def test_trailing_quantile_rule_hits_its_registered_share_on_noise():
    x = pd.Series(np.random.default_rng(0).standard_normal(6000))
    for rule, th, pi in (("q_above", 0.6, 0.4), ("q_above", 0.6667, 0.33), ("q_at_or_below", 0.5, 0.5)):
        on = gate(pd.DataFrame({"x": x}), {"column": "x", "rule": rule, "threshold": th}, 504).dropna()
        assert abs(on.mean() - pi) < 0.02


def test_gated_vectorized_equals_views_and_is_flat_off_gate():
    m = daily_market("SPY", "2016-01-04", "2016-12-30", 1)
    rng = np.random.default_rng(1)
    g = pd.Series(rng.choice([0.0, 1.0, np.nan], len(m.index)), index=m.index)
    gm = replace(m, state=pd.DataFrame({"g": g}))
    s = Gated(r0(), "g")
    vec, loop = target_series(s, gm), target_series(s, gm, vectorized=False)
    np.testing.assert_allclose(vec, loop, atol=1e-15)
    assert (vec[g != 1.0] == 0).all() and np.allclose(vec[g == 1.0], target_series(r0(), m)[g == 1.0])
    with pytest.raises(ValueError, match="no gate state"):
        s.targets(m)


# ── causality (done-when 3) ─────────────────────────────────────────────────


@pytest.fixture(scope="module")
def mk():
    return daily_market("SPY", "2015-01-01", "2016-06-30", 3)


def test_every_gate_is_causal_at_the_decision_time(mk):
    G = all_gates(mk)
    assert set(G.columns) >= {"B1", "B4", "B5", "R2", "R1", "R3", "E1", "E4", "R2_fit1512"}
    assert (G.notna().sum() > 20).all(), "each gate must exist on part of the sample for the test to bite"
    assert_causal(all_gates, mk, CLOSE, k=3)


def test_b6_is_causal_given_a_causal_stream(mk):
    from vwap_mr.backtest.costs import ZERO
    from vwap_mr.backtest.engine import run

    def b6(m):
        net = run(m, r0(), costs=ZERO, cash_yield=pd.Series(0.0, index=m.index)).net
        st = state_frame(m.sessions, net=net, cfg=SMALL)
        return gate(st, spec_dict()["gates"]["B6"], Q)

    assert_causal(b6, mk, CLOSE, k=3)


def test_harness_catches_a_gate_that_peeks_one_session(mk):
    with pytest.raises(AssertionError):
        assert_causal(lambda m: all_gates(m, peek=True, regimes=False), mk, CLOSE, k=3)


def test_gated_strategy_is_causal(mk):
    def targets(m):
        return target_series(Gated(r0(), "B1"), replace(m, state=all_gates(m, regimes=False)[["B1"]]))

    assert_causal(targets, mk, CLOSE, k=3)


# ── the registered spec (done-when 1) ───────────────────────────────────────


def test_registered_spec_program_power_and_gate_columns(mk):
    s = load_spec(SPEC)
    h3._validate_h3(s)
    assert s["headlines"] == ["B1", "B4", "B5", "R2"] and len(s["variants"]) == 12 and s["budget"] == 16
    assert s["test"]["kind"] == "cond_mean_diff" and s["power"]["diagnostic"] is True
    assert s["window"] == {"start": date(2016, 1, 4), "end": date(2025, 12, 31)}
    assert s["expected"]["magnitude_bp_day"] < min(s["power"]["mde_by_headline"].values())
    for name, mde in s["power"]["mde_by_headline"].items():
        pi = s["gates"][name]["pi"] or 0.5
        assert mde == pytest.approx(mde_cond_diff(2513, 92.4e-4, pi, k=4)["mde_bp_day"], abs=0.01)
    for e in ("E1", "E2", "E3", "E4"):
        assert s["gates"][e]["under_powered"] is True
    # every gate column exists in the state / regime frames the runner builds
    st = state_frame(mk.sessions, vix=pd.Series(1.0, index=mk.index), net=pd.Series(0.0, index=mk.index),
                     cfg=SMALL)  # fmt: skip
    reg, _ = regime_frame(mk.sessions, small_rc(s["regime_configuration"]))
    cols = set(st.columns) | set(reg.columns)
    assert all(g["column"] in cols for g in s["gates"].values())
    # the frozen U9 configuration is unchanged
    rc = s["regime_configuration"]
    assert (rc["R2"]["fit_window"], rc["R2"]["refit_every"], rc["R3"]["hazards"]) == (
        1008,
        21,
        [63, 126, 252],
    )
    assert rc["R3"]["prior"]["beta0"] == pytest.approx(2.362069868704949)


def test_validator_refuses_missing_gate_entries_and_wrong_kind(tmp_path):
    d = spec_dict()
    del d["gates"]["R2"]
    p = tmp_path / "H3_regime_gates.yaml"
    p.write_text(yaml.safe_dump(d, allow_unicode=True), encoding="utf-8")
    with pytest.raises(SpecError, match="gates"):
        h3.run_h3(p, require_registered=False, ledger_path=tmp_path / "l", looks_path=tmp_path / "k",
                  cells_dir=tmp_path / "c")  # fmt: skip
    with pytest.raises(SpecError, match="cond_mean_diff"):
        h3.run_h3(SPECS_DIR / "H1_vwap_mr.yaml", require_registered=False, ledger_path=tmp_path / "l",
                  looks_path=tmp_path / "k", cells_dir=tmp_path / "c")  # fmt: skip


# ── the runner on synthetic data ────────────────────────────────────────────


def fast_spec(tmp_path, **gates) -> Path:
    d = spec_dict()
    d["test"]["n_boot"] = 99
    d["window"]["end"] = date(2016, 12, 30)
    d["state_configuration"]["trailing_quantile_window"] = Q
    d["regime_configuration"] = small_rc(d["regime_configuration"])
    for k, v in gates.items():
        d["gates"][k] |= v
    p = tmp_path / "H3_regime_gates.yaml"
    p.write_text(yaml.safe_dump(d, sort_keys=False, allow_unicode=True), encoding="utf-8")
    return p


def dry(tmp_path, spec, inputs=None):
    return h3.run_h3(spec, inputs=inputs or (lambda s: h3.synthetic_inputs(s, start="2014-09-01")),
                     require_registered=False, ledger_path=tmp_path / "ledger.jsonl",
                     looks_path=tmp_path / "looks.jsonl", cells_dir=tmp_path / "cells", state_cfg=SMALL)  # fmt: skip


@pytest.fixture(scope="module")
def run(tmp_path_factory):
    tmp = tmp_path_factory.mktemp("h3")
    # B5 made always-on: an untestable headline must stay in the Holm step at p = 1
    rec = dry(tmp, fast_spec(tmp, B5={"threshold": 100.0}))
    return tmp, rec


def test_dry_run_records_one_look_then_one_row_per_gate(run):
    tmp, rec = run
    (look,) = looks(tmp / "looks.jsonl")
    assert look["n"] == 16 and look["label"].startswith("U10 run H3_regime_gates") and look["window"] == "dev"
    rows = [json.loads(x) for x in (tmp / "ledger.jsonl").read_text().splitlines()]
    assert len(rows) == 16 and {r["kind"] for r in rows} == {"headline", "variant"}
    assert sum(r["kind"] == "headline" for r in rows) == 4
    assert all(r["started_at"] == look["at"] for r in rows)
    assert set(rec["refits"]) == {"R1", "R2", "R2_refit63", "R2_fit756", "R2_fit1512"}
    assert {i["gate"] for i in rec["incremental"]} == set(spec_dict()["incremental"]["gates"])


def test_untestable_headline_keeps_p_one_in_holm(run):
    _, rec = run
    b5 = rec["gates"]["B5"]
    assert b5["status"] == "no_fit" and b5["untestable"] and not b5["passes"] and b5["p"] is None
    tested = [rec["gates"][h]["p"] for h in ("B1", "B4", "R2")]
    assert b5["p_holm"] == 1.0  # m stays 4: the smallest adjusted p is 4 × the smallest raw p
    assert min(rec["gates"][h]["p_holm"] for h in ("B1", "B4", "R2")) == pytest.approx(
        min(1.0, 4 * min(tested))
    )


def test_return_row_is_conditioned_on_the_previous_decisions_gate(run):
    tmp, rec = run
    spec = load_spec(tmp / "H3_regime_gates.yaml")
    m, frame, vix = h3.synthetic_inputs(spec, start="2014-09-01")
    st = state_frame(frame, vix=vix, cfg=SMALL)
    G = gate(st, spec["gates"]["B1"], Q).reindex(m.index)
    daily = pd.read_parquet(tmp / "cells" / rec["gates"]["B1"]["cell_hash"] / "daily.parquet")
    np.testing.assert_array_equal(daily["gate_prev"].to_numpy(), G.shift(1).iloc[1:].to_numpy())
    on = daily["gate_prev"] == 1.0
    assert rec["gates"]["B1"]["delta_bp_day"] == pytest.approx(
        (daily["r"][on].mean() - daily["r"][~on].mean()) * 1e4, abs=1e-9
    )


def test_runner_refuses_a_second_run_scratch_breaches_and_real_data(run, tmp_path):
    tmp, _ = run
    with pytest.raises(RunError, match="already run"):
        dry(tmp, tmp / "H3_regime_gates.yaml")
    with pytest.raises(RunError, match="scratch"):
        h3.run_h3(fast_spec(tmp_path), require_registered=False)
    real = lambda s: (replace(daily_market("SPY", "2016-01-04", "2016-12-30", 1), source="diag"), None, None)
    with pytest.raises(RunError, match="synthetic"):
        dry(tmp_path, fast_spec(tmp_path), inputs=real)
    assert len(looks(tmp_path / "looks.jsonl")) == 0  # refused before the look


def test_report_refuses_without_the_look_and_renders_with_it(run, tmp_path):
    tmp, rec = run
    with pytest.raises(ReportRefused):
        h3.render_h3(rec, tmp_path / "empty.jsonl")
    md = h3.render_h3(rec, tmp / "looks.jsonl")
    assert "Holm" in md and "Untestable headlines" in md and "K = 16" in md and "degenerate" in md


def test_refit_summary_keeps_the_refit_covering_the_window_start():
    idx = pd.date_range("2015-12-01", periods=60, freq="B", tz="America/New_York")
    log = [{"session": s, "converged": s != 40, "stat": [0.3, 0.32 if s == 40 else 0.8], "swap_flag": s == 40,
            "degenerate": s == 20} for s in (0, 20, 40)] + [{"session": 50, "skipped": "NaN in the fit window"}]  # fmt: skip
    out = h3.refit_summary(log, idx, date(2016, 1, 4))  # position 23: the refit at 20 covers it, 0 does not
    assert (
        out["refits"] == 2 and out["degenerate"] == 1 and out["swap_flag"] == 1 and out["not_converged"] == 1
    )
    assert out["skipped"] == 1 and out["sep_median"] == pytest.approx((0.5 + 0.02) / 2)


def test_never_on_gate_reports_no_economic_numbers(tmp_path):
    rec = dry(tmp_path, fast_spec(tmp_path, B5={"threshold": -100.0}))
    g = rec["gates"]["B5"]
    assert g["status"] == "no_fit" and g["gated"]["flat_share"] == 1.0
    assert (
        g["gated"]["sharpe"] is None
        and "d_sharpe_ann" not in g["gated"]
        and "never on" in g["gated"]["d_sharpe_note"]
    )


def test_validator_refuses_unknown_rule_before_the_look_and_binds_estimator_windows(tmp_path):
    d = spec_dict()
    d["gates"]["B1"]["rule"] = "between"
    p = tmp_path / "H3_regime_gates.yaml"
    p.write_text(yaml.safe_dump(d, allow_unicode=True), encoding="utf-8")
    with pytest.raises(SpecError, match="rule"):
        dry(tmp_path, p)
    assert not (tmp_path / "looks.jsonl").exists()
    s = load_spec(SPEC)
    h3._check_windows(s, h3.REGISTERED)
    with pytest.raises(SpecError, match="estimator windows"):
        h3._check_windows(s, replace(h3.REGISTERED, mf=504))
