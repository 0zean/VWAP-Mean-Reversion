"""U6: the H1 spec, the variants, the runner's refusals, the look gate on the report. No network; synthetic markets."""

import copy
import json
import shutil
import subprocess
from dataclasses import dataclass, replace
from datetime import date
from pathlib import Path

import numpy as np
import pandas as pd
import pytest
import yaml
from causality import assert_causal, assert_fill_after
from test_backtest import _gross_fn

from vwap_mr.backtest.costs import ZERO
from vwap_mr.backtest.engine import legs, run
from vwap_mr.backtest.market import Market
from vwap_mr.config import SPECS_DIR, DecisionTime
from vwap_mr.data.synthetic import daily_market
from vwap_mr.experiments import runner as rn
from vwap_mr.experiments.looks import check_window, looks, record_look
from vwap_mr.experiments.report import ReportRefused, render
from vwap_mr.experiments.spec import SpecError, check_clean, load_spec, registered_sha, validate
from vwap_mr.strategies.base import target_series
from vwap_mr.strategies.region import Region
from vwap_mr.strategies.vwap_mr import VARIANTS, VolTarget, VwapCell, r0, r0_cells, vol_scale

CLOSE = DecisionTime.CLOSE
SPEC = SPECS_DIR / "H1_vwap_mr.yaml"
SEEDS = {"SPY": 1, "QQQ": 2, "IWM": 3, "DIA": 4}


def market(n_end: str = "2016-12-30", seed: int = 1, symbol: str = "SPY") -> Market:
    return daily_market(symbol, "2016-01-04", n_end, seed)


def spec_dict() -> dict:
    with open(SPEC, encoding="utf-8") as f:
        return yaml.safe_load(f)


def fast_spec(tmp_path, **test) -> Path:
    """The registered spec with fewer resamples, written under tmp_path (same id, so the same file name)."""
    d = spec_dict()
    d["test"] |= {"n_boot": 199} | test
    p = tmp_path / "H1_vwap_mr.yaml"
    p.write_text(yaml.safe_dump(d, sort_keys=False), encoding="utf-8")
    return p


def synthetic_loader(sym, spec, rth):
    return market("2017-06-30", SEEDS[sym], sym)


def dry_run(tmp_path, *, loader=synthetic_loader, spec=None, looks_path=None):
    return rn.run_spec(spec or fast_spec(tmp_path), require_registered=False, loader=loader,
                       ledger_path=tmp_path / "ledger.jsonl", looks_path=looks_path or tmp_path / "looks.jsonl",
                       cells_dir=tmp_path / "cells")  # fmt: skip


# ── the spec ────────────────────────────────────────────────────────────────


def test_registered_spec_carries_u5_magnitude_mde_and_diagnostic_flag():
    s = load_spec(SPEC)
    assert s["expected"]["magnitude_bp_day"] == 2.3 and "U5" in s["expected"]["source"]
    assert s["power"] == s["power"] | {"diagnostic": True, "mde_bp_day": 4.58, "n": 2513, "sigma_d_bp": 92.4}
    assert s["execution"] == {"decision": "close", "fill": "next_open"}
    assert s["headlines"] == ["R0"] and len(s["variants"]) == 7 and s["budget"] == 8
    assert "moc" not in " ".join(s["variants"])  # §9.2 / §9.3 at their defaults
    assert s["window"] == {"start": date(2016, 1, 4), "end": date(2025, 12, 31)}


@pytest.mark.parametrize(
    "mutate, msg",
    [
        (lambda d: d.pop("expected"), "lacks"),
        (lambda d: d.update(budget=7), "over budget"),
        (lambda d: d["variants"].extend(["x1", "x2"]), "at most 8"),
        (lambda d: d["window"].update(end=date(2026, 3, 1)), "development window"),
        (lambda d: d["execution"].update(fill="moc"), "fills"),
        (lambda d: d["power"].pop("diagnostic"), "diagnostic"),
        (lambda d: d["test"].update(alpha=0.2), "alpha < promising_alpha"),
        (lambda d: d["cost"].update(curve=[0.3, 2.3]), "registered_bp"),
        (lambda d: d.update(headlines=["R0", "R0b"]), "several headlines"),
        (lambda d: d["variants"].append("rth_volume"), "unique"),
    ],
)
def test_spec_validation_refuses(mutate, msg):
    d = spec_dict()
    mutate(d)
    with pytest.raises(SpecError, match=msg):
        validate(d, "H1_vwap_mr")
    with pytest.raises(SpecError, match="file name"):
        validate(spec_dict(), "H1_other")


def _git(repo, *args):
    subprocess.run(["git", *args], cwd=repo, check=True, capture_output=True)


@pytest.fixture
def repo(tmp_path):
    r = tmp_path / "repo"
    (r / "specs").mkdir(parents=True)
    _git(r, "init", "-q")
    _git(r, "config", "user.email", "t@t")
    _git(r, "config", "user.name", "t")
    (r / "README").write_text("x")
    _git(r, "add", ".")
    _git(r, "commit", "-qm", "init")
    return r


def test_registered_sha_refuses_untracked_modified_and_staged(repo):
    p = repo / "specs" / "H1_vwap_mr.yaml"
    shutil.copyfile(SPEC, p)
    with pytest.raises(SpecError, match="not committed"):
        registered_sha(p, repo)
    with pytest.raises(SpecError, match="uncommitted"):
        check_clean(repo, ("specs",))
    _git(repo, "add", ".")
    with pytest.raises(SpecError, match="uncommitted changes"):
        registered_sha(p, repo)  # staged, not committed: ls-files finds it, status refuses it
    _git(repo, "commit", "-qm", "spec")
    sha = registered_sha(p, repo)
    assert len(sha) == 40 and check_clean(repo, ("specs",)) == sha
    p.write_text(p.read_text() + "\n# edit\n")
    with pytest.raises(SpecError, match="uncommitted changes"):
        registered_sha(p, repo)


def test_runner_refuses_an_uncommitted_spec_before_any_look(repo, tmp_path):
    p = repo / "specs" / "H1_vwap_mr.yaml"
    shutil.copyfile(SPEC, p)
    with pytest.raises(SpecError, match="not committed"):
        rn.run_spec(p, repo=repo, loader=synthetic_loader, ledger_path=tmp_path / "l.jsonl",
                    looks_path=tmp_path / "k.jsonl", cells_dir=tmp_path / "c")  # fmt: skip
    assert not (tmp_path / "k.jsonl").exists()


def test_runner_refuses_dirty_code_with_a_committed_spec(repo, tmp_path):
    p = repo / "specs" / "H1_vwap_mr.yaml"
    shutil.copyfile(SPEC, p)
    _git(repo, "add", ".")
    _git(repo, "commit", "-qm", "spec")
    (repo / "scripts").mkdir()
    (repo / "scripts" / "new.py").write_text("x = 1\n")
    with pytest.raises(SpecError, match="uncommitted changes"):
        rn.run_spec(p, repo=repo, loader=synthetic_loader, ledger_path=tmp_path / "l.jsonl",
                    looks_path=tmp_path / "k.jsonl", cells_dir=tmp_path / "c")  # fmt: skip


# ── the runner ──────────────────────────────────────────────────────────────


def test_dry_run_records_one_look_first_and_one_ledger_row_per_cell(tmp_path):
    rec = dry_run(tmp_path)
    (look,) = looks(tmp_path / "looks.jsonl")
    assert look["n"] == 1 + 7 + 3 + 16 and look["window"] == "dev" and rec["look"] == look
    rows = [json.loads(x) for x in (tmp_path / "ledger.jsonl").read_text().splitlines()]
    assert len(rows) == 27 and {r["kind"] for r in rows} == {"headline", "variant", "split"}
    assert all(
        r["spec_id"] == "H1_vwap_mr" and r["status"] == "ok" and r["started_at"] == look["at"] for r in rows
    )
    assert len({r["cell_hash"] for r in rows}) == 27
    assert all((tmp_path / "cells" / r["cell_hash"] / "daily.parquet").exists() for r in rows)
    assert rec["verdict"] in ("confirmed", "promising", "rejected") and len(rec["cells"]) == 16
    assert [c["bp"] for c in rec["cost_curve"]] == [0.0, 0.3, 1.0, 2.3]
    assert rec["coherence"] == sum(c["mean_bp_day"] > 0 for c in rec["cells"])


def test_a_spec_runs_once(tmp_path):
    dry_run(tmp_path)
    with pytest.raises(rn.RunError, match="already run"):
        dry_run(tmp_path)


def test_a_crash_after_the_look_leaves_the_look_and_no_trial_rows(tmp_path):
    def loader(sym, spec, rth):
        m = synthetic_loader(sym, spec, rth)
        return m if sym == "SPY" else replace(m, exo=None)  # tbill_yield raises at the first split instrument

    with pytest.raises(ValueError, match="DTB3"):
        dry_run(tmp_path, loader=loader)
    assert len(looks(tmp_path / "looks.jsonl")) == 1
    assert not (tmp_path / "ledger.jsonl").exists()


def test_unregistered_runs_read_synthetic_markets_only(tmp_path):
    def loader(sym, spec, rth):
        m = synthetic_loader(sym, spec, rth)
        s = m.sessions.copy()
        s.attrs["source"] = "alpaca"
        return replace(m, sessions=s, source="alpaca")

    with pytest.raises(rn.RunError, match="synthetic"):
        dry_run(tmp_path, loader=loader)
    assert not (tmp_path / "looks.jsonl").exists()


def test_sessions_outside_the_dev_window_are_refused_before_the_look(tmp_path):
    with pytest.raises(ValueError, match="outside its window"):
        dry_run(tmp_path, loader=lambda sym, spec, rth: daily_market(sym, "2025-06-02", "2026-01-30", 1))
    assert not (tmp_path / "looks.jsonl").exists()


def test_runner_refuses_names_outside_the_registry_and_other_test_kinds(tmp_path):
    d = spec_dict()
    d["variants"][0] = "not_a_variant"
    p = tmp_path / "H1_vwap_mr.yaml"
    p.write_text(yaml.safe_dump(d), encoding="utf-8")
    with pytest.raises(SpecError, match="registry"):
        dry_run(tmp_path, spec=p)
    d = spec_dict()
    d["test"]["kind"] = "paired_sharpe"
    p.write_text(yaml.safe_dump(d), encoding="utf-8")
    with pytest.raises(SpecError, match="mean_vs_zero"):
        dry_run(tmp_path, spec=p)


def test_check_window():
    dev = pd.bdate_range("2016-01-04", "2016-02-01").tz_localize("America/New_York")
    check_window(dev, "dev")
    with pytest.raises(ValueError):
        check_window(dev, "diag")
    check_window(pd.bdate_range("2015-01-02", "2015-12-31"), "diag")
    with pytest.raises(ValueError):
        check_window(pd.bdate_range("2015-12-01", "2016-01-05"), "diag")
    with pytest.raises(ValueError):
        check_window(pd.bdate_range("2025-12-01", "2026-01-02"), "dev")


def test_report_refuses_a_run_whose_look_is_not_recorded(tmp_path):
    rec = dry_run(tmp_path)
    md = render(rec, tmp_path / "looks.jsonl")
    assert "K = 27" in md and "Registered as a diagnostic" in md and "p·K" in md
    other = tmp_path / "other.jsonl"
    record_look("something else", 1, path=other)
    with pytest.raises(ReportRefused):
        render(rec, other)
    forged = copy.deepcopy(rec)
    forged["look"]["at"] = "2000-01-01T00:00:00+00:00"
    with pytest.raises(ReportRefused):
        render(forged, tmp_path / "looks.jsonl")


@pytest.mark.parametrize(
    "p, ok, want",
    [(0.01, True, "confirmed"), (0.07, True, "promising"), (0.2, True, "rejected"), (0.01, False, "rejected"),
     (0.0499, True, "confirmed"), (0.05, True, "promising"), (0.10, True, "rejected")],
)  # fmt: skip
def test_verdict_tiers(p, ok, want):
    assert rn.verdict(p, ok, 0.05, 0.10) == want


def test_floors():
    spec = load_spec(SPEC)
    f = rn.floors({"ret_ann": 0.031, "gross_bp_rt": 2.0}, 12, spec)
    assert all(v["ok"] for v in f.values())
    f = rn.floors({"ret_ann": 0.029, "gross_bp_rt": float("nan")}, 11, spec)
    assert not any(v["ok"] for v in f.values())


# ── R₀ variants ─────────────────────────────────────────────────────────────

STRATEGIES: list[Region | VolTarget] = [r0()] + [f() for f in VARIANTS.values()]


def test_variant_definitions():
    m = market()
    bpos, nle5 = VARIANTS["b_positive"](), VARIANTS["n_le_5"]()
    assert isinstance(bpos, Region) and isinstance(nle5, Region)
    assert len(bpos.cells) == 12 and all(c.b > 0 for c in bpos.cells)
    assert len(nle5.cells) == 12 and all(c.n <= 5 for c in nle5.cells)
    head, lo, sh = (target_series(s, m) for s in (r0(), VARIANTS["long_only"](), VARIANTS["short_only"]()))
    assert (lo >= 0).all() and (sh <= 0).all()
    np.testing.assert_allclose(lo + sh, head, atol=1e-15)
    vt = target_series(VARIANTS["vol_target_10"](), m)
    np.testing.assert_allclose(vt, head * vol_scale(m.sessions["close"], 0.10, 21), atol=0)
    assert (vt.abs() <= head.abs() + 1e-15).all() and (vt.iloc[:21] == 0).all()
    assert len({s.name for s in STRATEGIES}) == len(STRATEGIES)


def test_vol_scale_by_hand():
    close = pd.Series(100 * np.cumprod(1 + np.tile([0.01, -0.01], 20)))
    sd = close.pct_change().iloc[1:22].std(ddof=1) * np.sqrt(252)
    s = vol_scale(close, 0.10, 21)
    assert (s[:21] == 0).all() and s[21] == pytest.approx(min(1.0, 0.10 / sd))
    assert vol_scale(pd.Series(np.full(30, 50.0)), 0.10, 21)[25] == 1.0  # zero vol: unscaled, never levered


def test_variant_cells_use_their_input_columns():
    m = market()
    s = m.sessions
    cases = [(VwapCell(5, 0.001, weight="rth_volume"), "close", "rth_volume"),
             (VwapCell(5, 0.001, anchor="rth_vwap"), "rth_vwap", "volume")]  # fmt: skip
    for c, price, vol in cases:
        vw = (s[price] * s[vol]).rolling(5).sum() / s[vol].rolling(5).sum()
        d = s["close"] / vw - 1
        np.testing.assert_array_equal(
            c.targets(m).iloc[4:], np.where(d > 0.001, -1, np.where(d < -0.001, 1, 0))[4:]
        )


def test_missing_variant_input_makes_its_windows_flat():
    m = market()
    s = m.sessions.copy()
    s.iloc[100, s.columns.get_loc("rth_volume")] = np.nan
    t = VwapCell(5, 0.0, weight="rth_volume").targets(replace(m, sessions=s))
    assert (t.iloc[100:105] == 0).all() and (t.iloc[95:100] != 0).any()


@pytest.mark.parametrize("strategy", STRATEGIES, ids=lambda s: s.name)
def test_r0_and_variants_vectorized_equal_the_view_loop(strategy):
    m = market("2016-07-29")
    np.testing.assert_allclose(
        target_series(strategy, m), target_series(strategy, m, vectorized=False), atol=1e-15
    )


@pytest.mark.parametrize("strategy", STRATEGIES, ids=lambda s: s.name)
def test_r0_and_variants_are_causal_and_fill_after_the_decision(strategy):
    m = market()
    assert_causal(lambda mk: target_series(strategy, mk), m, CLOSE)
    assert_causal(lambda mk: run(mk, strategy, costs=ZERO, cash_yield=0.0).targets, m, CLOSE)
    f = target_series(strategy, m).to_numpy()
    lg, _ = legs(m, "next_open")
    for c in (60, 150, 200):
        assert_fill_after(_gross_fn(lg, "next_open"), f, c, slice(0, c + 1))


@dataclass(frozen=True)
class PeekRthVolume(VwapCell):
    """Mutant: weights by the next session's RTH volume."""

    def targets(self, market):
        s = market.sessions.copy()
        s["rth_volume"] = s["rth_volume"].shift(-1)
        return pd.Series(self._targets(s), index=s.index)


@dataclass(frozen=True)
class PeekRthVwap(VwapCell):
    """Mutant: anchors on the next session's RTH VWAP."""

    def targets(self, market):
        s = market.sessions.copy()
        s["rth_vwap"] = s["rth_vwap"].shift(-1)
        return pd.Series(self._targets(s), index=s.index)


@pytest.mark.parametrize("mutant", [PeekRthVolume(5, 0.0, weight="rth_volume"),
                                    PeekRthVwap(5, 0.0, anchor="rth_vwap")], ids=lambda s: type(s).__name__)  # fmt: skip
def test_harness_catches_a_peek_at_the_variant_inputs(mutant):
    with pytest.raises(AssertionError):
        assert_causal(lambda mk: target_series(mutant, mk), market(), CLOSE)


def test_vol_target_harness_catches_a_peek():
    @dataclass(frozen=True)
    class PeekVol(VolTarget):
        def targets(self, market):
            sc = vol_scale(market.sessions["close"].shift(-1), self.sigma, self.window)
            return self.inner.targets(market) * sc

    with pytest.raises(AssertionError):
        assert_causal(lambda mk: target_series(PeekVol(r0()), mk), market(), CLOSE)


def test_default_cell_is_unchanged_by_the_variant_fields():
    m = market()
    from vwap_mr.features.deviation import trading_deviation

    d = trading_deviation(m.sessions["close"], m.sessions["volume"], 3)
    np.testing.assert_array_equal(VwapCell(3, 0.001).targets(m).iloc[2:],
                                  np.where(d > 0.001, -1, np.where(d < -0.001, 1, 0))[2:])  # fmt: skip
    assert [c.name for c in r0_cells()] == [
        f"vwap_mr_n{n}_b{b:g}bp" for n in (2, 3, 5, 10) for b in (0, 10, 25, 50)
    ]
