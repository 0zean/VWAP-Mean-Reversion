"""U0 smoke: the package imports, the ledger round-trips a row under lock, and the looks ledger holds K₀ = 1."""

import json
import multiprocessing as mp

import numpy as np
import pytest

import vwap_mr
from vwap_mr import config
from vwap_mr.experiments.ledger import Ledger
from vwap_mr.experiments.looks import LOOKS_PATH, bonferroni, looks, looks_total, record_look


def test_import_and_config():
    assert vwap_mr.__doc__
    assert config.DEV_START < config.DEV_END < config.HOLDOUT_START
    assert config.HOLDOUT_START.isoformat() == "2026-01-01"
    assert {d.value for d in config.DecisionTime} == {"close", "t1545"}


def test_ledger_round_trip(tmp_path):
    led = Ledger(tmp_path / "ledger.jsonl")
    row = {"cell_hash": "abc", "status": "ok", "sharpe": np.float64(0.5), "bad": float("nan")}
    with led.run_lock():
        led.append(row)
    assert led.rows() == [{"cell_hash": "abc", "status": "ok", "sharpe": 0.5, "bad": None}]
    with pytest.raises(ValueError):
        led.append({"status": "ok"})


def _hold_run_lock(path, ready, release):
    with Ledger(path).run_lock():
        ready.set()
        release.wait(10)


def test_run_lock_excludes_second_process(tmp_path):
    path = tmp_path / "ledger.jsonl"
    ready, release = mp.Event(), mp.Event()
    p = mp.Process(target=_hold_run_lock, args=(path, ready, release))
    p.start()
    try:
        assert ready.wait(30)
        with pytest.raises(RuntimeError, match="another experiment run"), Ledger(path).run_lock():
            pass
    finally:
        release.set()
        p.join(30)
    with Ledger(path).run_lock():  # released
        pass


def test_truncated_line_is_isolated(tmp_path, capsys):
    path = tmp_path / "ledger.jsonl"
    path.write_bytes(b'{"event": "x"}\n{"cell_ha')
    Ledger(path).append({"event": "y"})
    assert [r["event"] for r in Ledger(path).rows()] == ["x", "y"]
    assert "skipped 1 malformed" in capsys.readouterr().err


def test_seeded_looks_ledger():
    rows = looks(LOOKS_PATH)  # later units append looks: check the seed row, not the count
    assert rows[0]["n"] == 1
    assert rows[0]["label"] == "README in-sample chart and down-market reading (prior look)"
    assert looks_total(LOOKS_PATH) >= 1
    assert json.loads(LOOKS_PATH.read_text(encoding="utf-8").splitlines()[0]) == rows[0]


def test_record_look_and_bonferroni(tmp_path):
    path = tmp_path / "looks.jsonl"
    record_look("a", 3, path=path)
    record_look("b", 2, path=path)
    assert looks_total(path) == 5
    assert len(looks(path)[0]["git_sha"]) == 40
    with pytest.raises(ValueError):
        record_look("c", 0, path=path)
    with pytest.raises(ValueError):
        record_look("c", 1.5, path=path)  # ty: ignore[invalid-argument-type]  (the runtime guard is the test)
    assert bonferroni(0.02, 5) == pytest.approx(0.10)
    assert bonferroni(0.5, 5) == 1.0


def test_looks_reader_refuses_malformed_line(tmp_path):
    path = tmp_path / "looks.jsonl"
    record_look("a", 5, path=path)
    with open(path, "a", encoding="utf-8") as fh:
        fh.write('{"label": "b", "n": 7')
    with pytest.raises(ValueError, match="malformed look row"):
        looks_total(path)
    with pytest.raises(ValueError, match="NaN"):
        bonferroni(float("nan"), 5)
