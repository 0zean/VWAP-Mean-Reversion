"""The looks ledger: `results/looks.jsonl` counts every configuration examined on the development window —
diagnostics, pilots, anything whose result was read — so a report can print K, the number of looks, and a
Bonferroni bound (p × K) next to every bootstrap p (PLAN.md §0, §6.7).

Ported from 0zean/wavelet-meta-model `families/looks.py` (`record_look`, `looks_total`, `bonferroni`). Changes:
rows are appended through `Ledger` (lock + fsync); a missing git SHA raises instead of recording "unknown"
(PLAN.md §0, no silent fallbacks). The seed is K₀ = 1 (PLAN.md §7 U0).

Contract: a look is appended BEFORE its result is read. A look is not a trial: trials are counted and budgeted by
the runner's ledger; looks are the wider count of what was examined, including what never became a trial.
"""

import subprocess
from datetime import UTC, datetime
from pathlib import Path

from vwap_mr.config import RESULTS_DIR, ROOT
from vwap_mr.experiments.ledger import Ledger

LOOKS_PATH = RESULTS_DIR / "looks.jsonl"


def _git_sha() -> str:
    """HEAD of this repository; raises if git is unavailable (a look without a SHA cannot be audited)."""
    out = subprocess.run(["git", "rev-parse", "HEAD"], cwd=ROOT, capture_output=True, text=True, check=True)
    return out.stdout.strip()


def _validate_look(row: dict) -> None:
    if not row.get("label") or not isinstance(row.get("n"), int) or row["n"] < 1:
        raise ValueError("a look needs a label and an integer n >= 1")


def record_look(
    label: str, n: int, note: str = "", path: Path = LOOKS_PATH, *, when: str | None = None
) -> dict:
    """Append one record: `label` (a script, a report, a pilot), `n` configurations examined, a note."""
    if isinstance(n, bool) or int(n) != n:
        raise ValueError(f"n must be an integer, got {n!r}")
    row = {"at": when or datetime.now(UTC).isoformat(), "label": str(label), "n": int(n), "note": str(note),
           "git_sha": _git_sha()}  # fmt: skip
    Ledger(path, validate=_validate_look).append(row)
    return row


def looks(path: Path = LOOKS_PATH) -> list[dict]:
    return Ledger(path, validate=_validate_look).rows()


def looks_total(path: Path = LOOKS_PATH) -> int:
    """K: the configurations examined so far."""
    return int(sum(int(r["n"]) for r in looks(path)))


def bonferroni(p: float, k: int) -> float:
    """The Bonferroni bound on a p-value over K looks (capped at 1)."""
    return float(min(1.0, p * max(int(k), 1)))
