"""The looks ledger: `results/looks.jsonl` counts every configuration examined on the development window —
diagnostics, pilots, anything whose result was read — so a report can print K, the number of looks, and a
Bonferroni bound (p × K) next to every bootstrap p (PLAN.md §0, §6.7).

Ported from 0zean/wavelet-meta-model `families/looks.py` (`record_look`, `looks_total`, `bonferroni`). Changes:
rows are appended through `Ledger` (lock + fsync); a missing git SHA raises instead of recording "unknown"
(PLAN.md §0, no silent fallbacks); the reader raises on a malformed line instead of skipping it (a skipped look
would undercount K). The seed is K₀ = 1 (PLAN.md §7 U0).

Contract: a look is appended BEFORE its result is read; `looked(label, n, compute)` enforces it for a table by
recording first and computing after. A look is not a trial: trials are counted and budgeted by the runner's ledger;
looks are the wider count of what was examined, including what never became a trial.

Windows (U5): each row names the sample it read, `"dev"` (anything touching the development window
2016-01-04 → 2025-12-31; a row without the field is a dev row, as the seed is) or `"diag"` (the secondary history
before DEV_START only). The label is descriptive: K = `looks_total()` counts **every** row in every window (user
ruling 2026-10-21, the conservative count); `looks_total(window="dev")` gives the development-window share.
"""

import json
import subprocess
from collections.abc import Callable
from datetime import UTC, datetime
from pathlib import Path
from typing import TypeVar

from vwap_mr.config import RESULTS_DIR, ROOT
from vwap_mr.experiments.ledger import Ledger
from vwap_mr.stats.multiplicity import bonferroni  # noqa: F401  (re-exported for the looks report)

LOOKS_PATH = RESULTS_DIR / "looks.jsonl"
WINDOWS = ("dev", "diag")
T = TypeVar("T")


def _git_sha() -> str:
    """HEAD of this repository; raises if git is unavailable (a look without a SHA cannot be audited)."""
    out = subprocess.run(["git", "rev-parse", "HEAD"], cwd=ROOT, capture_output=True, text=True, check=True)
    return out.stdout.strip()


def _validate_look(row: dict) -> None:
    if not row.get("label") or not isinstance(row.get("n"), int) or row["n"] < 1:
        raise ValueError("a look needs a label and an integer n >= 1")
    if row.get("window", "dev") not in WINDOWS:
        raise ValueError(f"a look's window must be one of {WINDOWS}, got {row.get('window')!r}")


def record_look(
    label: str,
    n: int,
    note: str = "",
    path: Path = LOOKS_PATH,
    *,
    when: str | None = None,
    window: str = "dev",
) -> dict:
    """Append one record: `label` (a script, a report, a pilot), `n` configurations examined, a note, the window."""
    if isinstance(n, bool) or int(n) != n:
        raise ValueError(f"n must be an integer, got {n!r}")
    row = {"at": when or datetime.now(UTC).isoformat(), "label": str(label), "n": int(n), "note": str(note),
           "window": window, "git_sha": _git_sha()}  # fmt: skip
    Ledger(path, validate=_validate_look).append(row)
    return row


def looks(path: Path = LOOKS_PATH) -> list[dict]:
    """Every look, strictly: a malformed or invalid line raises (skipping it would undercount K)."""
    if not Path(path).exists():
        return []
    out = []
    with open(path, encoding="utf-8") as fh:
        for i, line in enumerate(fh, start=1):
            if not line.strip():
                continue
            try:
                row = json.loads(line)
                _validate_look(row)
            except (json.JSONDecodeError, TypeError, AttributeError, ValueError) as e:
                raise ValueError(
                    f"{path}:{i}: malformed look row ({e}); repair it by hand, K is unknown"
                ) from e
            out.append(row)
    return out


def looks_total(path: Path = LOOKS_PATH, window: str | None = None) -> int:
    """K: the configurations examined so far, in every window (or only on `window`)."""
    return int(sum(int(r["n"]) for r in looks(path) if window is None or r.get("window", "dev") == window))


def looked(
    label: str, n: int, compute: Callable[[], T], *, window: str, note: str = "", path: Path = LOOKS_PATH
) -> T:
    """Record the look, then compute its result: a table built through this cannot be read unrecorded."""
    record_look(label, n, note, path, window=window)
    return compute()
