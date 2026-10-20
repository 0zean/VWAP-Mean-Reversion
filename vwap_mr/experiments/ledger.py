"""Append-only JSONL ledger: `results/ledger.jsonl`, one JSON object per line (PLAN.md §3.1, §5).

Ported from 0zean/wavelet-meta-model `experiments/ledger.py` (`Ledger`, `clean`, the msvcrt/fcntl lock); its
stage ranking and DSR trial counting are not ported (PLAN.md §3.2).

Contract: rows are trials (`cell_hash` + `status`) or events (`event`). Each append is one write + fsync under an
exclusive lock, so a killed process leaves complete rows plus at most one truncated last line. A truncated line is
reported on stderr and skipped by the reader, and isolated by the next append (which starts a fresh line); it is
never rewritten. The file is committed to git.
"""

import contextlib
import json
import os
import sys
import time
from collections.abc import Callable, Iterable
from pathlib import Path

import numpy as np

from vwap_mr.config import RESULTS_DIR

DEFAULT_PATH = RESULTS_DIR / "ledger.jsonl"
STATUSES = ("ok", "no_fit", "error")

if os.name == "nt":  # Windows: a byte-range lock on the file's first byte (msvcrt has no flock)
    import msvcrt

    def _lock(f, blocking: bool = True) -> None:
        f.seek(0)
        while True:
            try:
                msvcrt.locking(f.fileno(), msvcrt.LK_NBLCK, 1)
                return
            except OSError:
                if not blocking:
                    raise BlockingIOError from None
                time.sleep(0.05)

    def _unlock(f) -> None:
        f.seek(0)
        msvcrt.locking(f.fileno(), msvcrt.LK_UNLCK, 1)

else:
    import fcntl

    def _lock(f, blocking: bool = True) -> None:
        fcntl.flock(f, fcntl.LOCK_EX | (0 if blocking else fcntl.LOCK_NB))

    def _unlock(f) -> None:
        fcntl.flock(f, fcntl.LOCK_UN)


def clean(x):
    """JSON-safe value: NaN / ±inf → None, numpy scalars → Python, recursively."""
    if isinstance(x, dict):
        return {str(k): clean(v) for k, v in x.items()}
    if isinstance(x, list | tuple):
        return [clean(v) for v in x]
    if isinstance(x, np.generic):
        x = x.item()
    if isinstance(x, float) and not np.isfinite(x):
        return None
    return x


def validate_trial(row: dict) -> None:
    """A row the runner can count: an event, or a trial with a cell hash and a known status."""
    if "event" in row:
        return
    if not row.get("cell_hash"):
        raise ValueError(f"ledger row without 'cell_hash': {str(row)[:200]}")
    if row.get("status") not in STATUSES:
        raise ValueError(f"ledger row with unknown status {row.get('status')!r}")


class Ledger:
    def __init__(self, path: Path | str = DEFAULT_PATH, validate: Callable[[dict], None] = validate_trial):
        self.path = Path(path)
        self.validate = validate

    def rows(self) -> list[dict]:
        """Every parseable row, in file order; malformed lines (a killed write) are reported on stderr and skipped."""
        if not self.path.exists():
            return []
        out, bad = [], []
        with open(self.path, encoding="utf-8") as f:
            for i, line in enumerate(f, start=1):
                if not line.strip():
                    continue
                try:
                    row = json.loads(line)
                except json.JSONDecodeError:
                    bad.append(i)
                    continue
                if not isinstance(row, dict):
                    bad.append(i)
                    continue
                out.append(row)
        if bad:
            print(f"[LEDGER]  {self.path}: skipped {len(bad)} malformed line(s) {bad[:10]}", file=sys.stderr)
        return out

    @contextlib.contextmanager
    def run_lock(self):
        """Exclusive for the duration of a run (a sidecar `.lock` file): two runs on one ledger would both miss the
        other's rows (duplicates) and could race the holdout check."""
        self.path.parent.mkdir(parents=True, exist_ok=True)
        with open(self.path.with_name(self.path.name + ".lock"), "a") as f:
            try:
                _lock(f, blocking=False)
            except BlockingIOError:
                raise RuntimeError(f"another experiment run holds {self.path} (one run per ledger)") from None
            try:
                yield
            finally:
                _unlock(f)

    def append(self, rows: dict | Iterable[dict]) -> None:
        rows = [rows] if isinstance(rows, dict) else list(rows)
        if not rows:
            return
        for r in rows:
            self.validate(r)
        self.path.parent.mkdir(parents=True, exist_ok=True)
        data = "".join(json.dumps(clean(r), sort_keys=True, allow_nan=False) + "\n" for r in rows)
        with open(self.path, "a+b") as f:
            _lock(f)
            try:
                f.seek(0, os.SEEK_END)
                if f.tell() > 0:
                    f.seek(-1, os.SEEK_END)
                    if f.read(1) != b"\n":  # a killed write left a partial line: start a fresh one
                        data = "\n" + data
                f.write(data.encode())
                f.flush()
                os.fsync(f.fileno())
            finally:
                _unlock(f)
