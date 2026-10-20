"""Fixed dates, paths and enums shared by every module (PLAN.md §4, §5).

Contract: these values are part of the pre-registration. `HOLDOUT_START` (PLAN.md §9.6) is fixed here in U0; a later
change is an amendment recorded in PLAN.md §11, never an edit. The frozen `RunConfig` (≤ 30 fields) arrives with
the runner (U6).
"""

from datetime import date
from enum import StrEnum
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
RESULTS_DIR = ROOT / "results"
SPECS_DIR = ROOT / "specs"
CACHE_DIR = ROOT / "data" / "cache"

DEV_START = date(2016, 1, 4)  # first session of the Alpaca SIP development window
DEV_END = date(2025, 12, 31)  # last calendar day of the development window
HOLDOUT_START = date(2026, 1, 1)  # research code is refused bars on or after this date


class DecisionTime(StrEnum):
    """When a strategy decides: after the close (fill at the next open) or at 15:45 ET (fill at the close)."""

    CLOSE = "close"
    T1545 = "t1545"
