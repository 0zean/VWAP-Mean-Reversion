"""U6: run the registered H1 spec once and write its report (PLAN.md §7 U6) -> results/reports/U06_H1.md.

    uv run python scripts/u06_h1.py                                    (the one registered run)
    uv run python scripts/u06_h1.py --synthetic --scratch <tmp dir>    (dry run: synthetic markets, scratch ledgers)

The registered run refuses an uncommitted or dirty spec, uncommitted code and a spec already in the ledger; it
records its look before computing, and the report refuses to render without that look. `--synthetic` replaces every
market with seeded random data (no market information), skips the git checks and writes the ledger, looks, cells
and report under `--scratch` only."""

import argparse
import shutil
import sys
from pathlib import Path

from vwap_mr.config import RESULTS_DIR, SPECS_DIR
from vwap_mr.data.synthetic import daily_market
from vwap_mr.experiments.looks import LOOKS_PATH
from vwap_mr.experiments.report import render
from vwap_mr.experiments.runner import run_spec

SEEDS = {"SPY": 1, "QQQ": 2, "IWM": 3, "DIA": 4}


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--spec", type=Path, default=SPECS_DIR / "H1_vwap_mr.yaml")
    ap.add_argument("--out", type=Path, default=RESULTS_DIR / "reports" / "U06_H1.md")
    ap.add_argument("--synthetic", action="store_true")
    ap.add_argument("--scratch", type=Path, help="dry-run directory (required with --synthetic)")
    a = ap.parse_args(argv)
    if a.synthetic:
        if a.scratch is None:
            ap.error("--synthetic needs --scratch (a dry run never touches results/)")
        a.scratch.mkdir(parents=True, exist_ok=True)
        looks = a.scratch / "looks.jsonl"
        shutil.copyfile(LOOKS_PATH, looks)  # K as of now, so the dry run's p·K reads like the real one
        rec = run_spec(
            a.spec, require_registered=False, ledger_path=a.scratch / "ledger.jsonl", looks_path=looks,
            cells_dir=a.scratch / "cells",
            loader=lambda sym, spec, rth: daily_market(sym, str(spec["window"]["start"]),
                                                       str(spec["window"]["end"]), SEEDS[sym]),
        )  # fmt: skip
        out = a.scratch / "U06_H1.md"
        out.write_text(render(rec, looks), encoding="utf-8")
    else:
        rec = run_spec(a.spec)
        out = a.out
        out.write_text(render(rec), encoding="utf-8")
    print(f"verdict {rec['verdict']}; report {out}; record {rec['record_path']}; {rec['runtime_s']} s")
    return 0


if __name__ == "__main__":
    sys.exit(main())
