"""U11: run the registered H4 spec once and write its report (PLAN.md §7 U11) -> results/reports/U11_blend.md.

    uv run python scripts/u11_h4.py                                    (the one registered run)
    uv run python scripts/u11_h4.py --synthetic --scratch <tmp dir>    (dry run: synthetic series, scratch ledgers)

The registered run refuses an uncommitted or dirty spec, uncommitted code and a spec already in the ledger. It checks
the rebuilt H1 stream against H1's ledger row, records its look before computing, and the report refuses to render
without that look. `--synthetic` replaces the market and the long series with one seeded random series (no market
information), skips the git checks and writes the ledger, looks, cells and report under `--scratch` only."""

import argparse
import shutil
import sys
from pathlib import Path

from vwap_mr.config import RESULTS_DIR, SPECS_DIR
from vwap_mr.experiments.h3 import synthetic_inputs
from vwap_mr.experiments.h4 import render_h4, run_h4
from vwap_mr.experiments.looks import LOOKS_PATH


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--spec", type=Path, default=SPECS_DIR / "H4_blend.yaml")
    ap.add_argument("--out", type=Path, default=RESULTS_DIR / "reports" / "U11_blend.md")
    ap.add_argument("--synthetic", action="store_true")
    ap.add_argument("--scratch", type=Path, help="dry-run directory (required with --synthetic)")
    a = ap.parse_args(argv)
    if a.synthetic:
        if a.scratch is None:
            ap.error("--synthetic needs --scratch (a dry run never touches results/)")
        a.scratch.mkdir(parents=True, exist_ok=True)
        looks = a.scratch / "looks.jsonl"
        shutil.copyfile(LOOKS_PATH, looks)  # K as of now, so the dry run's p·K reads like the real one
        rec = run_h4(a.spec, inputs=synthetic_inputs, require_registered=False, ledger_path=a.scratch / "ledger.jsonl",
                     looks_path=looks, cells_dir=a.scratch / "cells")  # fmt: skip
        out = a.scratch / "U11_blend.md"
        out.write_text(render_h4(rec, looks), encoding="utf-8")
    else:
        rec = run_h4(a.spec)
        out = a.out
        out.write_text(render_h4(rec), encoding="utf-8")
    print(f"headline {'passes' if rec['passes'] else 'null'}; report {out}; "
          f"record {rec['record_path']}; {rec['runtime_s']} s")  # fmt: skip
    return 0


if __name__ == "__main__":
    sys.exit(main())
