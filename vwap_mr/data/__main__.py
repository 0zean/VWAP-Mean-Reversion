"""Data CLI (U1).

    uv run python -m vwap_mr.data fetch --symbols SPY QQQ IWM DIA --start 2016-01-04 --vintage 2026-10
    uv run python -m vwap_mr.data exo --vintage 2026-10                 # VIX (CBOE), DTB3 (FRED API)
    uv run python -m vwap_mr.data diag --vintage 2026-10                # Yahoo SPY 1993-2015, splice check
    uv run python -m vwap_mr.data quality --vintage 2026-10             # -> results/data_quality.csv
    uv run python -m vwap_mr.data freeze --vintage 2026-10              # write MANIFEST.json (also: --vintage diag)
    uv run python -m vwap_mr.data verify --vintage 2026-10

`--end` defaults to HOLDOUT_START (exclusive). Nothing here reads past it.
"""

import argparse
import json
from pathlib import Path

import pandas as pd

from vwap_mr.config import CACHE_DIR, DEV_START, HOLDOUT_START, RESULTS_DIR
from vwap_mr.data.alpaca import ts
from vwap_mr.data.bars import load_bars
from vwap_mr.data.calendar import get_calendar
from vwap_mr.data.exo import SERIES, load_series
from vwap_mr.data.quality import QUALITY_TIMEFRAMES, quality_report
from vwap_mr.data.secondary import load_spy_long
from vwap_mr.data.store import freeze, verify_vintage, vintage_dir

SYMBOLS = ["SPY", "QQQ", "IWM", "DIA"]


def parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("command", choices=["fetch", "exo", "diag", "quality", "freeze", "verify"])
    p.add_argument("--vintage", required=True)
    p.add_argument("--symbols", nargs="+", default=SYMBOLS)
    p.add_argument("--timeframes", nargs="+", default=["1DayPrint", "5Min"], choices=list(QUALITY_TIMEFRAMES))
    p.add_argument("--start", default=str(DEV_START))
    p.add_argument("--end", default=str(HOLDOUT_START), help="exclusive; default HOLDOUT_START")
    p.add_argument("--cache-root", type=Path, default=CACHE_DIR)
    p.add_argument("--report", type=Path, default=RESULTS_DIR / "data_quality.csv")
    p.add_argument("--note", default="")
    return p


def main(argv: list[str] | None = None) -> None:
    a = parser().parse_args(argv)
    kw = {"vintage": a.vintage, "cache_root": a.cache_root}
    if a.command == "fetch":
        for sym in a.symbols:
            for tf in a.timeframes:
                df = load_bars(sym, tf, a.start, a.end, **kw)
                print(f"[DATA]  {sym:5s} {tf:9s} {len(df):>9,} bars  {df.index[0]} -> {df.index[-1]}")
    elif a.command == "exo":
        for name in SERIES:
            df = load_series(name, "1990-01-01", a.end, **kw)
            print(f"[EXO]   {name}: {len(df):,} rows {df.index[0].date()} -> {df.index[-1].date()}")
    elif a.command == "diag":
        long, report = load_spy_long(a.vintage, cache_root=a.cache_root)
        print(json.dumps(report, indent=2))
        print(f"[DIAG]  SPY long: {len(long):,} sessions {long.index[0].date()} -> {long.index[-1].date()}, "
              f"{int(long['is_diag'].sum()):,} diag")  # fmt: skip
    elif a.command == "quality":
        cal = get_calendar(vintage_dir(a.vintage, a.cache_root), ts(a.end))
        rows = [
            {"symbol": s, "timeframe": tf, **quality_report(load_bars(s, tf, a.start, a.end, **kw), cal, tf)}
            for s in a.symbols
            for tf in a.timeframes
        ]
        a.report.parent.mkdir(parents=True, exist_ok=True)
        pd.DataFrame(rows).to_csv(a.report, index=False, lineterminator="\n")
        print(
            pd.DataFrame(rows).drop(columns=["missing_session_dates", "missing_last_bar_dates"]).to_string()
        )
        print(f"[DATA]  quality report -> {a.report}")
    elif a.command == "freeze":
        man = freeze(vintage_dir(a.vintage, a.cache_root), note=a.note)
        print(f"[DATA]  froze {man['vintage']}: {len(man['files'])} files, data_hash {man['data_hash']}")
    elif a.command == "verify":
        print(
            f"[DATA]  {a.vintage} verified, data_hash {verify_vintage(vintage_dir(a.vintage, a.cache_root))}"
        )


if __name__ == "__main__":
    main()
