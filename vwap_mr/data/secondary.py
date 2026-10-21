"""Secondary daily SPY history 1993-01-29 -> 2015-12-31, the `diag` source (U1, PLAN.md §4; §9.1 = Yahoo chart API).

Yahoo's chart API returns prices adjusted for splits only (`close` is what `VWAP.R`'s `Cl()` read; SPY has never
split, so they are raw) and `adjclose`, adjusted for splits and dividends with Yahoo's own dividend table.

Splice (PLAN.md §11, 2026-10-20 amendment): raw against raw. The ratio `1DayAuction close_raw / Yahoo close` over
every common session in [DEV_START, HOLDOUT_START) must be level (|median − 1| <= SPLICE_RTOL: no split or
adjustment hidden in either series) and constant (every calendar year's median within SPLICE_RTOL of the overall
median: a dividend-adjusted or re-scaled source would drift). Daily deviations (the two sources define the close
differently on some 2016-2022 sessions, U1) are reported, not gated. Dividends are not part of the ratio: the
development window uses the reconciled table (dividends.py); before DEV_START the diag prices carry Yahoo's
factor, chained to the 1DayAuction factor at DEV_START so the total-return series is continuous.

Contract:
- frames carry `attrs["source"] = "diag"` and a boolean `is_diag` column; `refuse_diag` (called by the cost
  model) raises on either. No registered headline statistic is computed on them.
- the raw download is cached under `data/cache/diag/yahoo/<SYMBOL>` (store.py sidecar), fetched with
  `period2 = HOLDOUT_START`, so no holdout row is ever downloaded.
- `close_split` is the split-adjusted, not dividend-adjusted close (Yahoo before DEV_START, the raw closing cross
  after; U5 emulates `VWAP.R` with it). Pre-2016 dividends are Yahoo's alone (not cross-checked; diag only)."""

import json
from collections.abc import Callable
from datetime import datetime
from pathlib import Path

import numpy as np
import pandas as pd

from vwap_mr.config import CACHE_DIR, DEV_START, HOLDOUT_START
from vwap_mr.data.alpaca import NY_TZ, midnight, ts
from vwap_mr.data.bars import load_bars
from vwap_mr.data.exo import http_get
from vwap_mr.data.store import MANIFEST, FrozenVintageError, load_frame, save_frame, vintage_dir

DIAG_START = ts("1993-01-29")  # SPY's first session
YAHOO_URL = "https://query1.finance.yahoo.com/v8/finance/chart/{symbol}"
# raw closes agree to the cent on most sessions (≈ 0.5 bp at SPY's price); one quarterly dividend is 30-50 bp
SPLICE_RTOL = 2e-4
MIN_CORR = 0.9999


class SpliceError(ValueError):
    pass


class DiagDataError(ValueError):
    pass


def refuse_diag(frame: pd.DataFrame, what: str = "this operation") -> None:
    """Raise if `frame` carries any `diag` row (attrs or the `is_diag` column)."""
    if frame.attrs.get("source") == "diag" or ("is_diag" in frame.columns and bool(frame["is_diag"].any())):
        raise DiagDataError(f"diag (secondary-source) rows cannot be used for {what}")


def parse_yahoo_chart(text: str) -> pd.DataFrame:
    """Yahoo v8 chart JSON -> frame indexed by tz-aware NY session midnight: open, high, low, close (split-adjusted),
    volume, adjclose (split + dividend adjusted). Rows with any missing field raise."""
    chart = json.loads(text)["chart"]
    if chart.get("error") or not chart.get("result"):
        raise ValueError(f"Yahoo chart error: {chart.get('error')}")
    res = chart["result"][0]
    q = res["indicators"]["quote"][0]
    stamps = pd.to_datetime(np.asarray(res["timestamp"], dtype=np.int64), unit="s", utc=True)
    idx = midnight(pd.DatetimeIndex(stamps).tz_convert(NY_TZ)).as_unit("ns")
    cols = {k: q[k] for k in ("open", "high", "low", "close", "volume")}
    cols["adjclose"] = res["indicators"]["adjclose"][0]["adjclose"]
    df = pd.DataFrame(cols, index=idx).astype("float64")
    if df.isna().any(axis=None):
        bad = df.index[df.isna().any(axis=1)]
        raise ValueError(f"Yahoo rows with missing fields: {[str(d.date()) for d in bad[:5]]}")
    if df.index.has_duplicates:
        raise ValueError("Yahoo chart has duplicate sessions")
    return df.sort_index()


def _epoch(day: pd.Timestamp) -> int:
    return int(day.tz_localize(NY_TZ).timestamp())


def load_yahoo(symbol: str = "SPY", *, cache_root: Path = CACHE_DIR, http: Callable[..., str] = http_get):
    """The raw Yahoo daily history [DIAG_START, HOLDOUT_START), from `data/cache/diag/yahoo/<symbol>` (fetched once;
    a frozen `diag` cache is never re-fetched)."""
    vdir = vintage_dir("diag", cache_root)
    base = vdir / "yahoo" / symbol
    ident = {"symbol": symbol, "source": "yahoo"}
    if base.with_suffix(".json").exists():
        return load_frame(base, expect=ident)[0]
    if (vdir / MANIFEST).exists():
        raise FrozenVintageError(f"frozen diag cache has no yahoo/{symbol}")
    holdout = ts(HOLDOUT_START)
    params = {"period1": _epoch(DIAG_START), "period2": _epoch(holdout), "interval": "1d",
              "events": "div,split", "includeAdjustedClose": "true"}  # fmt: skip
    df = parse_yahoo_chart(http(YAHOO_URL.format(symbol=symbol), params))
    df = df[df.index < holdout.tz_localize(NY_TZ)]
    meta = {**ident, "coverage_start": str(DIAG_START.date()), "coverage_end": str(holdout.date()),
            "fetched_at": datetime.now().astimezone().isoformat(), "url": YAHOO_URL.format(symbol=symbol)}  # fmt: skip
    save_frame(base, df, meta)
    return df


def splice(raw: pd.DataFrame, auction: pd.DataFrame) -> tuple[pd.DataFrame, dict]:
    """Splice Yahoo (`raw`, parse_yahoo_chart columns) onto the adjusted `1DayAuction` frame `auction`.

    Returns the diag frame for sessions before DEV_START (open, close, close_split, volume, is_diag) and the splice
    report. Raises SpliceError per the module docstring or if the adjusted closes correlate <= MIN_CORR."""
    dev_start = ts(DEV_START).tz_localize(NY_TZ)
    common = raw.index.intersection(auction.index[auction["close_raw"].notna()])
    common = common[common >= dev_start]
    if len(common) < 250:
        raise SpliceError(f"only {len(common)} overlapping sessions from {DEV_START}; need >= 250")
    ratio = auction.loc[common, "close_raw"] / raw.loc[common, "close"]
    k = float(ratio.median())
    dev = ratio / k - 1
    yearly = dev.groupby(common.year).median()
    pre = raw.index < dev_start
    if not pre.any():
        raise SpliceError("Yahoo history has no session before DEV_START")
    last_pre, first = raw.index[pre][-1], auction.index[0]
    if first != dev_start:
        raise SpliceError(f"1DayAuction must start at DEV_START; starts {first.date()}")
    f_pre = float(auction["factor"].iloc[0])
    if auction["dividend"].iloc[0] > 0:  # an ex-date on the first session also discounts the last diag close
        p0 = auction["close_raw"].iloc[0]
        f_pre *= p0 / (p0 + auction["dividend"].iloc[0])
    yf = raw["adjclose"] / raw["close"]
    scale = k * f_pre * yf / yf.loc[last_pre]  # Yahoo's dividend chain before DEV_START, ours from it
    chained = raw.loc[common, "close"] * scale.loc[common]
    corr = float(np.corrcoef(auction.loc[common, "close"], chained)[0, 1])
    level = chained / auction.loc[common, "close"] - 1
    report = {
        "n_overlap": len(common),
        "overlap": f"{common[0].date()} -> {common[-1].date()}",
        "raw_ratio_median": k,
        "raw_ratio_yearly_median_dev": {int(y): float(v) for y, v in yearly.items()},
        "raw_daily_dev_bp": {"median_abs": float(dev.abs().median() * 1e4), "p99_abs": float(dev.abs().quantile(0.99) * 1e4),
                             "max_abs": float(dev.abs().max() * 1e4), "max_date": str(dev.abs().idxmax().date())},
        "adjusted_close_corr": corr,
        "yahoo_vs_own_dividend_chain_max_dev": float(level.abs().max()),
        "factor_at_dev_start": f_pre,
        "missing_in_yahoo": len(auction.index.difference(raw.index)),
        "missing_auction_close": int(auction["close_raw"].isna().sum()),
    }  # fmt: skip
    if (yearly.abs() > SPLICE_RTOL).any():
        raise SpliceError(
            f"splice ratio not constant: yearly median deviations {report['raw_ratio_yearly_median_dev']}"
        )
    if abs(k - 1) > SPLICE_RTOL:
        raise SpliceError(f"raw price levels differ: median ratio {k:.6f} (|k - 1| > {SPLICE_RTOL:.0e})")
    if not corr > MIN_CORR:
        raise SpliceError(f"adjusted close correlation {corr:.6f} <= {MIN_CORR}")
    out = pd.DataFrame(
        {
            "open": raw.loc[pre, "open"] * scale.loc[pre],
            "close": raw.loc[pre, "close"] * scale.loc[pre],
            "close_split": raw.loc[pre, "close"],
            "volume": raw.loc[pre, "volume"],
        }
    )
    out["is_diag"] = True
    out.attrs["source"] = "diag"
    return out, report


def load_spy_long(vintage: str, *, cache_root: Path = CACHE_DIR, http: Callable[..., str] = http_get):
    """`load_long("SPY", ...)`: SPY from 1993-01-29."""
    return load_long("SPY", vintage, cache_root=cache_root, http=http)


def load_long(
    symbol: str, vintage: str, *, cache_root: Path = CACHE_DIR, http: Callable[..., str] = http_get
):
    """`symbol` daily from its first Yahoo session to HOLDOUT_START: diag (spliced) before DEV_START; from DEV_START
    the adjusted `1DayAuction` open / close, `close_split` = raw closing cross, and `1DayPrint` volume.

    For diagnostics and estimator warm-up only: the frame is tagged diag as a whole (`refuse_diag` refuses it) and
    row-wise (`is_diag`). Returns (frame, splice report)."""
    auction = load_bars(
        symbol, "1DayAuction", DEV_START, HOLDOUT_START, vintage=vintage, cache_root=cache_root
    )
    volume = load_bars(symbol, "1DayPrint", DEV_START, HOLDOUT_START, vintage=vintage, cache_root=cache_root)[
        "volume"
    ]
    raw = load_yahoo(symbol, cache_root=cache_root, http=http)
    diag, report = splice(raw, auction)
    report["volume_ratio_print_vs_yahoo_median"] = float(
        (volume / raw["volume"].reindex(volume.index)).median()
    )
    tail = pd.DataFrame({"open": auction["open"], "close": auction["close"], "close_split": auction["close_raw"],
                         "volume": volume.reindex(auction.index), "is_diag": False})  # fmt: skip
    out = pd.concat([diag, tail])
    if out.index.has_duplicates or not out.index.is_monotonic_increasing:
        raise SpliceError("spliced index is not sorted and unique")
    out.attrs["source"] = "diag"
    return out, report
