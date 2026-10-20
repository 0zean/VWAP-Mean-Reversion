"""Secondary daily SPY history 1993-01-29 -> 2015-12-31, the `diag` source (U1, PLAN.md §4; §9.1 = Yahoo chart API).

Yahoo's chart API returns OHLC adjusted for splits only (`close` is what `VWAP.R`'s `Cl()` read) and `adjclose`,
adjusted for splits and dividends. Alpaca's `adjustment=all` adjusts OHLC for both, so the diag OHLC are scaled
by `adjclose / close` (the same total-return convention) and then by one constant ratio to Alpaca's level.

Contract:
- the splice ratio `alpaca close / adjusted Yahoo close` is measured over every common session in
  [DEV_START, HOLDOUT_START); it must be constant to rounding (max |ratio / median − 1| <= SPLICE_RTOL) and the
  spliced closes must correlate > 0.9999, else `SpliceError`. A dividend-adjustment mismatch shows as a ratio that
  steps at every ex-date, which this refuses.
- frames carry `attrs["source"] = "diag"` and a boolean `is_diag` column; `refuse_diag` (called by the cost
  model) raises on either. No registered headline statistic is computed on them.
- the raw download is cached under `data/cache/diag/yahoo/SPY` (store.py sidecar), fetched with
  `period2 = HOLDOUT_START`, so no holdout row is ever downloaded.
- `close_split` keeps Yahoo's split-adjusted close unscaled (U5 emulates `VWAP.R` with it)."""

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
SPLICE_RTOL = (
    5e-4  # ≈ 1/8 of one quarterly SPY dividend (0.3–0.5 %): a per-ex-date mismatch cannot hide under it
)
MIN_CORR = 0.9999
PRICE_COLUMNS = ["open", "high", "low", "close"]


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


def splice(raw: pd.DataFrame, alpaca: pd.DataFrame) -> tuple[pd.DataFrame, dict]:
    """Total-return-adjust `raw` (Yahoo) and scale it to Alpaca's `1DayPrint` level.

    Returns the diag frame for sessions before DEV_START (open, high, low, close, volume, close_split, is_diag) and
    the splice report. Raises SpliceError if the overlap ratio is not constant to SPLICE_RTOL or the spliced
    closes correlate <= MIN_CORR."""
    factor = raw["adjclose"] / raw["close"]
    adj = raw[PRICE_COLUMNS].mul(factor, axis=0)
    dev_start = ts(DEV_START).tz_localize(NY_TZ)
    common = adj.index.intersection(alpaca.index)
    common = common[common >= dev_start]
    if len(common) < 250:
        raise SpliceError(f"only {len(common)} overlapping sessions from {DEV_START}; need >= 250")
    ratio = alpaca.loc[common, "close"] / adj.loc[common, "close"]
    k = float(ratio.median())
    rel = (ratio / k - 1).abs()
    corr = float(np.corrcoef(alpaca.loc[common, "close"], adj.loc[common, "close"] * k)[0, 1])
    yearly = (ratio / k - 1).groupby(common.year).median()
    report = {
        "n_overlap": len(common),
        "overlap": f"{common[0].date()} -> {common[-1].date()}",
        "ratio_median": k,
        "ratio_max_rel_dev": float(rel.max()),
        "ratio_max_rel_dev_date": str(rel.idxmax().date()),
        "ratio_yearly_median_rel": {int(y): float(v) for y, v in yearly.items()},
        "close_corr": corr,
        "volume_ratio_median": float((alpaca.loc[common, "volume"] / raw.loc[common, "volume"]).median()),
        "missing_in_yahoo": len(alpaca.index[alpaca.index >= dev_start].difference(raw.index)),
        "missing_in_alpaca": len(raw.index[raw.index >= dev_start].difference(alpaca.index)),
    }
    if report["ratio_max_rel_dev"] > SPLICE_RTOL:
        raise SpliceError(f"splice ratio not constant: max |ratio/median - 1| = {report['ratio_max_rel_dev']:.2e} "
                          f"on {report['ratio_max_rel_dev_date']} > {SPLICE_RTOL:.0e}")  # fmt: skip
    if not corr > MIN_CORR:
        raise SpliceError(f"spliced close correlation {corr:.6f} <= {MIN_CORR}")
    pre = adj.index < dev_start
    out = adj.loc[pre] * k
    out["volume"] = raw.loc[pre, "volume"]
    out["close_split"] = raw.loc[pre, "close"]
    out["is_diag"] = True
    out.attrs["source"] = "diag"
    return out, report


def load_spy_long(vintage: str, *, cache_root: Path = CACHE_DIR, http: Callable[..., str] = http_get):
    """SPY daily from 1993-01-29 to HOLDOUT_START: diag (spliced) before DEV_START, Alpaca `1DayPrint` from DEV_START.

    For diagnostics and estimator warm-up only: the frame is tagged diag as a whole (`refuse_diag` refuses it) and
    row-wise (`is_diag`). Returns (frame, splice report)."""
    alpaca = load_bars("SPY", "1DayPrint", DEV_START, HOLDOUT_START, vintage=vintage, cache_root=cache_root)
    diag, report = splice(load_yahoo("SPY", cache_root=cache_root, http=http), alpaca)
    tail = alpaca[["open", "high", "low", "close", "volume"]].assign(close_split=np.nan, is_diag=False)
    out = pd.concat([diag, tail])
    if out.index.has_duplicates or not out.index.is_monotonic_increasing:
        raise SpliceError("spliced index is not sorted and unique")
    out.attrs["source"] = "diag"
    return out, report
