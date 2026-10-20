"""Cash dividends and total-return adjustment factors (U1; PLAN.md §11 amendment of 2026-10-20).

Neither Alpaca's `adjustment=all` nor Yahoo's `adjclose` is right on every ex-date (U1 found Alpaca missing SPY
2016-03-18, 2018-06-15 and DIA 2016-03-18, doubling QQQ 2022-09-19; Yahoo missing QQQ 2020-09-21). So:

- the dividend table is Alpaca's corporate-actions cash dividends **cross-checked** against Yahoo's dividend events;
  an ex-date where the two disagree by more than AMOUNT_ATOL (or one has it and the other does not) is refused
  unless `data/dividends_overrides.csv` (checked in; symbol, ex_date, amount, note citing the source) resolves it;
- any split or other price-changing corporate action in the window raises (none occurs for SPY, QQQ, IWM, DIA in
  2016-2025; handling one is future work, not a silent skip);
- the reconciled table is cached in the vintage (`dividends/<SYMBOL>`), so a frozen vintage does not depend on a
  later edit of the overrides file;
- `adjustment_factor`: backward multiplicative, factor(t) = Π over ex-dates e > t of P_e / (P_e + D_e), P_e the raw
  close on the ex-date, equal to 1 on the last session of the table's window. Adjusted returns then equal the exact
  total return (P_e + D_e) / P_{e-1} − 1 (Yahoo's and CRSP's 1 − D_e / P_{e-1} only approximate it). Prices × factor
  are total-return adjusted (exactly close-to-close; an adjusted open-to-open return across an ex-date is off by
  D·(O_e/P_e − 1)/O_{e−1}, ≈ 0.25 bp typically, so an engine needing exact next-open legs uses raw prices + `dividend`), and the factor does not move when later dividends are paid outside the window (a frozen vintage stays
  frozen)."""

import json
from collections.abc import Callable
from datetime import datetime
from pathlib import Path

import numpy as np
import pandas as pd

from vwap_mr.config import CACHE_DIR, ROOT
from vwap_mr.data.alpaca import NY_TZ, AlpacaSource, midnight, ts
from vwap_mr.data.store import MANIFEST, FrozenVintageError, load_frame, save_frame, vintage_dir

OVERRIDES_PATH = ROOT / "data" / "dividends_overrides.csv"
AMOUNT_ATOL = 6e-4  # dollars per share; Yahoo rounds amounts to 3 decimals
YAHOO_URL = "https://query1.finance.yahoo.com/v8/finance/chart/{symbol}"
PRICE_ACTIONS = ("forward_splits", "reverse_splits", "unit_splits", "stock_dividends", "spin_offs",
                 "cash_mergers", "stock_mergers", "stock_and_cash_mergers", "name_changes")  # fmt: skip


class DividendMismatchError(ValueError):
    pass


def _frame(dates, amounts) -> pd.DataFrame:
    idx = pd.DatetimeIndex(pd.to_datetime(list(dates)), name=None).as_unit("ns")
    df = pd.DataFrame({"amount": np.asarray(list(amounts), dtype=float)}, index=idx)
    return df.groupby(level=0).sum().sort_index()  # two rows on one ex-date (regular + special) add up


def parse_alpaca(actions: dict[str, list[dict]]) -> pd.DataFrame:
    """Alpaca corporate actions -> cash dividends by ex-date (tz-naive). Raises on any price-changing action."""
    other = {k: v for k, v in actions.items() if k in PRICE_ACTIONS and v}
    if other:
        raise DividendMismatchError(f"unhandled corporate actions: {json.dumps(other, default=str)[:500]}")
    unknown = set(actions) - set(PRICE_ACTIONS) - {"cash_dividends"}
    if unknown:
        raise DividendMismatchError(f"unknown corporate-action types {sorted(unknown)}")
    rows = actions.get("cash_dividends", [])
    return _frame((r["ex_date"] for r in rows), (r["rate"] for r in rows))


def parse_yahoo_dividends(text: str) -> pd.DataFrame:
    """Yahoo chart JSON `events.dividends` -> amount by ex-date (NY date of the event stamp)."""
    chart = json.loads(text)["chart"]
    if chart.get("error") or not chart.get("result"):
        raise ValueError(f"Yahoo chart error: {chart.get('error')}")
    divs = (chart["result"][0].get("events") or {}).get("dividends") or {}
    stamps = pd.to_datetime([int(d["date"]) for d in divs.values()], unit="s", utc=True)
    days = midnight(pd.DatetimeIndex(stamps).tz_convert(NY_TZ)).tz_localize(None)
    return _frame(days, (d["amount"] for d in divs.values()))


def read_overrides(path: Path = OVERRIDES_PATH) -> pd.DataFrame:
    """The checked-in overrides: symbol, ex_date, amount, note (every row must cite its source in `note`)."""
    if not path.exists():
        return pd.DataFrame(columns=["symbol", "ex_date", "amount", "note"])
    df = pd.read_csv(path, dtype={"symbol": str, "note": str}, parse_dates=["ex_date"])
    if df["note"].isna().any() or (df["note"].str.strip() == "").any():
        raise ValueError(f"{path}: every override needs a note citing its source")
    if df.duplicated(["symbol", "ex_date"]).any():
        raise ValueError(f"{path}: duplicate (symbol, ex_date) rows")
    return df


def reconcile(
    symbol: str, alpaca: pd.DataFrame, yahoo: pd.DataFrame, overrides: pd.DataFrame
) -> pd.DataFrame:
    """Cross-check the two tables. Returns amount, alpaca, yahoo, resolution (0 = agree, 1 = override) by ex-date;
    raises DividendMismatchError listing every unresolved disagreement."""
    t = pd.DataFrame({"alpaca": alpaca["amount"], "yahoo": yahoo["amount"]})
    ov = overrides[overrides["symbol"] == symbol].set_index("ex_date")["amount"]
    agree = (t["alpaca"] - t["yahoo"]).abs() <= AMOUNT_ATOL
    t["amount"] = t["alpaca"].where(agree)
    t["resolution"] = np.where(agree, 0.0, np.nan)
    has_ov = t.index.isin(ov.index) & ~agree.to_numpy()
    if has_ov.any():
        t.loc[has_ov, "amount"] = ov.reindex(t.index[has_ov]).to_numpy()
        t.loc[has_ov, "resolution"] = 1.0
    stale = ov.index.difference(t.index[has_ov])
    if len(stale):
        raise DividendMismatchError(
            f"{symbol}: overrides that match no disagreement: {[str(d.date()) for d in stale]}"
        )
    bad = t[t["resolution"].isna()]
    if len(bad):
        lines = "\n".join(
            f"  {d.date()}  alpaca {a}  yahoo {y}"
            for d, a, y in zip(bad.index, bad["alpaca"], bad["yahoo"], strict=True)
        )
        raise DividendMismatchError(
            f"{symbol}: dividend tables disagree; resolve in {OVERRIDES_PATH.name}:\n{lines}"
        )
    return t[t["amount"] > 0][["amount", "alpaca", "yahoo", "resolution"]]


def _yahoo_text(symbol: str, start: pd.Timestamp, end: pd.Timestamp, http: Callable[..., str]) -> str:
    params = {"period1": int(start.tz_localize(NY_TZ).timestamp()), "period2": int(end.tz_localize(NY_TZ).timestamp()),
              "interval": "1d", "events": "div"}  # fmt: skip
    return http(YAHOO_URL.format(symbol=symbol), params)


def load_dividends(symbol: str, start, end, *, vintage: str, cache_root: Path = CACHE_DIR, source=None,
                   http: Callable[..., str] | None = None, overrides: Path | None = None) -> pd.DataFrame:  # fmt: skip
    """The reconciled cash-dividend table for ex-dates in [start, end), cached in the vintage (`dividends/<SYMBOL>`,
    a request outside the cached window rebuilds it, except in a frozen vintage, where it raises)."""
    from vwap_mr.data.exo import http_get

    start, end = ts(start), ts(end)
    vdir = vintage_dir(vintage, cache_root)
    base = vdir / "dividends" / symbol
    ident = {"symbol": symbol}
    frozen = (vdir / MANIFEST).exists()
    if base.with_suffix(".json").exists():
        df, meta = load_frame(base, expect=ident)
        if ts(meta["coverage_start"]) <= start and end <= ts(meta["coverage_end"]):
            return df[(df.index >= start) & (df.index < end)]
        if frozen:
            raise FrozenVintageError(f"{symbol} dividends cover [{meta['coverage_start']}, {meta['coverage_end']}); "
                                     f"requested [{start.date()}, {end.date()})")  # fmt: skip
    elif frozen:
        raise FrozenVintageError(f"frozen {vdir.name} has no dividends/{symbol}")
    src = source or AlpacaSource()
    alp = parse_alpaca(src.corporate_actions(symbol, start.date(), ts(end - pd.Timedelta(days=1)).date()))
    yah = parse_yahoo_dividends(_yahoo_text(symbol, start, end, http or http_get))
    yah = yah[(yah.index >= start) & (yah.index < end)]
    ov = read_overrides(overrides or OVERRIDES_PATH)
    ov = ov[(ov["ex_date"] >= start) & (ov["ex_date"] < end)]  # only this window's overrides can be stale
    table = reconcile(symbol, alp, yah, ov)
    meta = {**ident, "coverage_start": str(start.date()), "coverage_end": str(end.date()),
            "fetched_at": datetime.now().astimezone().isoformat(), "overrides_used": int(table["resolution"].sum())}  # fmt: skip
    save_frame(base, table, meta)
    return table


def adjustment_factor(close_raw: pd.Series, dividends: pd.DataFrame) -> pd.Series:
    """Backward total-return factor per session (see the module docstring). `close_raw` is indexed by session
    (tz-aware NY midnight or tz-naive); every ex-date must be a session with a finite close, else raise."""
    idx = pd.DatetimeIndex(close_raw.index)
    days = idx.tz_convert(NY_TZ).tz_localize(None) if idx.tz is not None else idx
    pos = days.get_indexer(pd.DatetimeIndex(dividends.index))
    if (pos < 0).any():
        bad = dividends.index[pos < 0]
        raise ValueError(
            f"ex-dates that are not sessions in the price series: {[str(d.date()) for d in bad]}"
        )
    px = close_raw.to_numpy(float)[pos]
    if not np.isfinite(px).all():
        bad = [str(d.date()) for d in dividends.index[~np.isfinite(px)]]
        raise ValueError(f"missing close on ex-dates {bad}")
    step = np.ones(len(days))
    step[pos] = px / (px + dividends["amount"].to_numpy(float))
    # factor(t) = product of step[e] for e > t
    after = np.cumprod(step[::-1])[::-1]
    factor = np.append(after[1:], 1.0)
    return pd.Series(factor, index=close_raw.index, name="factor")
