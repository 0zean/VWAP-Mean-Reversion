"""Frame cache and frozen vintages (U1; ported from 0zean/wavelet-meta-model `data/store.py`: `atomic_write`,
`merge_bars`, the sha256 sidecar; npz replaced by parquet).

Each cached frame is two files: `{name}.parquet` (column `ts` = index as int64 ns UTC, float64 value columns, int64
ns UTC for tz-aware datetime columns) and `{name}.json` (identity, coverage, fetched_at, schema version and a
sha256 over the arrays, verified on every load). Writes are atomic (temp file + fsync + os.replace; the parquet
lands before the sidecar, so a crash in between leaves a hash mismatch that a load rejects).

A cache directory (a *vintage*) is frozen by `freeze`, which writes `MANIFEST.json` listing every sidecar's hash.
After that `atomic_write` refuses any path inside it, and every load checks its sidecar hash against the manifest.
The manifest hash is the vintage's data hash (the ledger's `data_hash`, U6). The `live` cache is never frozen."""

import hashlib
import json
import os
import tempfile
from collections.abc import Callable
from pathlib import Path
from typing import BinaryIO

import numpy as np
import pandas as pd
import pyarrow as pa
import pyarrow.parquet as pq

from vwap_mr.config import CACHE_DIR

SCHEMA_VERSION = 1
MANIFEST = "MANIFEST.json"
LIVE = "live"


class CacheError(RuntimeError):
    pass


class FrozenVintageError(CacheError):
    pass


def vintage_dir(vintage: str, root: Path = CACHE_DIR) -> Path:
    """`live` and `diag` are directories of that name; any other vintage `v` is `vintage_v`."""
    if not vintage or any(c in vintage for c in "/\\:"):
        raise ValueError(f"bad vintage name {vintage!r}")
    return Path(root) / (vintage if vintage in (LIVE, "diag") else f"vintage_{vintage}")


def frozen_root(path: Path) -> Path | None:
    """The frozen vintage directory containing `path`, if any."""
    for parent in Path(path).resolve().parents:
        if (parent / MANIFEST).exists():
            return parent
    return None


def atomic_write(path: Path, write: Callable[[BinaryIO], object]) -> None:
    """Write `path` via a temp file, fsync and os.replace. Refuses any path inside a frozen vintage."""
    if (root := frozen_root(path)) is not None:
        raise FrozenVintageError(f"{root} is frozen; refusing to write {path}")
    path.parent.mkdir(parents=True, exist_ok=True)
    fd, tmp = tempfile.mkstemp(dir=path.parent, prefix=f".{path.name}.", suffix=".tmp")
    try:
        with os.fdopen(fd, "wb") as fh:
            write(fh)
            fh.flush()
            os.fsync(fh.fileno())
        os.replace(tmp, path)
    except BaseException:
        Path(tmp).unlink(missing_ok=True)
        raise


def cache_paths(base: Path) -> tuple[Path, Path]:
    return base.with_suffix(".parquet"), base.with_suffix(".json")


def _arrays(df: pd.DataFrame) -> tuple[dict[str, np.ndarray], list[str], list[str]]:
    idx = pd.DatetimeIndex(df.index)
    ts = (idx.tz_convert("UTC") if idx.tz is not None else idx).as_unit("ns").asi8.astype(np.int64)
    arrays, dt_cols = {"ts": ts}, []
    for c in df.columns:
        if isinstance(df[c].dtype, pd.DatetimeTZDtype):
            arrays[c] = pd.DatetimeIndex(df[c]).tz_convert("UTC").as_unit("ns").asi8.astype(np.int64)
            dt_cols.append(c)
        else:
            arrays[c] = df[c].to_numpy(dtype=np.float64)
    return arrays, list(df.columns), dt_cols


def _sha256(arrays: dict[str, np.ndarray], columns: list[str]) -> str:
    h = hashlib.sha256()
    for name in ["ts", *columns]:
        a = np.ascontiguousarray(arrays[name])
        h.update(name.encode())
        h.update(str(a.dtype).encode())
        h.update(a.tobytes())
    return h.hexdigest()


def save_frame(base: Path, df: pd.DataFrame, meta: dict) -> dict:
    """Write `df` (sorted unique DatetimeIndex; float or tz-aware datetime columns) and its sidecar."""
    if not df.index.is_monotonic_increasing or df.index.has_duplicates:
        raise CacheError("index must be sorted and unique")
    tz = pd.DatetimeIndex(df.index).tz
    arrays, columns, dt_cols = _arrays(df)
    full = {
        **meta,
        "schema_version": SCHEMA_VERSION,
        "columns": columns,
        "datetime_columns": dt_cols,
        "index_tz": str(tz) if tz is not None else None,
        "n_rows": len(df),
        "first": df.index[0].isoformat() if len(df) else None,
        "last": df.index[-1].isoformat() if len(df) else None,
        "sha256": _sha256(arrays, columns),
    }
    path, js = cache_paths(base)
    table = pa.table({k: pa.array(v) for k, v in arrays.items()})
    atomic_write(path, lambda fh: pq.write_table(table, fh))
    atomic_write(js, lambda fh: fh.write(json.dumps(full, indent=2, sort_keys=True).encode()))
    return full


def read_meta(base: Path) -> dict:
    js = cache_paths(base)[1]
    if not js.exists():
        raise CacheError(f"missing sidecar {js}")
    meta = json.loads(js.read_text(encoding="utf-8"))
    if meta.get("schema_version") != SCHEMA_VERSION:
        raise CacheError(f"{js}: schema_version {meta.get('schema_version')} != {SCHEMA_VERSION}")
    return meta


def load_frame(base: Path, expect: dict | None = None) -> tuple[pd.DataFrame, dict]:
    """Load and verify a cached frame: sidecar present, schema, `expect` keys, array hash, and (inside a frozen
    vintage) the manifest's hash for this sidecar. Any failure raises CacheError."""
    path, js = cache_paths(base)
    meta = read_meta(base)
    for k, v in (expect or {}).items():
        if meta.get(k) != v:
            raise CacheError(f"{js}: {k}={meta.get(k)!r}, expected {v!r}")
    columns, dt_cols = meta["columns"], set(meta["datetime_columns"])
    table = pq.read_table(path)
    arrays = {name: table.column(name).to_numpy() for name in ["ts", *columns]}
    if _sha256(arrays, columns) != meta["sha256"]:
        raise CacheError(f"{path}: array hash does not match {js}")
    if (root := frozen_root(path)) is not None:
        listed = _read_manifest(root)["files"].get(js.resolve().relative_to(root).as_posix())
        if listed != meta["sha256"]:
            raise CacheError(f"{js}: hash not in {root / MANIFEST} (listed {listed!r})")
    index = pd.DatetimeIndex(arrays["ts"].astype("datetime64[ns]"))
    if meta["index_tz"] is not None:
        index = index.tz_localize("UTC").tz_convert(meta["index_tz"])
    data = {
        c: pd.DatetimeIndex(arrays[c].astype("datetime64[ns]")).tz_localize("UTC")
        if c in dt_cols
        else arrays[c]
        for c in columns
    }
    return pd.DataFrame(data, index=index), meta


def merge_bars(old: pd.DataFrame, new: pd.DataFrame) -> pd.DataFrame:
    """Union on timestamp; for duplicate stamps the newer fetch wins. Result sorted and unique."""
    both = pd.concat([old, new])
    return both[~both.index.duplicated(keep="last")].sort_index()


# ── Frozen vintages ─────────────────────────────────────────────────────────


def _read_manifest(root: Path) -> dict:
    return json.loads((root / MANIFEST).read_text(encoding="utf-8"))


def _file_sha(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def freeze(vdir: Path, note: str = "") -> dict:
    """Write `MANIFEST.json` for the vintage directory: every frame sidecar's array hash, the sha256 of every other
    JSON file (the calendar), and `data_hash` over the sorted listing. Refuses `live`, an empty or already frozen
    directory, and any sidecar whose frame fails verification."""
    vdir = Path(vdir)
    if vdir.name == LIVE:
        raise ValueError("the live cache is never frozen")
    if (vdir / MANIFEST).exists():
        raise FrozenVintageError(f"{vdir} is already frozen")
    files = {}
    for js in sorted(vdir.rglob("*.json")):
        rel = js.relative_to(vdir).as_posix()
        if js.with_suffix(".parquet").exists():
            _, meta = load_frame(js.with_suffix(""))  # verifies the array hash
            files[rel] = meta["sha256"]
        else:
            files[rel] = _file_sha(js)
    if not files:
        raise CacheError(f"{vdir}: nothing to freeze")
    digest = hashlib.sha256(json.dumps(files, sort_keys=True).encode()).hexdigest()
    manifest = {"vintage": vdir.name, "frozen_at": pd.Timestamp.now(tz="UTC").isoformat(), "note": note,
                "files": files, "data_hash": digest}  # fmt: skip
    atomic_write(
        vdir / MANIFEST, lambda fh: fh.write(json.dumps(manifest, indent=2, sort_keys=True).encode())
    )
    return manifest


def verify_vintage(vdir: Path) -> str:
    """Re-verify every file a frozen vintage lists (and that it lists every file present); returns `data_hash`."""
    vdir = Path(vdir)
    if not (vdir / MANIFEST).exists():
        raise CacheError(f"{vdir} is not frozen")
    man = _read_manifest(vdir)
    present = {p.relative_to(vdir).as_posix() for p in vdir.rglob("*.json") if p.name != MANIFEST}
    if present != set(man["files"]):
        raise CacheError(f"{vdir}: files differ from the manifest: {sorted(present ^ set(man['files']))}")
    for rel, sha in man["files"].items():
        p = vdir / rel
        if p.with_suffix(".parquet").exists():
            load_frame(p.with_suffix(""))  # array hash + manifest check
        elif _file_sha(p) != sha:
            raise CacheError(f"{p}: sha256 differs from the manifest")
    return man["data_hash"]
