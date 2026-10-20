"""Bar cache (U1, ported from wavelet-meta-model `data/store.py`).

Contract: parquet + JSON sidecar with sha256, coverage, fetched_at and schema version; atomic writes; a hash
mismatch raises; top-ups re-fetch a 7-session overlap and a re-adjustment triggers a full re-fetch; the
frozen development vintage is never written by a top-up."""
