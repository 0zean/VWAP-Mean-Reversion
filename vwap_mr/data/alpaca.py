"""Alpaca SIP bars (U1, ported from wavelet-meta-model `data/alpaca_source.py`).

Contract: credentials come from `.env` only (ALPACA_API_KEY, ALPACA_API_SECRET), are read in this module
and never logged; Alpaca's inclusive `end` is trimmed to `[start, end)`; a refused or empty request raises."""
