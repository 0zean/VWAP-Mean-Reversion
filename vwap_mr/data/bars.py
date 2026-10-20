"""`load_bars(symbol, timeframe, start, end, *, vintage, allow_forward=False)` (U1).

Contract: the only way research code obtains bars; raises `HoldoutError` for any bar at or after
`config.HOLDOUT_START` unless `allow_forward` (live only, with an audit row); timeframes `1DayPrint`,
`5Min`, `1DayRTH`, `snap1545`."""
