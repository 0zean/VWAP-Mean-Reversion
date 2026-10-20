"""`run(bars, strategy, *, costs, cash_yield) -> Result` (U3, ported from wavelet-meta-model
`wfo/position_backtest.py`).

Contract: fills strictly after the decision (`next_open` at the opening print of t+1; `moc` at the closing
print of t from a 15:45 decision); equity marked at 1DayPrint closes; a missing print is a counted fallback."""
