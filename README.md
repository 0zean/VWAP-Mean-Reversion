# VWAP Mean Reversion

A one-day reversal rule on index ETFs (a 3-day volume-weighted average of closes as the anchor, originally `VWAP.R`
on SPY), being rebuilt in Python as a pre-registered, regime-aware research program: Alpaca daily bars, a
position-series backtest with next-open and market-on-close fills at the official prints, block-bootstrap
statistics, Hurst/wavelet state estimators, HMM and changepoint regime models, a mean-reversion plus trend blend,
and an Alpaca paper-trading forward test. [PLAN.md](PLAN.md) is the source of truth for the thesis, protocol, units
of work and status; the old README's performance claim (Sharpe 0.468 vs 0.314) and why it does not stand as stated
are in PLAN.md §1.1. Setup: `uv sync`, copy `.env.example` to `.env`; run `scripts/done_gate.ps1` before closing a
unit.
