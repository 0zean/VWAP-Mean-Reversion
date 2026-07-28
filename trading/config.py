import os
from dataclasses import dataclass, field


@dataclass
class StrategyConfig:
    """All tunable parameters for the VWAP mean reversion strategy.

    Credentials are read from environment variables by default so that
    they are never hardcoded in source files.

        export ALPACA_API_KEY=<your_api_key>
        export ALPACA_API_SECRET=<your_api_secret>
    """

    # -- Universe --
    symbol: str = "SPY"

    # -- VWAP signal --
    nlookback: int = 3  # rolling window (days) used to compute VWAP
    u_lim: float = 1.001  # price/vwap > u_lim -> short
    l_lim: float = 0.999  # price/vwap < l_lim -> long

    # -- Backtest --
    start_date: str = "2016-01-01"  # Alpaca free plan: ~5 yr history
    end_date: str | None = None  # None = most recent available bar
    init_cash: float = 10_000.0
    fees: float = 0.0005  # 0.05% per trade (slippage)

    # -- Alpaca API credentials --
    api_key: str = field(default_factory=lambda: os.environ.get("ALPACA_API_KEY", ""))
    secret_key: str = field(
        default_factory=lambda: os.environ.get("ALPACA_API_SECRET", "")
    )

    paper: bool = True
