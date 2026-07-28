from datetime import datetime

import pandas as pd
from alpaca.data.enums import Adjustment
from alpaca.data.historical import StockHistoricalDataClient
from alpaca.data.models import BarSet
from alpaca.data.requests import StockBarsRequest
from alpaca.data.timeframe import TimeFrame
from config import StrategyConfig


def _build_client(config: StrategyConfig) -> StockHistoricalDataClient:
    """Instantiate the Alpaca historical data client

    Args:
        config (StrategyConfig): The strategy configuration object containing API credentials.

    Returns:
        StockHistoricalDataClient: An instance of the Alpaca historical data client.
    """
    if not config.api_key or not config.secret_key:
        raise ValueError(
            "Alpaca credentials not found. "
            "Set ALPACA_API_KEY and ALPACA_SECRET_KEY environment variables."
        )
    return StockHistoricalDataClient(
        api_key=config.api_key, secret_key=config.secret_key
    )


def get_historical_bars(config: StrategyConfig) -> pd.DataFrame | None:
    """Fetch daily OHLCV bars from Alpaca for the configured symbol and date range.

    Args:
        config (StrategyConfig): The strategy configuration object containing symbol, date range, and API credentials.

    Returns:
        pd.DataFrame: Timezone-naive DatetimeIndex, columns:
        open, high, low, close, volume, trade_count, vwap
    """
    client = _build_client(config)

    start = datetime.strptime(config.start_date, "%Y-%m-%d")
    end = (
        datetime.now()
        if config.end_date is None
        else datetime.strptime(config.end_date, "%Y-%m-%d")
    )

    request = StockBarsRequest(
        symbol_or_symbols=config.symbol,
        timeframe=TimeFrame.Day,
        start=start,
        end=end,
        adjustment=Adjustment.ALL,
    )

    bars = client.get_stock_bars(request)
    if isinstance(bars, BarSet):
        df: pd.DataFrame = bars.df

        if isinstance(df.index, pd.MultiIndex):
            df = df.xs(config.symbol, level="symbol")

        if isinstance(df.index, pd.DatetimeIndex) and df.index.tz is not None:
            df.index = df.index.tz_convert("UTC").tz_localize(None)

        df.index.name = "date"
        df = df.sort_index()

        return df

    return None
