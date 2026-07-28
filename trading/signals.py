import numpy as np
import pandas as pd
from config import StrategyConfig
from matplotlib.pylab import roll
from pygments.lexer import default
from pylab import sign


def rolling_vwap(close: pd.Series, volume: pd.Series, n: int) -> pd.Series:
    """Compute a rolling n-day volume weighted moving average.

    Args:
        close (pd.Series): Close price of stock.
        volume (pd.Series): Volume of stock traded.
        n (int): Period over wich rolling window is computed.

    Returns:
        pd.Series: VWAP
    """
    pv: pd.Series = close * volume
    return pv.rolling(n).sum() / volume.rolling(n).sum()


def compute_trade(signal: pd.Series, u_lim: float, l_lim: float) -> pd.Series:
    """_summary_

    Args:
        signal (pd.Series): _description_
        u_lim (float): _description_
        l_lim (float): _description_

    Returns:
        pd.Series: _description_
    """
    return pd.Series(
        np.select(
            condlist=[signal < l_lim, signal > u_lim],
            choicelist=[1, -1],
            default=0,
        ),
        index=signal.index,
        dtype=int,
        name="trade",
    )


def generate_signals(bars: pd.DataFrame, config: StrategyConfig) -> pd.DataFrame:
    close = bars["close"]
    volume = bars["volume"]

    vwap = rolling_vwap(close, volume, config.nlookback)

    signal = (close / vwap).fillna(1.0)
    trade = compute_trade(signal, config.u_lim, config.l_lim)

    return pd.DataFrame(
        {
            "close": close,
            "volume": volume,
            "vwap": vwap,
            "signal": signal,
            "trade": trade,
            "long_entry": trade == 1,
            "long_exit": trade != 1,
            "short_entry": trade == -1,
            "short_exit": trade != -1,
        }
    )


def signal_summary(signals: pd.DataFrame) -> None:
    trade = signals["trade"]
    total = len(trade)
    print(
        f"Signal breakdown ({total} bars) — "
        f"Long: {(trade == 1).sum()} "
        f"({(trade == 1).mean():.1%})  |  "
        f"Short: {(trade == -1).sum()} "
        f"({(trade == -1).mean():.1%})  |  "
        f"Flat: {(trade == 0).sum()} "
        f"({(trade == 0).mean():.1%})"
    )
