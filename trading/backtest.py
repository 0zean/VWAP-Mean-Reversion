import pandas as pd
import plotly.graph_objects as go
import vectorbt as vbt
from config import StrategyConfig


# Portfolio construction
def build_strategy_portfolio(
    signals: pd.DataFrame,
    config: StrategyConfig,
) -> vbt.Portfolio:
    """
    Construct the VWAP mean reversion portfolio from entry/exit masks.

    Args:
        signals (pd.DataFrame): output of signals.generate_signals()
        config (StrategyConfig): StrategyConfig (provides init_cash, fees)

    Returns:
        vbt.Portfolio: Strategy portfolio.
    """
    return vbt.Portfolio.from_signals(
        close=signals["close"],
        entries=signals["long_entry"],
        exits=signals["long_exit"],
        short_entries=signals["short_entry"],
        short_exits=signals["short_exit"],
        init_cash=config.init_cash,
        fees=config.fees,
        freq="D",
    )


def build_benchmark_portfolio(
    signals: pd.DataFrame,
    config: StrategyConfig,
) -> vbt.Portfolio:
    """Buy-and-hold benchmark with identical cash and fee assumptions."""
    close = signals["close"]

    entry_mask = pd.Series(False, index=close.index)
    entry_mask.iloc[0] = True  # buy first bar, hold forever

    return vbt.Portfolio.from_signals(
        close=close,
        entries=entry_mask,
        exits=pd.Series(False, index=close.index),
        init_cash=config.init_cash,
        fees=config.fees,
        freq="D",
    )


# Reporting
def print_comparison(
    pf: vbt.Portfolio,
    bnh: vbt.Portfolio,
    config: StrategyConfig,
) -> None:
    """Print a side-by-side performance summary.

    Passes buy-and-hold returns as the benchmark to vectorbt so that
    the Alpha and Beta rows are populated automatically.
    """
    sep = "=" * 62
    print(f"\n{sep}")
    print(f"  {config.symbol} VWAP Strategy  vs  Buy-and-Hold")
    print(f"{sep}\n")

    print(f"[{config.symbol} VWAP Strategy]")
    print(pf.stats())

    print(f"\n[{config.symbol} Buy-and-Hold]")
    print(bnh.stats())


# Plotting
def plot_comparison(
    pf: vbt.Portfolio,
    bnh: vbt.Portfolio,
    config: StrategyConfig,
) -> None:
    """Overlay strategy and benchmark portfolio values in a Plotly chart."""
    strat_val = pf.value().rename(f"{config.symbol} VWAP Strategy")
    bnh_val = bnh.value().rename(f"{config.symbol} Buy-and-Hold")

    fig = go.Figure()
    for series, color in [
        (strat_val, "#1f77b4"),
        (bnh_val, "#ff7f0e"),
    ]:
        fig.add_trace(
            go.Scatter(
                x=series.index,
                y=series.values,
                name=series.name,
                line={"color": color, "width": 1.5},
            )
        )

    fig.update_layout(
        title=f"Portfolio Value — {config.symbol} VWAP vs Buy-and-Hold",
        xaxis_title="Date",
        yaxis_title=f"Value ($, starting ${config.init_cash:,.0f})",
        legend={"orientation": "h", "y": -0.15},
        hovermode="x unified",
    )
    fig.show()


def plot_strategy_dashboard(pf: vbt.Portfolio) -> None:
    """Render vectorbt's built-in interactive strategy dashboard."""
    pf.plot().show()
