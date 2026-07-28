import logging

from backtest import (
    build_benchmark_portfolio,
    build_strategy_portfolio,
    plot_comparison,
    plot_strategy_dashboard,
    print_comparison,
)
from config import StrategyConfig
from data import get_historical_bars
from signals import generate_signals, signal_summary

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s  %(levelname)-8s  %(message)s",
    datefmt="%H:%M:%S",
)
logger = logging.getLogger(__name__)


def main() -> None:
    # 1. Configuration
    config = StrategyConfig()

    logger.info(
        "Running VWAP back-test  symbol=%s  nlookback=%d  "
        "l_lim=%.4f  u_lim=%.4f  start=%s",
        config.symbol,
        config.nlookback,
        config.l_lim,
        config.u_lim,
        config.start_date,
    )

    # 2. Data
    logger.info("Fetching bars from Alpaca …")
    bars = get_historical_bars(config)
    logger.info(
        "%d daily bars loaded  (%s → %s)",
        len(bars),
        bars.index[0].date(),
        bars.index[-1].date(),
    )

    # 3. Signals
    signals = generate_signals(bars, config)
    signal_summary(signals)

    # 4. Portfolios
    logger.info("Running vectorbt back-test …")
    pf = build_strategy_portfolio(signals, config)
    bnh = build_benchmark_portfolio(signals, config)

    # 5. Results
    print_comparison(pf, bnh, config)
    plot_comparison(pf, bnh, config)
    plot_strategy_dashboard(pf)


if __name__ == "__main__":
    main()
