import logging

import pandas as pd
from alpaca.trading.client import TradingClient
from alpaca.trading.enums import OrderSide, TimeInForce
from alpaca.trading.requests import MarketOrderRequest
from config import StrategyConfig
from data import get_historical_bars
from signals import generate_signals

logger = logging.getLogger(__name__)


def _build_trading_client(config: StrategyConfig) -> TradingClient:
    return TradingClient(
        api_key=config.api_key,
        secret_key=config.secret_key,
        paper=config.paper,
    )


def get_current_position(client: TradingClient, symbol: str) -> int:
    """
    Return the current signed share quantity for *symbol*.

    Args:
        client (TradingClient): Alpaca trading client
        symbol (str): Stock symbol

    Returns:
        int: positive = long, negative = short, 0 = flat
    """
    try:
        pos = client.get_open_position(symbol)
        return int(pos.qty)
    except Exception:
        return 0


def get_account_equity(client: TradingClient) -> float:
    """Return current account equity in USD."""
    account = client.get_account()
    return float(account.equity)


def _submit_order(
    client: TradingClient,
    symbol: str,
    side: OrderSide,
    qty: int,
) -> None:
    """Submit a market order and log the result."""
    order = client.submit_order(
        MarketOrderRequest(
            symbol=symbol,
            qty=qty,
            side=side,
            time_in_force=TimeInForce.DAY,
        )
    )
    logger.info("Order submitted: %s %d %s  id=%s", side, qty, symbol, order.id)


def reconcile_position(
    client: TradingClient,
    config: StrategyConfig,
    desired: int,  # -1 / 0 / 1 (from signal)
    shares_per_unit: int,  # how many shares = one "unit"
) -> None:
    """Bring the actual Alpaca position in line with the desired signal.

    Handles long → flat, flat → short, long → short, etc.
    Only submits an order when the current state differs from desired.
    """
    current_qty = get_current_position(client, config.symbol)
    current_dir = 0 if current_qty == 0 else (1 if current_qty > 0 else -1)

    if current_dir == desired:
        logger.info("Position already correct (%+d). No action.", desired)
        return

    # Close existing position
    if current_qty != 0:
        close_side = OrderSide.SELL if current_qty > 0 else OrderSide.BUY
        _submit_order(client, config.symbol, close_side, abs(current_qty))

    # Open new position
    if desired == 1:
        _submit_order(client, config.symbol, OrderSide.BUY, shares_per_unit)
    elif desired == -1:
        _submit_order(client, config.symbol, OrderSide.SELL, shares_per_unit)
    # desired == 0: go flat; closing order above is sufficient


# Main live loop
def run_live_once(config: StrategyConfig, target_pct: float = 0.95) -> None:
    """
    Execute a single live signal check and order reconciliation.

    Call this function once per day after the market closes
    (e.g., via a cron job or a `schedule` loop in a separate runner script).

    Args:
        config (StrategyConfig): Strategy Config
        target_pct (float, optional): fraction of equity to deploy per unit. Defaults to 0.95.
    """
    trading_client = _build_trading_client(config)

    # 1. Pull latest bars (enough for the rolling VWAP window)
    bars = get_historical_bars(config)

    # 2. Compute signal on the full history; we only care about the latest bar
    signals = generate_signals(bars, config)
    latest_signal = int(signals["trade"].iloc[-1])
    latest_close = float(signals["close"].iloc[-1])
    logger.info(
        "Latest bar: date=%s  close=%.2f  signal=%+d",
        signals.index[-1].date(),
        latest_close,
        latest_signal,
    )

    # 3. Size the position: use target_pct of equity
    equity = get_account_equity(trading_client)
    shares = max(1, int((equity * target_pct) / latest_close))

    # 4. Reconcile
    reconcile_position(trading_client, config, latest_signal, shares)
