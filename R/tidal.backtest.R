#' tidal.backtest: Function(s) for Fund Level Backtesting
#'
#' Generate daily NAV backtest results quickly and easily.
#'
#' @docType package
#' @name tidal.backtest
"_PACKAGE"
utils::globalVariables(c(".",
                         "is_cash",
                         "w",
                         "symbol",
                         "price",
                         "price_value",
                         "price_trade",
                         "price_close",
                         "shares",
                         "shares_value",
                         "shares_trade",
                         "shares_close",
                         "nav_value",
                         "nav_trade",
                         "nav_close",
                         "lag_shares_close",
                         "target_dollars",
                         "time",
                         "tmp",
                         "trade_dollars",
                         "trade_shares"
                         ))
