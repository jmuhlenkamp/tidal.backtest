#' @docType package
#' @name tidalbacktest
#' @title Package for Backtesting
#' @description Contains \code{\link{backtest}} function. \cr\cr
#'
#' @import data.table
#' @import tibble
#'
#' @export backtest
#'
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
                         "trade_shares",
                         "rate",
                         "g"
                         ))
