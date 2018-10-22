#' Trade a Set of Daily Weights
#'
#' Takes a set of daily prices and daily weights
#' and returns a daily NAV
#'
#' @param df_prices a data.frame containing the columns: symbol, date, price
#' @param df_weights a data.frame containing the columns: symbol, date, w
#' @export
#' @examples
#' library(quantmod)
#'
#' # Create a data.frame of adjusted close prices
#' getSymbolsData <- new.env()
#' getSymbols(c('XLB', 'XLE', 'XLF'),
#'            env = getSymbolsData,
#'            from = as.Date("2018-09-03"),
#'            to = as.Date("2018-09-29"))
#' df_close <- dplyr::bind_rows(
#'     lapply(getSymbolsData, function(x)
#'         data.frame(date = index(x), price = unname(Ad(x)))), .id = 'symbol')
#'
#' # Create a data.frame of weights
#' df_weights <- df_close
#' df_weights$w <- 0.32
#' df_weights$price <- NULL
#'
#' # Trade the weights
#' df_nav <- trade_weights(df_close, df_weights)

trade_weights <- function(df_prices, df_weights) {
    validate_df_columns(df_prices, c("symbol", "date", "price"))
    validate_df_columns(df_weights, c("symbol", "date", "w"))
}
