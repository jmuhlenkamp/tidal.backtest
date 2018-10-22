#' Trade a Set of Daily Weights
#'
#' Takes a set of daily prices and daily weights
#' and returns a daily NAV
#'
#' @param df_prices a data.frame containing the columns: symbol, date, price
#' @param df_weights a data.frame containing the columns: symbol, date, w
#' @export

trade_weights <- function(df_prices, df_weights) {
    validate_df_columns(df_prices, c("symbol", "date", "price"))
    validate_df_columns(df_weights, c("symbol", "date", "w"))
}
