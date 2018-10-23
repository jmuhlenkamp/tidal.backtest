#' Unexported function(s) to munge fedfunds and related data
#'
#' Changes fedfunds style data into price style data.
#'
#' @param df data.frame containing columns: symbol, date, rate (annualized, e.g. 2.25).
#'
convert_rate_g <- function(df) {
    validate_df_columns(df, c("symbol", "date", "rate"))
    if (length(unique(df$symbol)) != 1) {
        stop("convert_rate_g function currently only supports a single symbol. ",
             "Input df can only contain one unique symbol.")
    }
    as.data.frame(as.data.table(df)[,g:=c(0, (1+rate[-.N]/100)^(1/365)-1)])
}
#'
#' Unexported function(s) to munge fedfunds and related data
#'
#' Changes fedfunds growth data into price style data.
#'
#' @param df data.frame containing columns: symbol, date, g (daily growth, e.g. 0.01 = 1 percent).
#'
convert_g_price <- function(df) {
    validate_df_columns(df, c("symbol", "date", "g"))
    if (length(unique(df$symbol)) != 1) {
        stop("convert_g_price function currently only supports a single symbol. ",
             "Input df can only contain one unique symbol.")
    }
    as.data.frame(as.data.table(df)[,price:=cumprod(1+g)])
}
#'
#' Unexported function(s) to munge fedfunds and related data
#'
#' Converts fedfunds symbol and rates into broker style symbols and rates
#'
#' @param df data.frame containing columns: symbol, date, rate (annualized, e.g. 2.25).
#' @param new_symbol character string to use as symbol column on returned data.frame.
#' @param multiplier -1 or 1 depending on whether short or long rate.
#' @param add_rate Annualized rate to add to input rate (e.g. 0.25 = +25 basis points).
#'
convert_fedfunds_broker <- function(df, new_symbol, multiplier, add_rate) {
    as.data.frame(as.data.table(df)[,`:=`(symbol=new_symbol,
                                          rate=multiplier*ifelse(rate+add_rate < 0, 0,
                                                                 rate+add_rate))])
}
#'
#' Unexported function(s) to munge fedfunds and related data
#'
#' Takes a benchmark interest rate like fedfunds and return broker style
#' cash assets with symbols "_CASH_LONG_" and "_CASH_SHORT_" in the format
#' that trade_weights() requires.
#'
#' @param df data.frame containing columns: symbol, date, rate (annualized, e.g. 2.25).
#' @param add_rate_long Annualized rate to add to input rate to create annualized rate
#'                      paid on cash balances(e.g. -0.50 = -25 basis points).
#' @param add_rate_short Annualized rate to add to input rate to create annualized rate
#'                       charge on margin balances(e.g. 0.25 = +25 basis points).
#'
create_cash_assets <- function(df, add_rate_long = -0.50, add_rate_short = 0.25) {
    rbind(
        convert_g_price(convert_rate_g(
            convert_fedfunds_broker(df, "_CASH_LONG_", 1, add_rate_long))),
        convert_g_price(convert_rate_g(
            convert_fedfunds_broker(df, "_CASH_SHORT_", -1, add_rate_short)))
    )
}
