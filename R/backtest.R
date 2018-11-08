#' Backtest a Set of Daily Weights
#'
#' Backtest function that returns a daily NAV dataset
#'
#' @param data a data.frame containing the columns: symbol, date, w
#' @param rebal_group_by Optional Character indicating by group to apply to ith_date.
#'                       Acceptable values are: date, week, month, qtr.  Default is date
#'                       which performs a daily rebalance.
#' @param rebal_ith Optional Integer to determine which observation of each by group to return.
#'                 For Example ith_date = 2, week will return the second day of
#'                 each week.  Also, ith_date = 99, month will return last day of
#'                 each month.  Default is 1.
#' @param prices a data.frame containing the columns: symbol, date, price.
#'   Default is tidalprices::dailyclose.
#'
#' @import data.table
#' @export
#' @examples
#' \dontrun{
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
#' df_nav <- backtest(df_close, df_weights)
#' }

backtest <- function(
    data,
    rebal_group_by = "date",
    rebal_ith = 1,
    prices = tidalprice::dailyclose)
{
    rebal_ith_date <- 1
    if (rebal_ith_date != 1) {
        stop("Only rebal_ith_date = 1 is currently supported.")
    }
    has_cash_long <- as.logical(nrow(df_prices[df_prices$symbol == "_CASH_LONG_",]))
    has_cash_short <- as.logical(nrow(df_prices[df_prices$symbol == "_CASH_SHORT_",]))
    if (has_cash_long) {
        msg_cash_long <- "Broker charged rates within price data"
    } else {
        msg_cash_long <- "None (i.e. 0%)"
    }
    if (has_cash_short) {
        msg_cash_short <- "Broker charged rates within price data"
    } else {
        msg_cash_short <- "None (i.e. 0%)"
    }

    message("Assumptions Used for Backtest...") # eventually stick this in output list
    message("Cash Interest Earned   : ", msg_cash_long)
    message("Margin Interest Charged: ", msg_cash_short)
    message("Rebalance              : Day ", rebal_ith, " of each ", rebal_group_by)
    message("Trade Costs            : None")

    list_initialized <- backtest_initialize(
        prices, data, has_cash_long, has_cash_short)
    list_initialized$rebaldates <-
        backtest_initialize_rebaldates(list_initialized$dates, rebal_ith, rebal_group_by)

    list_executed <- backtest_execute(list_initialized)

    return(list_executed$df_fund)
}
