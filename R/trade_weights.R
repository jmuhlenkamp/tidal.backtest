#' Trade a Set of Daily Weights
#'
#' Takes a set of daily prices and daily weights
#' and returns a daily NAV
#'
#' @param df_prices a data.frame containing the columns: symbol, date, price
#' @param df_weights a data.frame containing the columns: symbol, date, w
#' @import data.table
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

    # Initialize various objects
    initial_fund_value <- 1000000
    symbols <- sort(unique(df_weights$symbol))
    dates <- sort(unique(df_prices$date))
    dates <- dates[dates >= min(df_weights$date)]
    min_date <- dates[1]

    # Convert input df to dt
    # Exclude extraneous data
    dt_prices <- as.data.table(as.data.frame(df_prices))
    dt_weights <- as.data.table(as.data.frame(df_weights))
    dt_prices <- dt_prices[symbol %in% symbols & date >= min_date]

    # Add is_cash column
    # Add rows to include _CASH_LONG_ and _CASH_SHORT_ for all dates
    dt_prices[, is_cash:=FALSE]
    dt <- rbind(dt_prices,
                data.table(symbol = '_CASH_LONG_',
                           date = dates,
                           price = 1.0,
                           is_cash = TRUE),
                data.table(symbol = '_CASH_SHORT_',
                           date = dates,
                           price = 1.0,
                           is_cash = TRUE))[
                               ,time:=character(.N)]

    # Multiply rows by 3 for value, trade and close times of days
    # Placeholder for more input price times used in future
    dt <- rbind(dt, dt, dt)
    dt[, time:=c(rep('value',.N/3),rep('trade',.N/3),rep('close',.N/3))]
    dt[, `:=`(shares=numeric(.N))]

    # Merge weights onto core dt
    setkey(dt, symbol, date, time, is_cash)
    setkey(dt_weights, symbol, date)
    dt <- dt_weights[dt]
    dt[is.na(w), w:=0]

    # Initialize holdings in cash
    initial_cash_price <-
        dt[symbol == '_CASH_LONG_' &
               date == min_date &
               time == 'value', price]
    dt[symbol == '_CASH_LONG_' &
           date == min_date &
           time == 'value',
       shares := initial_fund_value / initial_cash_price]

    # Cast core dt into a wide format for computation and readability ease
    dt <- data.table::dcast(dt[,.(is_cash, symbol, date, w, time, price, shares)],
                is_cash+symbol+date+w~time, value.var=c('price', 'shares'))
    setkey(dt, is_cash, symbol, date)

    # Initialize dt_fund for fund level info
    dt_fund <- unique(dt[, .(date)])[, `:=`(
        nav_value=numeric(.N),
        nav_trade=numeric(.N),
        nav_close=numeric(.N)
    )]
    setkey(dt_fund, date)

    return(dt_fund)
    # Start doing stuff
}
