#' Unexported Subfunction(s) of trade_weights
#'
#' Takes a set of daily prices and daily weights
#' and returns a list of initialized data ready to be traded.
#'
#' @param df_prices a data.frame containing the columns: symbol, date, price
#' @param df_weights a data.frame containing the columns: symbol, date, w
#' @param use_cash_long if TRUE use _CASH_LONG_ price within df_prices, else use 1 for all dates
#' @param use_cash_short if TRUE use _CASH_SHORT_ price within df_prices, else use 1 for all dates
#' @import data.table
#'
trade_weights_initialize <- function(df_prices, df_weights,
                                     use_cash_long, use_cash_short) {
    validate_df_columns(df_prices, c("symbol", "date", "price"))
    validate_df_columns(df_weights, c("symbol", "date", "w"))

    # Initialize various objects
    initial_fund_value <- 1000000
    symbols <- sort(unique(df_weights$symbol))
    dates <- sort(unique(df_prices[df_prices$symbol == 'XLB', 'date']))
    dates <- dates[dates >= min(df_weights$date)]
    min_date <- dates[1]

    # Convert input df to dt
    # Exclude extraneous data
    dt_prices <- as.data.table(as.data.frame(df_prices))[,.(symbol, date, price)]
    dt_weights <- as.data.table(as.data.frame(df_weights))[,.(symbol, date, w)]
    dt_prices <- dt_prices[symbol %in% symbols & date >= min_date]

    # Handle cash price series by either using 1.0 for all dates or
    # _CASH_LONG_ and/or _CASH_SHORT_ within df_prices input
    dt_prices[, is_cash:=FALSE]
    dt <- dt_prices
    if (use_cash_long) {
        dt[symbol == "_CASH_LONG_", is_cash:=TRUE]
    } else {
        dt <- rbind(dt,
                    data.table(symbol = '_CASH_LONG_',
                               date = dates,
                               price = 1.0,
                               is_cash = TRUE))
    }
    if (use_cash_short) {
        dt[symbol == "_CASH_SHORT_", is_cash:=TRUE]
    } else {
        dt <- rbind(dt,
                    data.table(symbol = '_CASH_SHORT_',
                               date = dates,
                               price = 1.0,
                               is_cash = TRUE))
    }

    # Multiply rows by 3 for value, trade and close times of days
    # Placeholder for more input price times used in future
    dt[,time:=character(.N)]
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

    return(list(dates=dates, dt=dt, dt_fund=dt_fund))
}
