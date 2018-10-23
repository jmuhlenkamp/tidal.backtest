#' Unexported Subfunction(s) of trade_weights
#'
#' Takes a set of daily prices and daily weights
#' and returns a list of initialized data ready to be traded.
#'
#' @param df_prices a data.frame containing the columns: symbol, date, price
#' @param df_weights a data.frame containing the columns: symbol, date, w
#' @import data.table
#'
trade_weights_initialize <- function(df_prices, df_weights) {
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

    return(list(dates=dates, dt=dt, dt_fund=dt_fund))
}
#'
#' Unexported Subfunction(s) of trade_weights
#'
#' Computes nav values for a given date share shares and prices
#' at various times (value, trade, close) within \code{dt}.
#'
#' @param dt_fund data.table of fund-level information
#' @param dt data.table of holdings-level information
#' @param idate date to update
#' @import data.table
#'
trade_weights_update_nav <- function(dt_fund, dt, idate) {
    dt_fund[date == idate, `:=`(
        nav_value=sum(dt[date == idate, .(tmp=shares_value * price_value)][,tmp]),
        nav_trade=sum(dt[date == idate, .(tmp=shares_trade * price_trade)][,tmp]),
        nav_close=sum(dt[date == idate, .(tmp=shares_close * price_close)][,tmp])
    )]
}
#' Unexported Subfunction(s) of trade_weights
#'
#' Takes the list returned from trade_weights_initialize()
#' and executes the daily trades.
#'
#' @param list_data list returned from trade_weights_initialize()
#' @import data.table
#'
trade_weights_execute <- function(list_data) {
    dates <- list_data$dates
    dt <- list_data$dt
    dt_fund <- list_data$dt_fund

    first <- TRUE
    for (idate in sort(dates)) {
        # Carry forward the shares_close from lag_idate to shares_value of idate
        if (!first) {
            dt[,lag_shares_close:=c(NA, shares_close[-.N]), by=c("symbol")]
            dt[date == idate,shares_value:=lag_shares_close]
        }

        trade_weights_update_nav(dt_fund, dt, idate)
        fv <- dt_fund[date == idate, nav_value]

        # Update non-cash trade_shares
        dt[is_cash==FALSE & date == idate, target_dollars:=w * fv]
        dt[is_cash==FALSE & date == idate, trade_dollars:=target_dollars - shares_value * price_value]
        dt[is_cash==FALSE & date == idate, trade_shares:=trade_dollars / price_value]

        # Determine cash changes in dollars (will later handle shares)
        net_purchases <- dt[is_cash==FALSE & date == idate, sum(trade_shares * price_trade)]
        cash_long <- dt[symbol == '_CASH_LONG_' & date == idate, shares_value * price_trade]
        cash_short <- dt[symbol == '_CASH_SHORT_' & date == idate, shares_value * price_trade]
        updated_cash_long <- max(c(0, cash_long - net_purchases))
        updated_cash_short <- -1 * min(c(0, cash_long - net_purchases))

        # Handle shares
        dt[symbol == '_CASH_LONG_' & date == idate, target_dollars:=updated_cash_long]
        dt[symbol == '_CASH_SHORT_' & date == idate, target_dollars:=updated_cash_short]
        dt[is_cash==TRUE & date == idate, trade_dollars:=target_dollars - shares_value * price_value]
        dt[is_cash==TRUE & date == idate, trade_shares:=trade_dollars / price_value]

        # Trade shares
        dt[date == idate, shares_trade:=shares_value + trade_shares]
        dt[date == idate, shares_close:=shares_trade]
        trade_weights_update_nav(dt_fund, dt, idate)

        lag_idate <- idate
        first <- FALSE
    }
    # Prepare data for returning to user
    dt_fund <- dt_fund[,.(date, nav_close)]
    setnames(dt_fund, old='nav_close', new='nav')
    df_fund <- as.data.frame(dt_fund)
    return(list(df_fund=df_fund))
}
