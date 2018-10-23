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
#'
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
