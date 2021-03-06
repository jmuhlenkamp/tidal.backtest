% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/backtest_execute.R
\name{backtest_update_nav}
\alias{backtest_update_nav}
\title{Unexported Subfunction(s) of backtest}
\usage{
backtest_update_nav(dt_fund, dt, idate)
}
\arguments{
\item{dt_fund}{data.table of fund-level information}

\item{dt}{data.table of holdings-level information}

\item{idate}{date to update}
}
\description{
Computes nav values for a given date share shares and prices
at various times (value, trade, close) within \code{dt}.
}
