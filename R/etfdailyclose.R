#' Daily Close Prices for XLB, XLE, and XLF
#'
#' Daily close prices for XLB, XLE, and XLF from 2017-10-23
#' through 2018-10-22.  Data is in format required for trade_weights().
#'
#' @docType data
#'
#' @source Raw .csv files were downloaded from Yahoo Finances and then
#' converted to R data.frame via the code shown in the examples.
#'
#' @examples
#'
#' print(etfdailyclose)
#'
#' # Code used to generate the data.frame from raw .csv files
#' \dontrun{
#' library(dplyr)
#' library(readr)
#' etfdailyclose <- bind_rows(
#'     read_csv('XLB.csv') %>% mutate(symbol = 'XLB'),
#'     read_csv('XLE.csv') %>% mutate(symbol = 'XLE'),
#'     read_csv('XLF.csv') %>% mutate(symbol = 'XLF')) %>%
#'     transmute(symbol, date = Date, price = round(Close, 2)) %>%
#'     as.data.frame()
#' }
#'
#' @keywords datasets
"etfdailyclose"
