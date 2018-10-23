#' Daily Federal Funds Target Range - Upper Limit
#'
#' Daily Federal Funds Target Range - Upper Limit
#' from 2017-10-23 through 2018-10-22.
#'
#' @docType data
#'
#' @source Raw data was downloaded from FRED via quantmod::getSymbols()
#' and converted to R data.frame via the code shown in the examples.
#'
#' @examples
#'
#' print(fedfunds)
#'
#' # Code used to generate the fedfunds data.frame from quantmod::getSymbols()
#' \dontrun{
#' library(quantmod)
#' getSymbols("DFEDTARU", src = "FRED")
#' DFEDTARU <- DFEDTARU["20171023/20181022"]
#' fedfunds <- data.frame(symbol = "_FEDFUNDS_",
#'                        date = index(DFEDTARU),
#'                        price = unname(DFEDTARU),
#'                        stringsAsFactors = FALSE)
#' }
#'
#' @keywords datasets
"fedfunds"
