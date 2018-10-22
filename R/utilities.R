#' Unexported Utility Functions
#'
#' Utility functions that do behind the scences work
#'
#' @param df a data.frame to validate
#' @param columns a character vector of columns that must be present in the data.frame

validate_df_columns <- function(df, columns) {
    chr_df <- deparse(substitute(df))
    chr_columns <- paste(columns, collapse = ", ")
    if (!('data.frame' %in% class(df))) {
        stop(paste("Argument", df, "must be a data.frame."))
    }
    if (sum(colnames(df) %in% columns) != length(columns)) {
        stop(paste(chr_df, "must contain the columns", chr_columns))
    }
}
