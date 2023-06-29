#' Create a CBLS FWF for submission
#'
#' @param df A dataframe returned by one of the CBLS table functions and checked by [cbls_check_table()].
#' @param path A string consisting of a file path and file name. Must end with `".txt"`.
#'
#' @return A fixed-width file written by [gdata::write.fwf()] exported to the path specified.
#' @export
#'
# @examples
#'
cbls_write_fwf <- function(df, path) {
  var_check(df, var = c("FILEID", "ACTION", "QTR", "RPT_YR", "PGMID"))

  if (substr(path, nchar(path) - 3, nchar(path)) != ".txt") {
    stop("`path` must end with \".txt\"", call. = FALSE)
  }

  df <- df %>%
    dplyr::select(FILEID:tidyselect::last_col())

  gdata::write.fwf(
    df,
    file = path,
    sep = "",
    colnames = FALSE,
    scientific = FALSE
  )
}
