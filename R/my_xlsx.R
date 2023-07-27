#' Save XLSX file with custom settings using `write.xlsx()`
#'
#' Save a dataframe as an XLSX file with table formatting using [openxlsx::write.xlsx()].
#'
#' @param x A dataframe or a list of dataframes.
#' @param file A file path.
#'
#' @return An XLSX file.
#' @export
#'
# @examples
#'
my_xlsx <- function(x, file) {
  if (is.data.frame(x)) {
    if (nrow(x) == 0) return(invisible(x))
  } else if (is.list(x) && all(sapply(x, is.data.frame))) {
    if (all(sapply(x, nrow) == 0)) return(invisible(x))
  } else {
    stop("`x` must be a dataframe or a list of dataframes", call. = FALSE)
  }

  openxlsx::write.xlsx(
    x = x,
    file = file,
    asTable = TRUE,
    tableStyle = "TableStyleMedium2",
    firstRow = TRUE
  )
}
