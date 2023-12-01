#' Save XLSX file with custom settings using `write.xlsx()`
#'
#' Save an object (a dataframe or list) as an XLSX file with custom table formatting using [openxlsx::write.xlsx()]. If `x` is a list of dataframes, each dataframe will be saved as a separate sheet. The file extension ".xlsx" is added if it is not found at the end of `file`.
#'
#' @param x A dataframe or list of dataframes.
#' @param file A file path.
#' @param ... Additional arguments passed to [openxlsx::write.xlsx()]
#'
#' @return An XLSX file.
#' @export
#'
#' @examples
#' \dontrun{
#' my_xlsx(mtcars, "mtcars.xlsx")
#' }
#'
my_xlsx <- function(x, file, ...) {
  # Check data type of `x`
  if (is.data.frame(x)) {
    if (nrow(x) == 0) return(invisible(x))
  } else if (is.list(x) && all(sapply(x, is.data.frame))) {
    if (all(sapply(x, nrow) == 0)) return(invisible(x))
  } else {
    stop("`x` must be a dataframe or a list of dataframes")
  }

  # Add ".xlsx" if not included in the file name
  if (!stringr::str_detect(file, stringr::regex(".xlsx$", ignore_case = TRUE))) {
    file <- paste0(file, ".xlsx")
  }

  openxlsx::write.xlsx(
    x = x,
    file = file,
    asTable = TRUE,
    tableStyle = "TableStyleMedium2",
    firstRow = TRUE,
    ...
  )
}
