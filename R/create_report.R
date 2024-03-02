#' Create a report spreadsheet
#'
#' This function wraps around [my_xl_table()] to create a report spreadsheet in Excel. It adds a column called "action" and suppresses the error returned by `openxlsx::saveWorkbook` when the file path already exists and `overwrite == FALSE`.
#'
#' @param df A dataframe.
#' @param file A file path. `".xlsx"` will be appended if not present.
#' @param style_cols A vector of variable names in `df` to highlight.
#' @param overwrite Logical: overwrite existing file if `TRUE`.
#'
#' @return An Excel workbook.
#' @export
#'
# @examples
#'
create_report <- function(df, file, style_cols, overwrite) {
  # Check data type of `df`
  if (is.data.frame(df)) {
    if (nrow(df) == 0) return(invisible(df))
  } else {
    stop("`df` must be a dataframe")
  }

  # Add `action` column to `df`
  df <- df %>%
    dplyr::mutate(action = NA) %>%
    dplyr::relocate(action)

  # Include `action` in list of columns to highlight
  style_cols <- c("action", style_cols)

  tryCatch(
    {
      my_xl_table(df, file = file, style_cols = style_cols, overwrite = overwrite)
    },
    error = function(e) {
      invisible(df)
    }
  )
}
