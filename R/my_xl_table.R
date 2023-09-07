#' Save a dataframe to an Excel workbook with custom formatting
#'
#' This function produces an Excel workbook with the dataframe `df` formatted as a table. Variable names passed to `style_cols` will be highlighted in red. If `nrow(df) == 0`, `df` will be returned invisibly without producing an Excel workbook.
#'
#' `my_xl_table()` is built on functions from the [openxlsx] package.
#'
#' @param df A dataframe.
#' @param file A file path. ".xlsx" will be added if not present.
#' @param style_cols A vector of variable names in `df` to highlight.
#' @param as_table Logical: format data as a table if `TRUE`.
#' @param sheet A name for the worksheet.
#'
#' @return An Excel workbook.
#' @export
#'
#' @examples
#' \dontrun{
#' my_xl_table(mtcars, file = "mtcars_workbook", style_cols = c("mpg", "cyl"))
#' }
#'
my_xl_table <- function(df, file, style_cols = NULL, as_table = TRUE, sheet = "Sheet1") {
  # Check data type of `df`
  if (is.data.frame(df)) {
    if (nrow(df) == 0) return(invisible(df))
  } else {
    stop("`df` must be a dataframe", call. = FALSE)
  }

  # Add ".xlsx" if not included in the file name
  if (!stringr::str_detect(file, stringr::regex(".xlsx$", ignore_case = TRUE))) {
    file <- paste0(file, ".xlsx")
  }

  if (!is.null(style_cols)) {
    var_check(df, var = style_cols)

    # Get column number(s) from `style_cols`
    cols <- which(colnames(df) %in% style_cols)
  }

  wb <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(wb = wb, sheetName = sheet)

  if (as_table) {
    openxlsx::writeDataTable(
      wb = wb, sheet = sheet, x = df,
      tableStyle = "TableStyleMedium2"
    )
    openxlsx::freezePane(wb = wb, sheet = sheet, firstRow = TRUE)
  } else {
    openxlsx::writeData(wb = wb, sheet = sheet, x = df)
  }

  # Format columns in `style_cols`
  if (!is.null(style_cols)) {
    style_fill <- openxlsx::createStyle(fgFill = "#ffd0d0")

    openxlsx::addStyle(
      wb = wb, sheet = sheet,
      style = style_fill,
      rows = 2:(nrow(df) + 1),
      cols = cols,
      gridExpand = TRUE
    )
  }

  openxlsx::saveWorkbook(wb = wb, file = file, overwrite = TRUE)
}
