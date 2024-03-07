#' Write data to an Excel workbook
#'
#' @description
#' This function produces an Excel workbook using the [openxlsx] package.
#'
#' @details
#' If `data` is a list of dataframes, each will be written to a separate sheet in a single workbook. Dataframes with 0 rows will be skipped.
#'
#' @param data A dataframe (or list of dataframes).
#' @param file A file path. `".xlsx"` will be appended if not present.
#' @param style_cols A character vector (or list of vectors) of variable names in `data` to highlight.
#' @param sheet A name (or vector of names) for the worksheet(s).
#' @param as_table Logical: format data as a table if `TRUE`.
#' @param overwrite Logical: overwrite existing file if `TRUE`.
#'
#' @return None.
#' @export
#'
#' @examples
#' \dontrun{
#' my_xl_table(mtcars, file = "mtcars_workbook", style_cols = c("mpg", "cyl"))
#' }
#'
my_xl_table <- function(data, file, style_cols = NULL, sheet = "Sheet1", as_table = TRUE, overwrite = TRUE) {
  if (is.data.frame(data)) {
    if (nrow(data) == 0) return(invisible(data))
    sheets <- 1
  } else if (all(unlist(lapply(data, is.data.frame)))) {
    sheets <- length(data)
  } else {
    stop("`data` must be a dataframe or list")
  }

  wb <- openxlsx::createWorkbook()

  for (i in 1:sheets) {
    if (!is.data.frame(data)) {
      tbl <- data[[i]]
      col_name <- style_cols[[i]]
    } else {
      tbl <- data
      col_name <- style_cols
    }

    if (nrow(tbl) == 0) next

    openxlsx::addWorksheet(wb = wb, sheetName = sheet[i])

    if (as_table) {
      openxlsx::writeDataTable(
        wb = wb, sheet = sheet[i], x = tbl,
        tableStyle = "TableStyleMedium2"
      )
      openxlsx::freezePane(wb = wb, sheet = sheet[i], firstRow = TRUE)
    } else {
      openxlsx::writeData(wb = wb, sheet = sheet[i], x = tbl)
    }

    # Format columns in `style_cols`
    if (!is.null(col_name)) {
      # Get column number(s) from `style_cols`
      col_num <- which(colnames(tbl) %in% col_name)

      style_fill <- openxlsx::createStyle(fgFill = "#ffd0d0")

      openxlsx::addStyle(
        wb = wb, sheet = sheet[i],
        style = style_fill,
        rows = 2:(nrow(tbl) + 1),
        cols = col_num,
        gridExpand = TRUE
      )
    }
  }

  # Add ".xlsx" if not included in the file name
  if (!stringr::str_detect(file, "(?i)\\.xlsx$")) {
    file <- paste0(file, ".xlsx")
  }

  tryCatch(
    {
      openxlsx::saveWorkbook(wb = wb, file = file, overwrite = overwrite)
    },
    error = function(e) {
      invisible(data)
    }
  )
}
