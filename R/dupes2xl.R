#' Export dupesets to an Excel file for easy viewing
#'
#' This function saves a dataframe of dupesets to an existing macro-enabled Excel workbook. This workbook contains VBA code to format the data, making dupesets easy to differentiate and value differences easy to detect.
#'
#' @param df A dataframe of dupesets returned by [undupe()].
#' @param filename A name for the new Excel workbook.
#' @param path The folder path containing the Excel workbook to be used as a template.
#' @param visible A logical value: view the workbook while the macros run if `TRUE`.
#'
#' @return Nothing is returned in R. An Excel workbook is created using the `filename` and `path` provided as function arguments.
#' @export
#'
#' @importFrom openxlsx addWorksheet loadWorkbook removeWorksheet saveWorkbook writeData
#' @importFrom RDCOMClient COMCreate createCOMReference
#'
#' @family undupe functions
#' @examples
#' n_rows <- 20
#' df <- data.frame(
#'   x = sample(c("cat", "horse", "howler monkey"), size = n_rows, replace = TRUE),
#'   y = sample(c(1, 10, 100, NA), size = n_rows, replace = TRUE),
#'   z = sample(c("banana", "carrot", "pickle"), size = n_rows, replace = TRUE)
#' )
#' undupe <- undupe(df, undupe_vars = c("x", "y"))
#' f <- ""
#' p <- ""
#' \dontrun{
#' dupes2xl(undupe[["df_dupesets"]], filename = f, path = p)
#' }
dupes2xl <- function(df, filename, path = NULL, visible = FALSE) {
  xlpath <- paste0(path, filename, ".xlsm")
  # xlpath <- paste0(gsub("/", "\\\\", getwd()), "\\", gsub("/", "\\\\", path), filename, ".xlsm")

  # Open macro template file and load data
  wb <- loadWorkbook(paste0(path, "dupes2xl_template.xlsm"))
  addWorksheet(wb, "Dupesets")
  writeData(wb, "Dupesets", df)
  removeWorksheet(wb, "Sheet1")
  saveWorkbook(wb, paste0(path, filename, ".xlsm"))

  # Open connection to Excel and open file
  xlApp <- COMCreate("Excel.Application")
  xlWbk <- xlApp$Workbooks()$Open(xlpath)

  # View workbook (optional - may slow down format_dupesets macro)
  if (visible) {
    xlApp[["Visible"]] <- TRUE
  }

  # Run macros
  xlApp$Run("ThisWorkbook.create_table")
  xlApp$Run("ThisWorkbook.format_dupesets")

  # Close and save
  xlWbk$Close(SaveChanges = TRUE)
  xlApp$Quit()

  # Release resources
  rm(xlWbk, xlApp)
  gc()
}
