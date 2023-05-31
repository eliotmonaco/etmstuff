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
#' @family undupe functions
#' @examples
#' n_rows <- 20
#' df <- data.frame(
#'   x = sample(c("cat", "horse", "howler monkey"), size = n_rows, replace = TRUE),
#'   y = sample(c(1, 10, 100, NA), size = n_rows, replace = TRUE),
#'   z = sample(c("banana", "carrot", "pickle"), size = n_rows, replace = TRUE)
#' )
#' undupe <- undupe(df, visible_var = c("x", "y"))
#' f <- ""
#' p <- ""
#' \dontrun{
#' dupes2xl(undupe[["df_dupesets"]], filename = f, path = p)
#' }
#'
dupes2xl <- function(df, filename, path = NULL, visible = FALSE) {
  xl_path <- paste0(path, filename, ".xlsm")
  # xl_path <- paste0(gsub("/", "\\\\", getwd()), "\\", gsub("/", "\\\\", path), filename, ".xlsm")

  # Open macro template file and load data
  wb <- openxlsx::loadWorkbook(paste0(path, "dupes2xl_template.xlsm"))
  openxlsx::addWorksheet(wb, "Dupesets")
  openxlsx::writeData(wb, "Dupesets", df)
  openxlsx::removeWorksheet(wb, "Sheet1")
  openxlsx::saveWorkbook(wb, paste0(path, filename, ".xlsm"))

  # Open connection to Excel and open file
  xl_app <- RDCOMClient::COMCreate("Excel.Application")
  xl_wb <- xl_app$Workbooks()$Open(xl_path)

  # View workbook (optional - may slow down format_dupesets macro)
  if (visible) {
    xl_app[["Visible"]] <- TRUE
  }

  # Run macros
  xl_app$Run("ThisWorkbook.create_table")
  xl_app$Run("ThisWorkbook.format_dupesets")

  # Close and save
  xl_wb$Close(SaveChanges = TRUE)
  xl_app$Quit()

  # Release resources
  rm(xl_wb, xl_app)
  gc()
}
