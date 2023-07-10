#' Export dupesets to an Excel file for easy viewing
#'
#' This function saves a dataframe of dupesets to an existing macro-enabled Excel workbook. This workbook contains VBA code to format the data, making dupesets easy to differentiate and value differences easy to detect.
#'
#' @param df A dataframe of dupesets returned by [undupe()].
#' @param path_new The path and file name for the new Excel workbook. Must end in `".xlsm"`.
#' @param path_template The path and file name for the Excel workbook to be copied.
#' @param visible A logical value: view the workbook while the macros run if `TRUE`.
#'
#' @return Nothing is returned in R. An Excel workbook is created using the `filename` and `path` provided as function arguments.
#' @export
#'
#' @family undupe functions
#' @examples
#' n_rows <- 20
#'
#' df <- data.frame(
#'   x = sample(c("cat", "horse", "howler monkey"), size = n_rows, replace = TRUE),
#'   y = sample(c(1, 10, 100, NA), size = n_rows, replace = TRUE),
#'   z = sample(c("banana", "carrot", "pickle"), size = n_rows, replace = TRUE)
#' )
#'
#' undupe <- undupe(df, visible_var = c("x", "y"))
#'
#' p1 <- "new.xlsm"
#' p2 <- "dupes2xl_template.xlsm"
#'
#' \dontrun{
#' dupes2xl(undupe[["df_dupesets"]], path_new = p1, path_template = p2)
#' }
#'
dupes2xl <- function(df, path_new, path_template, visible = FALSE) {
  # xl_path <- paste0(path, filename, ".xlsm")
  # xl_path <- paste0(gsub("/", "\\\\", getwd()), "\\", gsub("/", "\\\\", path), filename, ".xlsm")

  # Open macro template file and load data
  wb <- openxlsx::loadWorkbook(path_template)
  openxlsx::addWorksheet(wb, "Dupesets")
  openxlsx::writeData(wb, "Dupesets", df)
  openxlsx::removeWorksheet(wb, "Sheet1")
  openxlsx::saveWorkbook(wb, path_new)

  # Open connection to Excel and open file
  xl_app <- RDCOMClient::COMCreate("Excel.Application")
  xl_wb <- xl_app$Workbooks()$Open(path_new)

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
