#' Populate key variables for CBLS tables
#'
#' This function populates values for the second through fifth variables (`ACTION` through `PGMID`) in each of the CBLS tables. A dataframe of one row is returned, which is then provided in the `key` argument of each CBLS table formatting function. Because the submission is quarterly, the function expects data from only one quarter of one year to be provided in `df`.
#'
#' @param df A dataframe of records ready for submission to CBLS.
#' @param action The action for each record in the formatted table, `"add"`, `"change"`, or `"delete"`.
#'
#' @return A dataframe with one row and four variables: `ACTION`, `QTR`, `RPT_YR`, and `PGMID`.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' df <- data.frame(
#'   lab_collection_date = sample(
#'     seq.Date(
#'       from = as.Date("2023-01-01"),
#'       to = as.Date("2023-03-31"),
#'       by = "day",
#'     ),
#'     size = 100
#'   )
#' )
#' cbls_key <- cbls_table_key(df, action = "add")
#'
cbls_table_key <- function(df, action = c("add", "change", "delete")) {
  # actions <- c("add", "change", "delete")
  #
  # if (!action %in% actions) {
  #   stop("`action` can only be \"add\", \"change\", or \"delete\"", call. = FALSE)
  # }

  var_check(df, var = "lab_collection_date")

  y <- get_year(df[["lab_collection_date"]])
  q <- get_quarter(df[["lab_collection_date"]])

  if (length(y) != 1 | length(q) != 1) {
    stop("df has dates from > 1 year and/or quarter", call. = FALSE)
  }

  data.frame(matrix(nrow = 1, ncol = 0)) %>%
    dplyr::mutate(
      ACTION = dplyr::case_when(
        action == "add" ~ "A",
        action == "change" ~ "C",
        action == "delete" ~ "D"
      ),
      QTR = q,
      RPT_YR = substr(y, 3, 4),
      PGMID = 20007
    )
}
