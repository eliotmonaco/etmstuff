#' Flag lead tests for elevated results
#'
#' This function determines whether or not the test result is elevated according to the blood lead reference value.
#'
#' @param df A dataframe of lead test records.
#' @param blrv Numeric: the blood lead reference value (mcg/dL) in effect during the date range of the data provided in `df`. A blood lead test result >= `blrv` is elevated.
#'
#' @return A dataframe with the new variables `bl_ref_val` and `lab_result_elev`.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
# @examples
#'
flag_elevated_results <- function(df, blrv) {
  var_check(df, var = c("lab_result_symbol", "lab_result_number"))

  df %>%
    dplyr::mutate(
      lab_result_elev = dplyr::case_when( # TRUE = elevated BLL; FALSE = non-elevated BLL
        is.na(.data$lab_result_symbol) & .data$lab_result_number < blrv ~ FALSE,
        is.na(.data$lab_result_symbol) & .data$lab_result_number >= blrv ~ TRUE,
        .data$lab_result_symbol == "<" & .data$lab_result_number <= blrv ~ FALSE,
        .data$lab_result_symbol == "<" & .data$lab_result_number > blrv ~ NA,
        .data$lab_result_symbol == ">" & .data$lab_result_number < blrv ~ NA,
        .data$lab_result_symbol == ">" & .data$lab_result_number >= blrv ~ TRUE
      ),
      bl_ref_val = blrv
    )
}
