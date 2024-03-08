#' Assign elevated status to lead tests
#'
#' @description
#' This function determines whether or not a test result is elevated according to the blood lead reference value (BLRV) at the time of the blood sample collection.
#'
#' @details
#' The BLRV is 3.5 μg/dL for any test collected on or after 2022-01-01. Prior to that, the BLRV was 5 μg/dL.
#'
#' @param df A dataframe of cleaned, deduplicated lead test records.
#'
#' @return A dataframe with the new variables `blrv` and `elevated_status`.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
# @examples
#'
assign_elevated_status <- function(df) {
  if (!is.data.frame(df)) stop("`df` must be a dataframe")
  var_check(df, var = c("result_sign", "result_number"))

  df %>%
    dplyr::mutate(
      blrv = dplyr::case_when(
        collection_date < as.Date("2022-01-01") ~ 5,
        collection_date >= as.Date("2022-01-01") ~ 3.5
      ),
      elevated_status = dplyr::case_when( # TRUE = elevated BLL; FALSE = non-elevated BLL
        is.na(.data$result_sign) & .data$result_number < .data$blrv ~ FALSE,
        .data$result_sign == "<" & .data$result_number <= .data$blrv ~ FALSE,
        is.na(.data$result_sign) & .data$result_number >= .data$blrv ~ TRUE,
        .data$result_sign == ">" & .data$result_number >= .data$blrv ~ TRUE,
        TRUE ~ NA,
      )
    )
}
