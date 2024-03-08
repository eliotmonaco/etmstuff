#' Assign BLL classes to lead tests
#'
#' @description
#' This function creates a new variable, `bll_class`, and assigns one of the following values to each lead test based on the test result value:
#'
#' * `> 0 and < 3.5 μg/dL`
#' * `≥ 3.5 and < 5 μg/dL`
#' * `≥ 5 and < 10 μg/dL`
#' * `≥ 10 and < 15 μg/dL`
#' * `≥ 15 and < 25 μg/dL`
#' * `≥ 25 and < 45 μg/dL`
#' * `≥ 45 μg/dL`
#'
#' @param df A dataframe of cleaned, deduplicated lead test records.
#'
#' @return The input dataframe with the new variable `bll_class`.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
# @examples
#'
assign_bll_class <- function(df) {
  if (!is.data.frame(df)) stop("`df` must be a dataframe")
  var_check(df, var = "result_number")

  # Create BLL classes
  df <- df %>%
    dplyr::mutate(bll_class = dplyr::case_when(
      .data$result_number                               <  3.5 ~ 1,
      .data$result_number >=  3.5 & .data$result_number <  5   ~ 2,
      .data$result_number >=  5   & .data$result_number < 10   ~ 3,
      .data$result_number >= 10   & .data$result_number < 15   ~ 4,
      .data$result_number >= 15   & .data$result_number < 25   ~ 5,
      .data$result_number >= 25   & .data$result_number < 45   ~ 6,
      .data$result_number >= 45                                ~ 7
    ))

  # Convert `bll_class` to factor
  df$bll_class <- factor(
    df$bll_class,
    levels = 1:7,
    labels = c(
      "> 0 and < 3.5 \u03bcg/dL",
      "\u2265 3.5 and < 5 \u03bcg/dL",
      "\u2265 5 and < 10 \u03bcg/dL",
      "\u2265 10 and < 15 \u03bcg/dL",
      "\u2265 15 and < 25 \u03bcg/dL",
      "\u2265 25 and < 45 \u03bcg/dL",
      "\u2265 45 \u03bcg/dL"
    ),
    ordered = TRUE
  )

  df
}
