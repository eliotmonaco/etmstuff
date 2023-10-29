#' Assign BLL classes to lead tests
#'
#' This function assigns each lead test to a BLL class based on the KDHE Elevated Blood Lead Disease Investigation Guidelines.
#'
#' @param df A dataframe of lead test records.
#'
#' @return A dataframe with the new variable `bll_class`.
#' @export
#'
#' @importFrom magrittr %>%
#'
# @examples
#'
assign_bll_class <- function(df) {
  var_check(df, var = "lab_result_number")

  # Create BLL classes
  df <- df %>%
    dplyr::mutate(bll_class = dplyr::case_when(
      lab_result_number                             <  3.5 ~ 1,
      lab_result_number >=  3.5 & lab_result_number <  5   ~ 2,
      lab_result_number >=  5   & lab_result_number < 10   ~ 3,
      lab_result_number >= 10   & lab_result_number < 15   ~ 4,
      lab_result_number >= 15   & lab_result_number < 25   ~ 5,
      lab_result_number >= 25   & lab_result_number < 45   ~ 6,
      lab_result_number >= 45                              ~ 7
    ))

  # Convert `bll_class` to factor
  df$bll_class <- factor(
    df$bll_class,
    levels = 1:7,
    labels = c(
      "0 to < 3.5",
      "3.5 to < 5",
      "5 to < 10",
      "10 to < 15",
      "15 to < 25",
      "25 to < 45",
      "> 45"
    ),
    ordered = TRUE
  )

  df
}
