#' Flag tests for overdue follow-up
#'
#' This function assigns each lead test to a BLL class and determines whether the follow-up for the test is overdue according to KDHE guidelines. The function is used in the lead dashboard.
#'
#' @param df A dataframe of lead test records.
#'
#' @return A dataframe with the new variables `bll_class` and `followup_overdue`.
#' @export
#'
#' @importFrom magrittr %>%
#'
# @examples
#'
followup_overdue_flag <- function(df) {
  var_check(df, var = "days_to_followup")

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

  # Label and order BLL factors
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

  # Flag as overdue according to KDHE guidelines
  df %>%
    dplyr::mutate(followup_overdue = dplyr::case_when(
      (bll_class == "3.5 to < 5" | bll_class == "5 to < 10") &
        days_to_followup > 92 ~ TRUE,
      (bll_class == "10 to < 15" | bll_class == "15 to < 25" | bll_class == "25 to < 45") &
        lab_specimen_source == "Blood - capillary" &
        days_to_followup > 31 ~ TRUE,
      (bll_class == "10 to < 15" | bll_class == "15 to < 25") &
        lab_specimen_source == "Blood - venous" &
        days_to_followup > 92 ~ TRUE,
      bll_class == "> 45" &
        lab_specimen_source == "Blood - capillary" &
        days_to_followup > 2 ~ TRUE,
      # bll_class == "> 45" &
      #   lab_specimen_source == "Blood - venous" &
      #   days_to_followup > 7 ~ TRUE,
      TRUE ~ FALSE
    ))
}
