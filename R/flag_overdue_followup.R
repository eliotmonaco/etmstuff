#' Flag lead tests for overdue follow-up
#'
#' This function determines whether the follow-up for each test is overdue according to the KDHE Elevated Blood Lead Disease Investigation Guidelines. The function is used in the lead dashboard.
#'
#' @param df A dataframe of lead test records.
#'
#' @return A dataframe with the new variables `followup_interval`, `sufficient_interval`, and `followup_overdue`.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
# @examples
#'
flag_overdue_followup <- function(df) {
  var_check(df, var = c("lab_collection_date", "lab_specimen_source", "bll_class", "days_to_followup"))

  last_date <- max(df$lab_collection_date)

  # Assign the follow-up interval for each BLL class + specimen source combination
  df <- df %>%
    dplyr::mutate(followup_interval = dplyr::case_when(
      .data$bll_class == "0 to < 3.5" & .data$lab_specimen_source == "Blood - capillary" ~ NA,
      .data$bll_class == "0 to < 3.5" & .data$lab_specimen_source == "Blood - venous"    ~ NA,
      .data$bll_class == "3.5 to < 5" & .data$lab_specimen_source == "Blood - capillary" ~ 92,
      .data$bll_class == "3.5 to < 5" & .data$lab_specimen_source == "Blood - venous"    ~ 92,
      .data$bll_class == "5 to < 10"  & .data$lab_specimen_source == "Blood - capillary" ~ 92,
      .data$bll_class == "5 to < 10"  & .data$lab_specimen_source == "Blood - venous"    ~ 92,
      .data$bll_class == "10 to < 15" & .data$lab_specimen_source == "Blood - capillary" ~ 31,
      .data$bll_class == "10 to < 15" & .data$lab_specimen_source == "Blood - venous"    ~ 92,
      .data$bll_class == "15 to < 25" & .data$lab_specimen_source == "Blood - capillary" ~ 31,
      .data$bll_class == "15 to < 25" & .data$lab_specimen_source == "Blood - venous"    ~ 92,
      .data$bll_class == "25 to < 45" & .data$lab_specimen_source == "Blood - capillary" ~ 31,
      .data$bll_class == "25 to < 45" & .data$lab_specimen_source == "Blood - venous"    ~ 28,
      .data$bll_class == "> 45"       & .data$lab_specimen_source == "Blood - capillary" ~  3,
      .data$bll_class == "> 45"       & .data$lab_specimen_source == "Blood - venous"    ~ 28
    ))

  # Determine if the range of the data allows for a sufficient interval in which a follow-up could have occurred
  df <- df %>%
    dplyr::mutate(sufficient_interval = as.numeric(last_date - .data$lab_collection_date) >= .data$followup_interval)

  # Flag test as overdue if
  #   1) there was a sufficient interval, and
  #   2 a) the days to follow-up exceeded the follow-up interval, or
  #   2 b) the days to follow-up is NA
  df %>%
    dplyr::mutate(followup_overdue = dplyr::case_when(
      .data$sufficient_interval & .data$days_to_followup > .data$followup_interval ~ TRUE,
      .data$sufficient_interval & is.na(.data$days_to_followup) ~ TRUE,
      TRUE ~ FALSE
    ))
}
