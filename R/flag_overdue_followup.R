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
#'
# @examples
#'
flag_overdue_followup <- function(df) {
  var_check(df, var = c("lab_collection_date", "lab_specimen_source", "bll_class", "days_to_followup"))

  last_date <- max(df$lab_collection_date)

  # Assign the follow-up interval for each BLL class + specimen source combination
  df <- df %>%
    dplyr::mutate(followup_interval = dplyr::case_when(
      bll_class == "0 to < 3.5" & lab_specimen_source == "Blood - capillary" ~ NA,
      bll_class == "0 to < 3.5" & lab_specimen_source == "Blood - venous"    ~ NA,
      bll_class == "3.5 to < 5" & lab_specimen_source == "Blood - capillary" ~ 92,
      bll_class == "3.5 to < 5" & lab_specimen_source == "Blood - venous"    ~ 92,
      bll_class == "5 to < 10"  & lab_specimen_source == "Blood - capillary" ~ 92,
      bll_class == "5 to < 10"  & lab_specimen_source == "Blood - venous"    ~ 92,
      bll_class == "10 to < 15" & lab_specimen_source == "Blood - capillary" ~ 31,
      bll_class == "10 to < 15" & lab_specimen_source == "Blood - venous"    ~ 92,
      bll_class == "15 to < 25" & lab_specimen_source == "Blood - capillary" ~ 31,
      bll_class == "15 to < 25" & lab_specimen_source == "Blood - venous"    ~ 92,
      bll_class == "25 to < 45" & lab_specimen_source == "Blood - capillary" ~ 31,
      bll_class == "25 to < 45" & lab_specimen_source == "Blood - venous"    ~ 28,
      bll_class == "> 45"       & lab_specimen_source == "Blood - capillary" ~  3,
      bll_class == "> 45"       & lab_specimen_source == "Blood - venous"    ~ 28
    ))

  # Determine if the range of the data allows for a sufficient interval in which a follow-up could have occurred
  df <- df %>%
    dplyr::mutate(sufficient_interval = as.numeric(last_date - lab_collection_date) >= followup_interval)

  # Flag test as overdue if
  #   1) there was a sufficient interval, and
  #   2 a) the days to follow-up exceeded the follow-up interval, or
  #   2 b) the days to follow-up is NA
  df %>%
    dplyr::mutate(followup_overdue = dplyr::case_when(
      sufficient_interval & days_to_followup > followup_interval ~ TRUE,
      sufficient_interval & is.na(days_to_followup) ~ TRUE,
      TRUE ~ FALSE
    ))
}
