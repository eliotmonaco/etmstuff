#' Track follow-ups for lead test records
#'
#' This function gets the number of days between the lab collection date and the date of the follow-up for tests with elevated results.
#'
#' @param df A dataframe of lead test records.
#' @param silent Logical: silence progress indicator if `TRUE`.
#'
#' @return A dataframe with the new variable `days_to_followup`.
#' @export
#'
#' @importFrom magrittr %>%
#'
# @examples
#'
track_followups <- function(df, silent = FALSE) {
  vars <- c(
    "dupe_id", "patient_id", "lab_collection_date",
    "lab_result_number", "lab_result_elev", "lab_specimen_source"
  )

  var_check(df, var = vars)

  # Prep `df`
  df2 <- df %>%
    dplyr::select(tidyselect::all_of(vars))

  days_list <- list()

  if (!silent) pb <- utils::txtProgressBar(1, nrow(df2), width = 50, style = 3)

  for (i in 1:nrow(df2)) {
    current_test <- df2[i,]

    # Get days to follow-up test
    if (current_test$lab_result_elev & current_test$lab_specimen_source == "Blood - capillary") {
      followup_test <- get_followup_same_day(df2, current_test)
      if (nrow(followup_test) == 0) {
        followup_test <- get_followup(df2, current_test)
      }
      days <- as.numeric(followup_test$lab_collection_date - current_test$lab_collection_date)
    } else if (current_test$lab_result_elev & current_test$lab_specimen_source == "Blood - venous") {
      followup_test <- get_followup(df2, current_test)
      days <- as.numeric(followup_test$lab_collection_date - current_test$lab_collection_date)
    } else {
      days <- c()
    }

    if (purrr::is_empty(days)) days <- NA

    days_list[[i]] <- days

    if (!silent) utils::setTxtProgressBar(pb, i)
  }

  if (!silent) close(pb)

  df$days_to_followup <- unlist(days_list)

  df
}

get_followup_same_day <- function(df, test) {
  # A same-day test can be considered a follow-up only when
  # 1) the current test is capillary, and 2) the same-day test is venous
  df %>%
    dplyr::filter(
      patient_id == test$patient_id,
      lab_collection_date == test$lab_collection_date,
      lab_specimen_source == "Blood - venous",
      dupe_id != test$dupe_id
    ) %>%
    dplyr::slice(1)
}

get_followup <- function(df, test) {
  # A follow-up test must be venous unless
  # 1) the current test is capillary, and 2) the BLL is between 3.5 and 5
  if (test$lab_specimen_source == "Blood - capillary" &
      test$lab_result_number >= 3.5 & test$lab_result_number < 5) {
    df %>%
      dplyr::filter(
        patient_id == test$patient_id,
        lab_collection_date > test$lab_collection_date
        ) %>%
      dplyr::slice_min(lab_collection_date, n = 1, with_ties = FALSE)
  } else {
    df %>%
      dplyr::filter(
        patient_id == test$patient_id,
        lab_collection_date > test$lab_collection_date,
        lab_specimen_source == "Blood - venous"
        ) %>%
      dplyr::slice_min(lab_collection_date, n = 1, with_ties = FALSE)
  }
}
