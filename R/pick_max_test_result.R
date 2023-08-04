#' Pick the test with the highest result for each person
#'
#' @description
#' This function returns a maximum of one test per person from `df`. Test are filtered by `patient_id` and evaluated based on the values in `test_reason` (confirmed results are either preferred or considered exclusively) and `lab_result_number`.
#'
#' The function has two modes:
#'
#' 1. Consider all test results (`cfmd_only = FALSE`): After filtering tests by `patient_id`, the highest confirmed result is kept. If there are no confirmed results for an individual, the highest unconfirmed result is kept.
#' 2. Consider confirmed test results only (`cfmd_only = TRUE`): After filtering tests by `patient_id`, the highest confirmed result is kept. Unconfirmed results are not considered. Therefore, if there is no confirmed result associated with a `patient_id` value, that individual is not represented by a test in the output.
#'
#' @details
#' The `test_reason` variable is created by the function [classify_test_reason()]. See this function's documentation for definitions of `test_reason` values.
#'
#' Confirmed results are from tests with these `test_reason` values:
#'
#' * `ven_cfm_init`
#' * `cap_cfm_elev`
#' * `cap_cfm_nonelev`
#' * `ven_cfm_elev`
#' * `ven_cfm_nonelev`
#' * `ven_flw`
#'
#' Unconfirmed results are from tests with these `test_reason` values:
#'
#' * `cap_scrn`
#' * `unknown`
#'
#' @param df A dataframe.
#' @param cfmd_only Logical: if `TRUE`, consider confirmed results only; if `FALSE` (the default), consider all results.
#' @param silent Logical: silence progress indicator if `TRUE`.
#'
#' @return  A dataframe.
#' @export
#'
#' @importFrom magrittr %>%
#'
# @examples
#'
pick_max_test_result <- function(df, cfmd_only = FALSE, silent = FALSE) {
  var_check(df, var = c(
    "patient_id", "lab_specimen_source",
    "lab_result_symbol", "lab_result_number",
    "test_reason"
  ))

  # Unique `patient_id`s
  pid <- df %>%
    dplyr::distinct(patient_id) %>%
    dplyr::pull()

  if (!silent) pb <- utils::txtProgressBar(1, length(pid), width = 50, style = 3)

  test_list <- list()

  if (!cfmd_only) {
    # Consider all results
    for (i in 1:length(pid)) {
      test_list[[i]] <- max_any_test(df, pid[i])
      if (!silent) utils::setTxtProgressBar(pb, i)
    }
  } else {
    # Consider confirmed results only
    for (i in 1:length(pid)) {
      test_list[[i]] <- max_cfmd_test(df, pid[i])
      if (!silent) utils::setTxtProgressBar(pb, i)
    }
  }

  if (!silent) close(pb)

  as.data.frame(do.call(rbind, test_list))
}

max_any_test <- function(df, id) {
  # `test_reason` values that produce confirmed results
  rsn_cfmd <- c(
    "ven_cfm_init", "cap_cfm_elev", "cap_cfm_nonelev",
    "ven_cfm_elev", "ven_cfm_nonelev", "ven_flw"
  )

  # Keep all tests from `df` where `patient_id` is `id`
  df <- df %>%
    dplyr::filter(patient_id == id)

  if (any(df$test_reason %in% rsn_cfmd)) {
    # If any results are confirmed, drop unconfirmed results, and keep the highest one
    df %>%
      dplyr::filter(test_reason %in% rsn_cfmd) %>%
      dplyr::slice_max(
        lab_result_number,
        n = 1,
        with_ties = FALSE
      )
  } else {
    # If no results are confirmed, keep the highest one
    df %>%
      dplyr::slice_max(
        lab_result_number,
        n = 1,
        with_ties = FALSE
      )
  }
}

max_cfmd_test <- function(df, id) {
  # `test_reason` values that produce confirmed results
  rsn_cfmd <- c(
    "ven_cfm_init", "cap_cfm_elev", "cap_cfm_nonelev",
    "ven_cfm_elev", "ven_cfm_nonelev", "ven_flw"
  )

  # Keep tests with confirmed results from `df` where `patient_id` is `id`
  df %>%
    dplyr::filter(
      patient_id == id,
      test_reason %in% rsn_cfmd
    ) %>%
    dplyr::slice_max(
      lab_result_number,
      n = 1,
      with_ties = FALSE
    )
}
