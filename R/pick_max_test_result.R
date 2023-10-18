#' Select the test with the highest result for each person
#'
#' @description
#' This function returns a maximum of one test per person from `df`. Confirmed and unconfirmed tests are handled differently based on the mode selected in the `eval` argument. Tests are filtered first by the `patient_id` variable. Test results are evaluated by the value in `lab_result_number` only; `lab_result_symbol` is not considered.
#'
#' @details
#' The `test_reason` variable is created by the function [assign_test_reason()]. See that function's documentation for definitions of `test_reason` values.
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
#' @param eval
#' * `all_tests`: pick the highest result among all tests for each person.
#' * `cfmd_preferred`: if both confirmed and unconfirmed tests are present for a person, pick the highest confirmed test result; otherwise pick the highest unconfirmed test result.
#' * `cfmd_only`: if both confirmed and unconfirmed tests are present for a person, pick the highest confirmed test result and ignore any unconfirmed test results.
#' @param silent Logical: silence progress indicator if `TRUE`.
#'
#' @return  A dataframe.
#' @export
#'
#' @importFrom magrittr %>%
#'
# @examples
#'
pick_max_test_result <- function(df, eval = c("all_tests", "cfmd_preferred", "cfmd_only"), silent = FALSE) {
  var_check(df, var = c("patient_id", "lab_result_number"))

  # Unique `patient_id`s
  pid <- df %>%
    dplyr::distinct(patient_id) %>%
    dplyr::pull()

  if (!silent) pb <- utils::txtProgressBar(1, length(pid), width = 50, style = 3)

  test_list <- list()

  if (eval == "all_tests") {
    for (i in 1:length(pid)) {
      test_list[[i]] <- max_result_all(df, pid[i])
      if (!silent) utils::setTxtProgressBar(pb, i)
    }
  } else if (eval == "cfmd_preferred") {
    for (i in 1:length(pid)) {
      test_list[[i]] <- max_result_cfmd_pref(df, pid[i])
      if (!silent) utils::setTxtProgressBar(pb, i)
    }
  } else if (eval == "cfmd_only") {
    for (i in 1:length(pid)) {
      test_list[[i]] <- max_result_cfmd_only(df, pid[i])
      if (!silent) utils::setTxtProgressBar(pb, i)
    }
  } else {
    stop('`eval` must be one of `c("all_tests", "cfmd_preferred", "cfmd_only")`', call. = FALSE)
  }

  if (!silent) close(pb)

  # Return dataframe as tibble
  purrr::list_rbind(test_list)
}

max_result_all <- function(df, id) {
  # Keep all tests from `df` where `patient_id` is `id`
  df %>%
    dplyr::filter(patient_id == id) %>%
    dplyr::slice_max(
      lab_result_number,
      n = 1,
      with_ties = FALSE
    )
}

max_result_cfmd_pref <- function(df, id) {
  var_check(df, var = "test_reason")

  # `test_reason` values that produce confirmed results
  rsn_cfmd <- c(
    "ven_cfm_init", "cap_cfm_elev", "cap_cfm_nonelev",
    "ven_cfm_elev", "ven_cfm_nonelev", "ven_flw"
  )

  # Keep all tests from `df` where `patient_id` is `id`
  df <- df %>%
    dplyr::filter(patient_id == id)

  if (any(df$test_reason %in% rsn_cfmd)) {
    # If any results are confirmed, drop unconfirmed results, and keep the highest confirmed result
    df %>%
      dplyr::filter(test_reason %in% rsn_cfmd) %>%
      dplyr::slice_max(
        lab_result_number,
        n = 1,
        with_ties = FALSE
      )
  } else {
    # If no results are confirmed, keep the highest unconfirmed result
    df %>%
      dplyr::slice_max(
        lab_result_number,
        n = 1,
        with_ties = FALSE
      )
  }
}

max_result_cfmd_only <- function(df, id) {
  var_check(df, var = "test_reason")

  # `test_reason` values that produce confirmed results
  rsn_cfmd <- c(
    "ven_cfm_init", "cap_cfm_elev", "cap_cfm_nonelev",
    "ven_cfm_elev", "ven_cfm_nonelev", "ven_flw"
  )

  # Keep only tests with confirmed results from `df` where `patient_id` is `id`
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
