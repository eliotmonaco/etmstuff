#' Pick the test (confirmatory preferred) with the highest result for a person
#'
#' @description
#' This function selects one test per person from `df`, filtering records by `patient_id` and evaluating them based on the values in `test_reason` and `lab_result_number`. There are two modes:
#'
#' 1. Consider all tests (`cfm_only = FALSE`): After filtering tests by `patient_id`, the confirmatory test with the highest result is kept. If there are no confirmatory tests for an individual, the non-confirmatory test with the highest result is kept.
#' 2. Consider confirmatory tests only (`cfm_only = TRUE`): After filtering tests by `patient_id`, the confirmatory test with the highest result is kept. If there is no confirmatory test for an individual, they are not represented in the output. Non-confirmatory tests are not considered.
#'
#' @details
#' The `test_reason` variable is created by the function [classify_test_reason()].
#'
#' Confirmatory `test_reason` values:
#'
#' * `cap_cfm`
#' * `ven_cfm_i`
#' * `ven_cfm_e`
#' * `ven_cfm_n`
#' * `ven_flw`
#'
#' Non-confirmatory `test_reason` values:
#'
#' * `cap_scrn`
#' * `unknown/other`
#'
#' @param df A dataframe.
#' @param cfm_only Logical: consider confirmatory tests only if `TRUE`; consider all tests (confirmatory, screening, and unknown/other) if `FALSE` (the default).
#' @param silent Logical: silence progress indicator if `TRUE`.
#'
#' @return  A dataframe.
#' @export
#'
#' @importFrom magrittr %>%
#'
# @examples
#'
pick_max_cfm_test <- function(df, cfm_only = FALSE, silent = FALSE) {
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

  if (!cfm_only) {
    # Consider all tests
    for (i in 1:length(pid)) {
      test_list[[i]] <- max_any_test(df, pid[i])
      if (!silent) utils::setTxtProgressBar(pb, i)
    }
  } else {
    # Consider confirmatory tests only
    for (i in 1:length(pid)) {
      test_list[[i]] <- max_cfm_test(df, pid[i])
      if (!silent) utils::setTxtProgressBar(pb, i)
    }
  }

  if (!silent) close(pb)

  as.data.frame(do.call(rbind, test_list))
}

max_any_test <- function(df, id) {
  # Confirmatory values for `test_reason`
  rsn_cfm <- c("cap_cfm", "ven_cfm_i", "ven_cfm_e", "ven_cfm_n", "ven_flw")

  # Keep all tests from `df` where `patient_id` is `id`
  df <- df %>%
    dplyr::filter(patient_id == id)

  if (any(df$test_reason %in% rsn_cfm)) {
    # If any tests are confirmatory, drop non-confirmatory tests, and keep the highest result
    df %>%
      dplyr::filter(test_reason %in% rsn_cfm) %>%
      dplyr::slice_max(
        lab_result_number,
        n = 1,
        with_ties = FALSE
      )
  } else {
    # If no tests are confirmatory, keep the highest result
    df %>%
      dplyr::slice_max(
        lab_result_number,
        n = 1,
        with_ties = FALSE
      )
  }
}

max_cfm_test <- function(df, id) {
  # Confirmatory values for `test_reason`
  rsn_cfm <- c("cap_cfm", "ven_cfm_i", "ven_cfm_e", "ven_cfm_n", "ven_flw")

  # Keep confirmatory tests from `df` where `patient_id` is `id`
  df %>%
    dplyr::filter(
      patient_id == id,
      test_reason %in% rsn_cfm
    ) %>%
    dplyr::slice_max(
      lab_result_number,
      n = 1,
      with_ties = FALSE
    )
}

# Old version, does not use `test_reason`
pick_highest_confirmed_test <- function(df, silent = FALSE) {
  var <- c(
    "patient_id", "lab_collection_date", "lab_specimen_source",
    "lab_result_symbol", "lab_result_number"
  )

  var_check(df, var = var)

  # Unique `patient_id`s
  pid_unq <- df %>%
    dplyr::distinct(patient_id) %>%
    dplyr::pull()

  # DF to hold one test per `patient_id`
  df_tests_unq <- df[0,]

  # Select the highest venous or capillary test from a sequence
  # pick_max_result <- function(df) {
  #   if (any(stringr::str_detect(df$lab_specimen_source, "venous"))) {
  #     # If any test results are venous or venous equivalent, take the highest of those tests
  #     df %>%
  #       dplyr::filter(stringr::str_detect(df$lab_specimen_source, "venous")) %>%
  #       dplyr::filter(lab_result_number == max(lab_result_number)) %>%
  #       dplyr::arrange(dplyr::desc(lab_collection_date)) %>%
  #       dplyr::slice(1)
  #   } else if (all(df$lab_specimen_source == "Blood - capillary")) {
  #     # If all test results are capillary, take the highest test
  #     df %>%
  #       dplyr::filter(lab_result_number == max(lab_result_number)) %>%
  #       dplyr::arrange(dplyr::desc(lab_collection_date)) %>%
  #       dplyr::slice(1)
  #   } else {
  #     message(paste("Unexpected specimen source combination found for `patient_id`", pid_unq[i]))
  #   }
  # }

  if (!silent) pb <- utils::txtProgressBar(1, length(pid_unq), width = 50, style = 3)

  for (i in 1:length(pid_unq)) {
    # All tests for each `patient_id`
    df_tests <- df %>%
      dplyr::filter(patient_id == pid_unq[i]) %>%
      dplyr::arrange(lab_collection_date)

    if (nrow(df_tests) == 1) {
      df_tests_unq <- rbind(df_tests_unq, df_tests)
    } else {
      # DF to hold max result from each test sequence
      df_max <- df[0,]
      # Row counter for `df_max`
      r <- 1

      while (nrow(df_tests) > 0) {
        # Holds rows that belong to a test sequence
        test_seq <- 1

        # Subset a sequence of tests, each within 92 days of the last
        for (j in 1:(nrow(df_tests) - 1)) {
          d1 <- df_tests$lab_collection_date[j]
          d2 <- df_tests$lab_collection_date[j + 1]
          diff <- as.numeric(difftime(d2, d1, units = "days"))
          if (!is.na(diff) & diff <= 92) {
            test_seq <- c(test_seq, j + 1)
          } else {
            break
          }
        }

        # Subset test sequence
        df_seq <- df_tests[test_seq,]

        # Relabel any test following an initial capillary test as "venous equivalent", to be treated as venous in pick_max_result()
        if (nrow(df_seq) > 1 & df_seq$lab_specimen_source[1] == "Blood - capillary") {
          df_seq$lab_specimen_source[2:nrow(df_seq)] <- "venous equivalent"
        }

        # Pick one test from sequence
        df_max[r, ] <- pick_max_result(df_seq)

        # Remove test sequence; if any tests remain, they recycle through the while loop
        df_tests <- df_tests[-test_seq,]

        r <- r + 1
      } # end: while

      # Pick one test from the aggregate of all sequences
      df_max2 <- pick_max_result(df_max)

      # Restores date format after they are changed in pick_max_result()
      # df_max2$lab_collection_date <- as.Date(df_max2$lab_collection_date, origin = "1970-01-01")

      df_tests_unq <- rbind(df_tests_unq, df_max2)
    } # end: else

    if (!silent) utils::setTxtProgressBar(pb, i)
  } # end: for

  if (!silent) close(pb)

  df_tests_unq
}

pick_max_result <- function(df) {
  if (any(stringr::str_detect(df$lab_specimen_source, "venous"))) {
    # If any test results are venous or venous equivalent, take the highest of those tests
    df %>%
      dplyr::filter(stringr::str_detect(df$lab_specimen_source, "venous")) %>%
      dplyr::filter(lab_result_number == max(lab_result_number)) %>%
      dplyr::arrange(dplyr::desc(lab_collection_date)) %>%
      dplyr::slice(1)
  } else if (all(df$lab_specimen_source == "Blood - capillary")) {
    # If all test results are capillary, take the highest test
    df %>%
      dplyr::filter(lab_result_number == max(lab_result_number)) %>%
      dplyr::arrange(dplyr::desc(lab_collection_date)) %>%
      dplyr::slice(1)
  } else {
    m <- paste("Unexpected specimen source combination found for `patient_id`", pid_unq[i])
    message(m)
  }
}
