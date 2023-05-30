#' Pick the highest confirmed test for each person in the data set
#'
#' This function is used when counting individuals based on the blood lead level (BLL) from their test results. It picks the test with the highest BLL, preferring a confirmed over an unconfirmed test, for each person within the time period defined by the data in `df`. Tests are grouped by `patient_id`. Tests for the same person that occur within 92 days of another belong to one sequence. The highest confirmed test is selected within each person sequence. If a person has more than one sequence, the highest confirmed test among the sequences is then selected.
#'
#' @param df A dataframe.
#' @param silent Logical: silence output to console if `TRUE`.
#'
#' @return  A dataframe.
#' @export
#'
#' @importFrom magrittr %>%
#'
# @examples
#'
pick_highest_confirmed_test <- function(df, silent = FALSE) {
  vars <- c(
    "recno", "patient_id",
    "lab_collection_date", "lab_specimen_source",
    "lab_result_symbol", "lab_result_number")

  var_check(df, var = vars)

  # Unique `patient_id`s
  pid_unq <- df %>%
    dplyr::distinct(patient_id) %>%
    dplyr::pull()

  # DF to hold one test per `patient_id`
  df_tests_unq <- df[0, ]

  # Select the highest venous or capillary test from a sequence
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
      message(paste("Unexpected specimen source combination found for `patient_id`", pid_unq[i]))
    }
  }

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
      df_max <- df[0, ]
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
        df_seq <- df_tests[test_seq, ]

        # Relabel any test following an initial capillary test as "venous equivalent", to be treated as venous in pick_max_result()
        if (nrow(df_seq) > 1 & df_seq$lab_specimen_source[1] == "Blood - capillary") {
          df_seq$lab_specimen_source[2:nrow(df_seq)] <- "venous equivalent"
        }

        # Pick one test from sequence
        df_max[r, ] <- pick_max_result(df_seq)

        # Remove test sequence; if any tests remain, they recycle through the while loop
        df_tests <- df_tests[-test_seq, ]

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
