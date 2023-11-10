#' Find similar tests on different days
#'
#' This function finds tests that fit the criteria for duplicates (the same person, test result, and specimen source) but occur on different days. The `days` argument determines the number of days to look ahead for similar tests. `df` should be a deduplicated data set of lead records.
#'
#' @param df A dataframe of deduplicated lead records.
#' @param days A number of days to look ahead for similar tests.
#' @param silent Logical: silence progress indicator if `TRUE`.
#'
#' @return A list in which each element is a dataframe consisting of a set of close tests.
#' @export
#'
#' @importFrom magrittr %>%
#'
# @examples
#'
find_close_tests <- function(df, days, silent = FALSE) {
  vars <- c(
    "dupe_id", "patient_id", "patient_record_number", "lab_collection_date",
    "lab_result_symbol", "lab_result_number", "lab_specimen_source"
  )

  var_check(df, var = vars)

  if (days != round(days)) stop("`days` must be an integer", call. = FALSE)

  df <- df %>%
    dplyr::select(tidyselect::all_of(vars))

  test_list <- list()

  ct <- 1

  if (!silent) pb <- utils::txtProgressBar(1, nrow(df), width = 50, style = 3)

  for (i in 1:nrow(df)) {
    tests <- df %>%
      dplyr::filter(
        patient_id == df$patient_id[i],
        lab_collection_date >= df$lab_collection_date[i],
        lab_collection_date <= df$lab_collection_date[i] + days,
        (lab_result_symbol == df$lab_result_symbol[i] |
           (is.na(lab_result_symbol) & is.na(df$lab_result_symbol[i]))),
        lab_result_number == df$lab_result_number[i],
        lab_specimen_source == df$lab_specimen_source[i],
        dupe_id != df$dupe_id[i]
      )

    if (nrow(tests) > 0) {
      test_list[[ct]] <- df[i,] %>%
        dplyr::bind_rows(tests)
      ct = ct + 1
    }

    if (!silent) utils::setTxtProgressBar(pb, i)
  }

  if (!silent) close(pb)

  test_list
}
