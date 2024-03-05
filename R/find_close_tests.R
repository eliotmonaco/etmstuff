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
#' @importFrom rlang .data
#'
# @examples
#'
find_close_tests <- function(df, days, silent = FALSE) {
  if (!is.data.frame(df)) stop("`df` must be a dataframe")
  if (length(days) != 1 | days != round(days)) stop("`days` must be an integer")

  vars <- c(
    "dupe_id", "person_id", "patient_record_number", "collection_date",
    "result_sign", "result_number", "specimen_source"
  )

  var_check(df, var = vars)


  df <- df %>%
    dplyr::select(tidyselect::all_of(vars))

  test_list <- list()

  ct <- 1

  if (!silent) pb <- utils::txtProgressBar(1, nrow(df), width = 50, style = 3)

  for (i in 1:nrow(df)) {
    tests <- df %>%
      dplyr::filter(
        .data$person_id == df$person_id[i],
        .data$collection_date >= df$collection_date[i],
        .data$collection_date <= df$collection_date[i] + days,
        (.data$result_sign == df$result_sign[i] |
           (is.na(.data$result_sign) & is.na(df$result_sign[i]))),
        .data$result_number == df$result_number[i],
        .data$specimen_source == df$specimen_source[i],
        .data$dupe_id != df$dupe_id[i]
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
