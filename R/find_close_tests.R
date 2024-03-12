#' Find similar tests on different days
#'
#' This function finds tests that fit the criteria for duplicates (the same person, test result, and specimen source) but occur on different days.
#'
#' @param df A dataframe of cleaned, deduplicated lead test records.
#' @param days The number of days to look ahead for similar tests.
#' @param silent Logical: silence progress indicator if `TRUE`.
#'
#' @return A dataframe.
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
    "dupe_id", "person_id", "collection_date",
    "result_sign", "result_number", "specimen_source"
  )
  var_check(df, var = vars)

  close_tests <- list()

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
      close_tests[[ct]] <- df[i,] %>%
        dplyr::bind_rows(tests)
      ct = ct + 1
    }

    if (!silent) utils::setTxtProgressBar(pb, i)
  }

  if (!silent) close(pb)

  purrr::list_rbind(close_tests)
}
