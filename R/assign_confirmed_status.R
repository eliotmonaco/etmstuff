#' Assign confirmed status to lead tests
#'
#' @description
#' This function determines the confirmed status (`"confirmed"`, `"unconfirmed"`, `"cap_after_venous"`, or `"unknown"`) for each test in df. Tests are subset into sequences for each person in the data set and sorted by 1) collection date, 2) specimen source (capillary tests before venous tests), and 3) result value (in descending order).
#'
#' @details
#' Within a test sequence for a single individual, status is `"confirmed"` if 1) the test is venous, or if 2) the test is capillary and the next prior test within *n* days (as determined by `rule`) is also capillary. Status is `"unconfirmed"` if the test is capillary and there is no prior test within *n* days. Status is `"cap_after_venous"` if the test is capillary and the next prior test within *n* days is venous. If a test meets none of the above criteria, its status is `"unknown"`.
#'
#' @param df A dataframe of cleaned, deduplicated lead tests.
#' @param row_id A unique row identifier variable in `df`.
#' @param rule A string indicating which rule to follow in assigning confirmed status to capillary tests. A capillary test is confirmed if it occurs within *n* days of a prior capillary test.
#' - `"cdc"`: *n* = 84
#' - `"tracking"`: *n* = 90
#' @param silent Logical: silence progress indicator if `TRUE`.
#'
#' @return The input dataframe with new variables.
#' @export
#'
#' @importFrom rlang .data
#'
# @examples
#'
assign_confirmed_status <- function(df, row_id, rule = c("cdc", "tracking"), silent = FALSE) {
  if (!is.data.frame(df)) stop("`df` must be a dataframe")
  if (length(row_id) != 1) stop("`row_id` must have length of 1")
  var <- c(
    "person_id", "collection_date", "specimen_source",
    "result_sign", "result_number"
  )
  var_check(df, var = c(var, row_id))
  if (any(duplicated(df[[row_id]]))) stop("`row_id` is not a unique row identifier")

  # `rule` determines the number of days to check since the previous test
  if (rule == "cdc") {
    x <- 84
    var_cfm <- "confirmed_status_cdc"
  } else if (rule == "tracking") {
    x <- 90
    var_cfm <- "confirmed_status_tracking"
  } else {
    stop("`rule` must be one of `c(\"cdc\", \"tracking\")")
  }

  # Get unique `person_id`s
  person_id_unq <- unique(df$person_id)

  all_sequences <- list()

  if (!silent) pb <- utils::txtProgressBar(1, length(person_id_unq), width = 50, style = 3)

  for (i in 1:length(person_id_unq)) {
    # Get the test sequence for one person
    test_sequence <- df |>
      dplyr::select(
        tidyselect::all_of(c(row_id, var))) |>
      dplyr::filter(.data$person_id == person_id_unq[i]) |>
      # Arrange by 1) date, 2) capillary then venous, 3) higher then lower result
      dplyr::arrange(.data$collection_date, .data$specimen_source, dplyr::desc(.data$result_number))

    n <- nrow(test_sequence)

    # Get days since prior test and prior test source
    if (n == 1) {
      test_sequence$days_since_prev <- NA
      test_sequence$prior_source <- NA
    } else {
      dates <- test_sequence$collection_date
      days <- as.numeric(dates[2:n] - dates[1:(n - 1)])
      test_sequence$days_since_prev <- c(NA, days)
      prior_source <- test_sequence$specimen_source[1:(n - 1)]
      test_sequence$prior_source <- c(NA, prior_source)
    }

    # Determine confirmed status
    test_sequence <- test_sequence |>
      dplyr::mutate({{ var_cfm }} := dplyr::case_when(
        specimen_source == "Blood - venous" ~ "confirmed",                # Condition 1
        specimen_source == "Blood - capillary" & days_since_prev <= x &   # Condition 2
          prior_source == "Blood - capillary" ~ "confirmed",
        specimen_source == "Blood - capillary" & days_since_prev <= x &   # Condition 3
          prior_source == "Blood - venous" ~ "cap_after_ven",
        specimen_source == "Blood - capillary" &                          # Condition 4
          (days_since_prev > x | is.na(days_since_prev)) ~ "unconfirmed",
        TRUE ~ "unknown"                                                  # Condition 5
      ))

    all_sequences[[i]] <- test_sequence

    if (!silent) utils::setTxtProgressBar(pb, i)
  }

  if (!silent) close(pb)

  # Condense all test sequences to a dataframe
  all_sequences <- purrr::list_rbind(all_sequences) |>
    dplyr::select(
      tidyselect::all_of(row_id), "days_since_prev",
      tidyselect::all_of(var_cfm)
    )

  # Compare row IDs in `df` and `all_sequences`
  rows_match <- identical(sort(df[[row_id]]), sort(all_sequences[[row_id]]))
  if (!rows_match) message("Problem matching rows")

  # Join new variables to `df`
  df |>
    dplyr::left_join(all_sequences, by = row_id)
}
