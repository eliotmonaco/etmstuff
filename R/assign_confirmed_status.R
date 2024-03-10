#' Assign confirmed status to lead tests
#'
#' @description
#' This function subsets `df` into test sequences for each person and returns the following variables:
#'
#' * the confirmed status (either `confirmed_status_cdc` or `confirmed_status_tracking`)
#' * `test_gap_prev` (the number of days since the previous test in the sequence)
#' * `test_gap_next` (the number of days before the next test in the sequence)
#' * `test_reason`
#'
#' @details
#' Within the function, tests are subset into sequences for each person in `df` and sorted by 1) collection date, 2) specimen source (capillary tests before venous tests), and 3) result value (in descending order).
#'
#' Confirmed status values:
#'
#' - `confirmed`: any venous test, or a capillary test following a prior capillary test within *n* days (*n* = 84 for the CDC; *n* = 90 for Tracking)
#' - `unconfirmed`: a capillary test that has no prior test within *n* days
#' - `cap_after_ven`: a capillary test following a venous test within *n* days
#' - `unknown_early`: a capillary test that has no prior test and occurs within *n* days of the start of the data set’s date range (this means a prior test may exist before the earliest date of the data set)
#' - `unknown`: any test not assigned an above status
#'
#' `test_reason` values:
#'
#' - `screening`: an unconfirmed capillary test with no prior test within one year
#' - `confirmatory`: the next test after an elevated screening test (venous or capillary)
#'
#' @param df A dataframe of cleaned, deduplicated lead test records.
#' @param row_id A unique row identifier variable in `df`.
#' @param rule A string indicating which rule to follow in assigning confirmed status to capillary tests. A capillary test is confirmed if it occurs within *n* days of a prior capillary test.
#' - `cdc`: *n* = 84
#' - `tracking`: *n* = 90
#' @param start_date A string formatted "YYYY-MM-DD" representing the start of the date range covered by the data set.
#' @param silent Logical: silence progress indicator if `TRUE`.
#'
#' @return The input dataframe with the new variables `test_gap_prev` (in days since the previous test), either `confirmed_status_cdc` or `confirmed_status_tracking`, and `test_reason`.
#' @export
#'
#' @importFrom rlang .data
#'
# @examples
#'
assign_confirmed_status <- function(df, row_id, rule = c("cdc", "tracking"), start_date, silent = FALSE) {
  if (!is.data.frame(df)) stop("`df` must be a dataframe")
  if (length(row_id) != 1) stop("`row_id` must have length of 1")
  var <- c(
    "person_id", "collection_date", "specimen_source",
    "result_number", "elevated_status"
  )
  var_check(df, var = c(var, row_id))
  if (any(duplicated(df[[row_id]]))) stop("`row_id` is not a unique row identifier")

  # `rule` determines the number of days in the retesting window for a confirmed capillary test
  if (rule == "cdc") {
    cfm_retest_wndw <- 84
    var_cfm <- "confirmed_status_cdc"
  } else if (rule == "tracking") {
    cfm_retest_wndw <- 90
    var_cfm <- "confirmed_status_tracking"
  } else {
    stop("`rule` must be one of `c(\"cdc\", \"tracking\")")
  }

  # Prior to these dates, the confirmed status and test reason are unknown because a prior test might exist before the start of the data's date range
  period_cfm_unk <- as.Date(start_date, format = "%Y-%m-%d") + cfm_retest_wndw
  period_scrn_unk <- as.Date(start_date, format = "%Y-%m-%d") + 365

  # Get unique `person_id`s
  person_id_unq <- unique(df$person_id)

  all_sequences <- list()

  if (!silent) pb <- utils::txtProgressBar(1, length(person_id_unq), width = 50, style = 3)

  for (i in 1:length(person_id_unq)) {
    # Get the test sequence for one person
    test_seq <- df |>
      dplyr::select(
        tidyselect::all_of(c(row_id, var))) |>
      dplyr::filter(.data$person_id == person_id_unq[i]) |>
      # Arrange by 1) date, 2) capillary then venous, 3) higher then lower result
      dplyr::arrange(.data$collection_date, .data$specimen_source, dplyr::desc(.data$result_number))

    n <- nrow(test_seq)

    # Get test gaps, prior test source, & prior test elevated status
    if (n == 1) {
      test_seq$test_gap_next <- NA
      test_seq$test_gap_prev <- NA
      test_seq$pr_src <- NA
      test_seq$pr_elev <- NA
    } else {
      dates <- test_seq$collection_date
      days_btw <- as.numeric(dates[2:n] - dates[1:(n - 1)])
      test_seq$test_gap_prev <- c(NA, days_btw)
      test_seq$test_gap_next <- c(days_btw, NA)
      test_seq$pr_src <- c(NA, test_seq$specimen_source[1:(n - 1)])
      test_seq$pr_elev <- c(NA, test_seq$elevated_status[1:(n - 1)])
    }

    # Determine confirmed status & test reason
    test_seq <- test_seq |>
      dplyr::mutate(
        {{ var_cfm }} := dplyr::case_when(
          specimen_source == "Blood - venous" ~
            "confirmed",
          specimen_source == "Blood - capillary" & is.na(test_gap_prev) &
            collection_date < period_cfm_unk ~
            "unknown_early",
          specimen_source == "Blood - capillary" & test_gap_prev <= cfm_retest_wndw &
            pr_src == "Blood - capillary" ~
            "confirmed",
          specimen_source == "Blood - capillary" & test_gap_prev <= cfm_retest_wndw &
            pr_src == "Blood - venous" ~
            "cap_after_ven",
          specimen_source == "Blood - capillary" &
            (test_gap_prev > cfm_retest_wndw | is.na(test_gap_prev)) ~
            "unconfirmed",
          TRUE ~
            "unknown"
        ),
        test_reason = dplyr::case_when(
          # A screening test is a capillary test with no prior test within one year
          specimen_source == "Blood - capillary" & collection_date >= period_scrn_unk &
            (test_gap_prev > 365 | is.na(test_gap_prev)) ~
            "screening",
          TRUE ~ NA
        )
      )

    if (n > 1) {
      # Get prior test reason
      test_seq$pr_rsn <- c(NA, test_seq$test_reason[1:(n - 1)])
      # Determine test reason
      test_seq <- test_seq |>
        dplyr::mutate(test_reason = dplyr::case_when(
          test_reason == "screening" ~ "screening",
          # A confirmatory test is the next test after an elevated screening test
          pr_rsn == "screening" & pr_elev ~ "confirmatory",
          TRUE ~ NA
        ))
    }

    # browser(expr = {n > 4})
    # browser(expr = {"unconfirmed" %in% test_seq[[var_cfm]]})
    # browser(expr = {"confirmatory" %in% test_seq$test_reason})
    # browser(expr = {"screening" %in% test_seq$test_reason})

    all_sequences[[i]] <- test_seq

    if (!silent) utils::setTxtProgressBar(pb, i)
  }

  if (!silent) close(pb)

  # Condense all test sequences to a dataframe
  all_sequences <- purrr::list_rbind(all_sequences) |>
    dplyr::select(
      tidyselect::all_of(row_id),
      "test_gap_prev", "test_gap_next",
      tidyselect::all_of(var_cfm),
      "test_reason"
    )

  # Compare row IDs in `df` and `all_sequences`
  rows_match <- identical(sort(df[[row_id]]), sort(all_sequences[[row_id]]))
  if (!rows_match) message("Problem matching rows")

  # Join new variables to `df`
  df |>
    dplyr::left_join(all_sequences, by = row_id)
}
