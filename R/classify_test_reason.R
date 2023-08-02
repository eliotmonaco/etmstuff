#' Impute reasons for sequences of blood lead tests
#'
#' @description
#' This function assigns a reason to each test in `df` based on the Kansas Department of Health and Environment's Elevated Blood Lead Investigation Guideline document. A possible sequence of tests for one person could begin with a capillary screening test, followed by a confirmatory venous test, which, if the result is elevated, could be followed by any number of venous follow-up tests until a non-elevated result is obtained. Each test in a sequence should be within a maximum of 90 days of the previous test, therefore it is recommended that `max_interval = 90` be provided in the function call. The reason assigned to each test depends on values in one or more previous tests, therefore it is recommended that prior data which has already been classified by this function be provided in `df2` for the most accurate results. It is also recommended that only values of `Blood - capillary` or `Blood - venous` be present in `lab_specimen_source`, as any other value will lead to a reason of `unknown/other` in subsequent tests.
#'
#' Possible `test_reason` values:
#'
#' * `cap_scrn`: A capillary screening test. This must be the first test in a sequence.
#' * `ven_cfm_init`: An initial venous confirmatory test. This must be the first test in a sequence.
#' * `cap_cfm_elev`: A capillary confirmatory test following an elevated `cap_scrn`.
#' * `cap_cfm_nonelev`: A capillary confirmatory test following a non-elevated `cap_scrn`.
#' * `ven_cfm_elev`: A venous confirmatory test following an elevated `cap_scrn`.
#' * `ven_cfm_nonelev`: A venous confirmatory test following a non-elevated `cap_scrn`.
#' * `ven_flw`: A venous follow-up test. This is any venous test following either an elevated venous test or an elevated capillary confirmatory test, i.e., `cap_cfm_elev` or `cap_cfm_nonelev`.
#' * `unknown`: A test that cannot be assigned another value.
#'
#' @param df The dataframe targeted for classification.
#' @param df2 A previously classified dataframe (optional).
#' @param bl_ref_val The blood lead reference value in mcg/dL (numeric). A blood lead test result >= `bl_rev_val` is elevated.
#' @param max_interval The maximum number of days between tests belonging to the same test sequence (an integer). The recommended value is `90`.
#' @param silent Logical: silence progress indicator if `TRUE`.
#'
#' @return The dataframe is returned with the new columns, `test_reason`, `bl_rev_val`, and `lab_result_elev`.
#' @export
#'
# @examples
#'
classify_test_reason <- function(df, df2 = NULL, bl_ref_val, max_interval, silent = FALSE) {
  vars1 <- c(
    "dupe_id", "patient_id", "lab_collection_date",
    "lab_result_symbol", "lab_result_number", "lab_specimen_source"
  )

  vars2 <- c("lab_result_elev", "bl_ref_val", "test_reason")

  var_check(df, var = vars1)

  df1 <- df %>%
    dplyr::select(tidyselect::all_of(vars1)) %>%
    # Sorting by source ensures that same-day capillary tests precede venous tests
    dplyr::arrange(lab_collection_date, patient_id, lab_specimen_source) %>%
    dplyr::mutate(
      # Add `lab_result_elev`: result is elevated if TRUE, non-elevated if FALSE
      lab_result_elev = dplyr::case_when(
        is.na(lab_result_symbol) & lab_result_number < bl_ref_val ~ FALSE,
        is.na(lab_result_symbol) & lab_result_number >= bl_ref_val ~ TRUE,
        lab_result_symbol == "<" & lab_result_number <= bl_ref_val ~ FALSE,
        lab_result_symbol == "<" & lab_result_number > bl_ref_val ~ NA,
        lab_result_symbol == ">" & lab_result_number < bl_ref_val ~ NA,
        lab_result_symbol == ">" & lab_result_number >= bl_ref_val ~ TRUE
      ),
      # Add column for the current blood lead reference value
      bl_ref_val = bl_ref_val,
      # Add column for the test reason, added below in loop
      test_reason = NA
    )

  if (!is.null(df2)) {
    var_check(df2, var = c(vars1, vars2))
    df2 <- df2 %>%
      dplyr::select(
        tidyselect::all_of(vars1),
        tidyselect::all_of(vars2)
      ) %>%
      dplyr::arrange(lab_collection_date, patient_id, lab_specimen_source)
  }

  # Create `df1` row with all NA values to use in loop
  df_na <- df1[1, ]
  df_na[1, ] <- NA

  if (!silent) pb <- utils::txtProgressBar(1, nrow(df1), width = 50, style = 3)

  for (i in 1:nrow(df1)) {
    current_test <- df1[i, ]

    # Get all tests in the previous `max_interval` for a person (including same-day tests)
    prev_int <- df1 %>%
      dplyr::bind_rows(df2) %>%
      dplyr::filter(
        patient_id == df1$patient_id[i],
        lab_collection_date <= df1$lab_collection_date[i],
        lab_collection_date >= df1$lab_collection_date[i] - max_interval,
        dupe_id != df1$dupe_id[i]
      )

    # Separate most recent test
    prev_test1 <- prev_int %>%
      dplyr::slice_max(lab_collection_date, n = 1, with_ties = TRUE)

    if (nrow(prev_test1) == 0) {
      prevtest <- df_na
    } else if (nrow(prev_test1) == 1) {
      if (prev_test1$lab_collection_date != df1$lab_collection_date[i]) {
        # If previous test is NOT same-day...
        prevtest <- prev_test1
      } else {
        # If previous test IS same-day...
        if (prev_test1$lab_specimen_source == "Blood - capillary" &
          df1$lab_specimen_source[i] == "Blood - venous") {
          prevtest <- prev_test1
        } else if (prev_test1$lab_specimen_source == "Blood - venous" &
          df1$lab_specimen_source[i] == "Blood - capillary") {
          # Get second most recent test
          prev_test2 <- prev_int %>%
            dplyr::anti_join(prev_test1, by = "dupe_id") %>%
            dplyr::slice_max(lab_collection_date, n = 1, with_ties = TRUE)
          if (nrow(prev_test2) == 0) {
            prevtest <- df_na
          } else if (nrow(prev_test2) == 1) {
            prevtest <- prev_test2
          } else {
            if ("Blood - venous" %in% prev_test2$lab_specimen_source) {
              prevtest <- prev_test2 %>%
                dplyr::filter(lab_specimen_source == "Blood - venous") %>%
                dplyr::slice_max(lab_result_number, n = 1, with_ties = FALSE)
            } else {
              prevtest <- prev_test2 %>%
                dplyr::slice_max(lab_result_number, n = 1, with_ties = FALSE)
            }
          }
        } else {
          df1$test_reason[i] <- "unknown"
          next
        }
      }
    } else {
      if (all(prev_test1$lab_specimen_source == c("Blood - capillary", "Blood - venous"))) {
        prevtest <- prev_test1 %>%
          dplyr::filter(lab_specimen_source == "Blood - venous")
      } else {
        df1$test_reason[i] <- "unknown"
        next
      }
    }

    # Assign test reason
    df1$test_reason[i] <- rsn <- dplyr::case_when(
      # cap_scrn: src = cap, prev rsn = NA
      df1$lab_specimen_source[i] == "Blood - capillary" &
        is.na(prevtest$test_reason) ~
        "cap_scrn",
      # ven_cfm_init: src = ven, prev rsn = NA
      df1$lab_specimen_source[i] == "Blood - venous" &
        is.na(prevtest$test_reason) ~
        "ven_cfm_init",
      # cap_cfm_elev: src = cap, prev rsn = cap_scrn, prev BLL = elevated
      df1$lab_specimen_source[i] == "Blood - capillary" &
        prevtest$test_reason == "cap_scrn" &
        prevtest$lab_result_elev ~
        "cap_cfm_elev",
      # cap_cfm_nonelev: src = cap, prev rsn = cap_scrn, prev BLL = non-elevated
      df1$lab_specimen_source[i] == "Blood - capillary" &
        prevtest$test_reason == "cap_scrn" &
        !prevtest$lab_result_elev ~
        "cap_cfm_nonelev",
      # ven_cfm_elev: src = ven, prev rsn = cap_scrn, prev BLL = elevated
      df1$lab_specimen_source[i] == "Blood - venous" &
        prevtest$test_reason == "cap_scrn" &
        prevtest$lab_result_elev ~
        "ven_cfm_elev",
      # ven_cfm_nonelev: src = ven, prev rsn = cap_scrn, prev BLL = non-elevated
      df1$lab_specimen_source[i] == "Blood - venous" &
        prevtest$test_reason == "cap_scrn" &
        !prevtest$lab_result_elev ~
        "ven_cfm_nonelev",
      # ven_flw: src = ven, prev src = ven OR previous rsn = cap_cfm_elev|cap_cfm_nonelev, prev BLL = elevated
      df1$lab_specimen_source[i] == "Blood - venous" &
        (prevtest$lab_specimen_source == "Blood - venous" |
          (prevtest$test_reason == "cap_cfm_elev" | prevtest$test_reason == "cap_cfm_nonelev")) &
        prevtest$lab_result_elev ~
        "ven_flw",
      # All others
      TRUE ~ "unknown"
    )

    if (!silent) utils::setTxtProgressBar(pb, i)
  }

  if (!silent) close(pb)

  df %>%
    dplyr::left_join(
      df1 %>%
        dplyr::select(dupe_id, tidyselect::all_of(vars2)),
      by = "dupe_id"
    )
}
