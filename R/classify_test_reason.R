#' Impute reasons for series of blood lead tests
#'
#' @description
#' This function assigns a reason to each test in `df` based on the Kansas Department of Health and Environment's Elevated Blood Lead Investigation Guideline document. A possible sequence of tests for one person could begin with a capillary screening test, followed by a confirmatory venous test, which, if the result is elevated, could be followed by any number of venous follow-up tests until a non-elevated result is obtained. Each test in a sequence should be within a maximum of 90 days of the previous test, therefore it is recommended that `max_interval = 90` be provided in the function call. The reason assigned to each test depends on values in one or more previous tests, therefore it is recommended that prior data which has already been classified by this function be provided in `df2` for the most accurate results. It is also recommended that only values of `Blood - capillary` or `Blood - venous` be present in `lab_specimen_source`, as any other value will lead to a reason of `unknown/other` in subsequent tests.
#'
#' Possible `test_reason` values are
#'
#' * `cap_scrn`: a capillary screening test (must be the first test in a sequence).
#' * `ven_cfm_i`: an initial confirmatory venous test (must be the first test in a sequence).
#' * `cap_cfm`: a confirmatory capillary test (follows a `cap_scrn`).
#' * `ven_cfm_e`: a confirmatory venous test following an elevated `cap_scrn`.
#' * `ven_cfm_n`: a confirmatory venous test following a non-elevated `cap_scrn`.
#' * `ven_flw`: a venous follow-up test (follows any venous test or an elevated `cap_cfm`).
#' * `unknown/other`: a test that cannot be assigned another value.
#'
#' @param df The dataframe targeted for classification.
#' @param df2 A previously classified dataframe (optional).
#' @param bl_ref_val The blood lead reference value in mcg/dL (numeric). A blood lead test result >= `bl_rev_val` is elevated.
#' @param max_interval The maximum number of days between tests belonging to the same test sequence (an integer). The recommended value is `90`.
#'
#' @return The dataframe is returned with a new column, `test_reason`.
#' @export
#'
# @examples
#'
classify_test_reason <- function(df, df2 = NULL, bl_ref_val, max_interval) {
  vars <- c(
    "row_id_src", "patient_id", "lab_collection_date",
    "lab_result_symbol", "lab_result_number",
    "lab_specimen_source", "test_reason")

  var_check(df, var = vars[1:6])

  df <- df %>%
    # Sort tests by date, person, and same-day capillary tests before venous
    dplyr::arrange(lab_collection_date, patient_id, lab_specimen_source) %>%
    dplyr::mutate(
      # Add `lab_result_elev`: lab result is elevated if TRUE, non-elevated if FALSE
      lab_result_elev = dplyr::case_when(
        is.na(lab_result_symbol) & lab_result_number < bl_ref_val ~ FALSE,
        is.na(lab_result_symbol) & lab_result_number >= bl_ref_val ~ TRUE,
        lab_result_symbol == "<" & lab_result_number <= bl_ref_val ~ FALSE,
        lab_result_symbol == "<" & lab_result_number > bl_ref_val ~ NA,
        lab_result_symbol == ">" & lab_result_number < bl_ref_val ~ NA,
        lab_result_symbol == ">" & lab_result_number >= bl_ref_val ~ TRUE
      ),
      # Add column for the blood lead reference value provided
      bl_ref_val = bl_ref_val,
      # Add column for the test reason
      test_reason = NA
    )

  if (!is.null(df2)) {
    var_check(df2, var = colnames(df))
    df2 <- df2 %>%
      # Match column order of `df`
      dplyr::select(tidyselect::all_of(colnames(df))) %>%
      dplyr::arrange(lab_collection_date, patient_id, lab_specimen_source)
  }

  # Create `df` row with NA values for loop
  df_na <- df[1,]
  df_na[1,] <- NA

  pb <- utils::txtProgressBar(1, nrow(df), width = 50, style = 3)

  for (i in 1:nrow(df)) {
    # Get all tests in the previous `max_interval` for a person (including same-day tests)
    prev_int <- df %>%
      dplyr::bind_rows(df2) %>%
      dplyr::filter(
        patient_id == df$patient_id[i],
        lab_collection_date <= df$lab_collection_date[i],
        lab_collection_date >= df$lab_collection_date[i] - max_interval,
        row_id_src != df$row_id_src[i]
      )

    # Separate most recent test
    prev_test1 <- prev_int %>%
      dplyr::slice_max(lab_collection_date, n = 1, with_ties = TRUE)

    if (nrow(prev_test1) == 0) {
      prevtest <- df_na
    } else if (nrow(prev_test1) == 1) {
      if (prev_test1$lab_collection_date != df$lab_collection_date[i]) {
        # If previous test is NOT same-day...
        prevtest <- prev_test1
      } else {
        # If previous test IS same-day...
        if (prev_test1$lab_specimen_source == "Blood - capillary" &
            df$lab_specimen_source[i] == "Blood - venous") {
          prevtest <- prev_test1
        } else if (prev_test1$lab_specimen_source == "Blood - venous" &
                   df$lab_specimen_source[i] == "Blood - capillary") {
          # Get second most recent test
          browser()
          prev_test2 <- prev_int %>%
            dplyr::anti_join(prev_test1, by = "row_id_src") %>%
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
          # browser()
          df$test_reason[i] <- "unknown/other"
          next
        }
      }
    } else {
      if (all(prev_test1$lab_specimen_source == c("Blood - capillary", "Blood - venous"))) {
        prevtest <- prev_test1 %>%
          dplyr::filter(lab_specimen_source == "Blood - venous")
      } else {
        # browser()
        df$test_reason[i] <- "unknown/other"
        next
      }
    }

    # Assign test reason
    df$test_reason[i] <- rsn <- dplyr::case_when(
      df$lab_specimen_source[i] == "Blood - capillary" &    # Current src = cap; prev rsn = NA
        is.na(prevtest$test_reason) ~ "cap_scrn",
      df$lab_specimen_source[i] == "Blood - capillary" &    # Current src = cap; prev rsn = cap_scrn
        prevtest$test_reason == "cap_scrn" ~ "cap_cfm",
      df$lab_specimen_source[i] == "Blood - venous" &       # Current src = ven; prev rsn = NA
        is.na(prevtest$test_reason) ~ "ven_cfm_i",
      df$lab_specimen_source[i] == "Blood - venous" &       # Current src = ven; prev rsn = cap_scrn;
        prevtest$test_reason == "cap_scrn" &                # result = elevated
        prevtest$lab_result_elev ~ "ven_cfm_e",
      df$lab_specimen_source[i] == "Blood - venous" &       # Current src = ven; prev rsn = cap_scrn;
        prevtest$test_reason == "cap_scrn" &                # result = non-elevated
        !prevtest$lab_result_elev ~ "ven_cfm_n",
      df$lab_specimen_source[i] == "Blood - venous" &       # Current src = ven; prev src = ven OR
        (prevtest$lab_specimen_source == "Blood - venous" | # (previous rsn = cap_cfm & result = elevated)
           (prevtest$test_reason == "cap_cfm" &
              prevtest$lab_result_elev)) ~ "ven_flw",
      TRUE ~ "unknown/other"                                # All others
    )

    utils::setTxtProgressBar(pb, i)
  }

  close(pb)

  df
}





