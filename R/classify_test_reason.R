#' Impute reasons for series of blood lead tests
#'
#' @param df The dataframe targeted for classification.
#' @param df2 A dataframe classified previously (optional).
#' @param bl_ref_val The blood lead reference value in mcg/dL (numeric). A blood lead test result >= `bl_rev_val` is elevated.
#' @param max_interval The maximum number of days between tests belonging to the same test sequence (an integer).
#'
#' @return
#' A dataframe is returned with a new column, `test_reason`. Possible values are:
#'
#' * `cap_scrn`: a capillary screening test.
#' * `ven_cfm_i`: an initial confirmatory venous test (i.e., not preceded by a capillary screening test).
#' * `ven_cfm_e`: a confirmatory venous test following an elevated capillary screening test.
#' * `ven_cfm_n`: a confirmatory venous test following a non-elevated capillary screening test.
#' * `ven_flw`: a venous follow-up test.
#'
#' @export
#'
# @examples
#'
classify_test_reason <- function(df, df2 = NULL, bl_ref_val, max_interval) {
  vars <- c("row_id_src", "patient_id", "lab_result_symbol", "lab_result_number", "lab_specimen_source", "test_reason")

  var_check(df, var = vars[1:5])

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
      # Add column for the blood lead reference value used
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
    # Get previous (or same-day) test within `max_interval` for same `patient_id`
    prevtest <- df %>%
      dplyr::bind_rows(df2) %>%
      dplyr::filter(
        patient_id == df$patient_id[i],
        lab_collection_date <= df$lab_collection_date[i],
        lab_collection_date >= df$lab_collection_date[i] - max_interval,
        row_id_src != df$row_id_src[i]
      ) %>%
      dplyr::slice_max(lab_collection_date, n = 1, with_ties = TRUE)

    if (nrow(prevtest) == 0) {
      prevtest <- df_na
    } else if (nrow(prevtest) == 1) {
      if (prevtest$lab_collection_date != df$lab_collection_date[i]) {
        # If previous test is NOT same-day...
        # Do nothing
      } else {
        # If previous test IS same-day...
        if (prevtest$lab_specimen_source == "Blood - capillary" &
            df$lab_specimen_source[i] == "Blood - venous") {
          # Do nothing
        } else if (prevtest$lab_specimen_source == "Blood - venous" &
                   df$lab_specimen_source[i] == "Blood - capillary") {
          prevtest <- df_na
        } else {
          df$test_reason[i] <- "CHECK"
          next
          # stop("\nUnexpected multiple same-day tests for `patient_id`: ", df$patient_id[i], call. = FALSE)
        }
      }
    } else {
      df$test_reason[i] <- "CHECK"
      next
      # stop("\nUnexpected multiple same-day tests for `patient_id`: ", df$patient_id[i], call. = FALSE)
    }

    # Assign test reason
    df$test_reason[i] <- rsn <- dplyr::case_when(
      df$lab_specimen_source[i] == "Blood - capillary" & # Current test = cap. No previous test.
        is.na(prevtest$test_reason) ~ "cap_scrn",
      df$lab_specimen_source[i] == "Blood - venous" &    # Current test = ven. No previous test.
        is.na(prevtest$test_reason) ~ "ven_cfm_i",
      df$lab_specimen_source[i] == "Blood - venous" &    # Current test = ven. Previous test = cap. & elevated.
        prevtest$test_reason == "cap_scrn" &
        prevtest$lab_result_elev ~ "ven_cfm_e",
      df$lab_specimen_source[i] == "Blood - venous" &    # Current test = ven. Previous test = cap. & non-elevated.
        prevtest$test_reason == "cap_scrn" &
        !prevtest$lab_result_elev ~ "ven_cfm_n",
      df$lab_specimen_source[i] == "Blood - venous" &    # Current test = ven. Previous test = ven.
        prevtest$lab_specimen_source == "Blood - venous" ~ "ven_flw",
      TRUE ~ "unknown/other"                             # All others
    )

    utils::setTxtProgressBar(pb, i)
  }

  close(pb)

  df
}





