#' Impute the reasons for a series of blood lead tests
#'
#' @param df The dataframe targeted for classification.
#' @param df2 A dataframe classified previously (optional).
#' @param bl_ref_val The blood lead reference value in mcg/dL (numeric). A blood lead test result >= `bl_rev_val` is elevated.
#' @param max_interval The maximum number of days between tests belonging to the same test sequence (an integer).
#'
#' @return
#' A dataframe is returned with a new column, `test_reason`. Possible values are:
#'
#' * `cap_scrn`:
#' * `ven_cfm_i`:
#' * `ven_cfm_e`:
#' * `ven_cfm_n`:
#' * `ven_flw`:
#'
#' @export
#'
# @examples
#'
classify_test_reason <- function(df, df2 = NULL, bl_ref_val, max_interval) {
  vars <- c("row_id_src", "patient_id", "lab_result_number", "lab_result_symbol", "test_reason")

  var_check(df, var = vars[1:4])

  if (!is.null(df2)) {
    var_check(df2, var = vars)
  }

  blrv <- paste0("bl_reference_value_", Sys.Date())

  # Add test_reason (empty) & lab_result_elev (T/F)
  df <- df %>%
    dplyr::arrange(lab_collection_date, patient_id, lab_specimen_source) %>%
    dplyr::mutate(
      lab_result_elev = dplyr::case_when(
        is.na(lab_result_symbol) & lab_result_number < bl_ref_val ~ FALSE,
        is.na(lab_result_symbol) & lab_result_number >= bl_ref_val ~ TRUE,
        lab_result_symbol == "<" & lab_result_number <= bl_ref_val ~ FALSE,
        lab_result_symbol == "<" & lab_result_number > bl_ref_val ~ NA,
        lab_result_symbol == ">" & lab_result_number < bl_ref_val ~ NA,
        lab_result_symbol == ">" & lab_result_number >= bl_ref_val ~ TRUE
      ),
      {{ blrv }} := bl_ref_val,
      test_reason = NA
    )

  if (!is.null(df2)) {
    df2 <- df2 %>%
      dplyr::arrange(lab_collection_date, patient_id, lab_specimen_source) %>%
      dplyr::mutate(
        lab_result_elev = case_when(
          is.na(lab_result_symbol) & lab_result_number < bl_ref_val ~ FALSE,
          is.na(lab_result_symbol) & lab_result_number >= bl_ref_val ~ TRUE,
          lab_result_symbol == "<" & lab_result_number <= bl_ref_val ~ FALSE,
          lab_result_symbol == "<" & lab_result_number > bl_ref_val ~ NA,
          lab_result_symbol == ">" & lab_result_number < bl_ref_val ~ NA,
          lab_result_symbol == ">" & lab_result_number >= bl_ref_val ~ TRUE
        ),
        {{ blrv }} := bl_ref_val
      )
  }

  # Create `df` row with NA values for loop
  df_na <- df[1,]
  df_na[1,] <- NA

  pb <- txtProgressBar(1, nrow(df), width = 50, style = 3)

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

    # what if there is >1 test on max(lab_collection_date)?
    # what if spec src of prev test is unknown? of current test?

    if (nrow(prevtest) == 0) {
      prevtest <- df_na
    } else if (nrow(prevtest) == 1) {
      if (prevtest$lab_collection_date != df$lab_collection_date[i]) { # Previous test is NOT same-day
        # Do nothing
      } else { # Previous test IS same-day
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

    df$test_reason[i] <- rsn <- dplyr::case_when(
      df$lab_specimen_source[i] == "Blood - capillary" &
        is.na(prevtest$test_reason) ~ "cap_scrn",
      df$lab_specimen_source[i] == "Blood - venous" &
        is.na(prevtest$test_reason) ~ "ven_cfm_i",
      df$lab_specimen_source[i] == "Blood - venous" &
        prevtest$test_reason == "cap_scrn" &
        prevtest$lab_result_elev ~ "ven_cfm_e",
      df$lab_specimen_source[i] == "Blood - venous" &
        prevtest$test_reason == "cap_scrn" &
        !prevtest$lab_result_elev ~ "ven_cfm_n",
      df$lab_specimen_source[i] == "Blood - venous" &
        prevtest$lab_specimen_source == "Blood - venous" ~ "ven_flw",
      TRUE ~ "unknown/other"
    )

    setTxtProgressBar(pb, i)
    # message(i) # If function gets hung, indicates which record
  }

  close(pb)

  df
}





