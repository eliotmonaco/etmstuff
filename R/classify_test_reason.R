#' Title
#'
#' @param df1 The dataframe targeted for classification.
#' @param df2 A dataframe classified previously.
#' @param bl_ref_val
#' @param max_interval
#'
#' @return
#' @export
#'
#' @examples
classify_test_reason <- function(df1, df2 = NULL, bl_ref_val, max_interval) {
  vars <- c("row_id_src", "patient_id", "lab_result_number", "lab_result_symbol", "test_reason_ks")

  var_check(df1, var = vars[1:4])

  if (!is.null(df2)) {
    var_check(df2, var = vars)
  }

  blrv <- paste0("bl_reference_value_", Sys.Date())

  # Add test_reason_ks (empty) & lab_result_elev (T/F)
  df1 <- df1 %>%
    dplyr::arrange(lab_collection_date, patient_id) %>%
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
      test_reason_ks = NA
    )

  if (!is.null(df2)) {
    df2 <- df2 %>%
      dplyr::arrange(lab_collection_date, patient_id) %>%
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

  pb <- txtProgressBar(1, nrow(df1), width = 50, style = 3)

  for (i in 1:nrow(df1)) {
    id <- df1$patient_id[i]
    t1 <- df1$lab_collection_date[i] - max_interval
    t2 <- df1$lab_collection_date[i] - 1
    src <- dplyr::case_when(
      df1$lab_specimen_source[i] == "Blood - capillary" ~ "cap",
      df1$lab_specimen_source[i] == "Blood - venous" ~ "ven",
      TRUE ~ "unk"
    )

    # All tests within prior `max_interval` days for same individual
    all_tests <- df1 %>%
      dplyr::bind_rows(df2) %>%
      dplyr::filter(
        patient_id == id,
        lab_collection_date >= t1,
        lab_collection_date <= t2
      )

    if (nrow(all_tests) > 0) {
      browser()
      # Most recent test within prior `max_interval` days for same individual
      prev_test <- all_tests %>%
        dplyr::filter(lab_collection_date == max(lab_collection_date))
      prev_rsn <- prev_test$test_reason_ks
      prev_elev <- prev_test$lab_result_elev
      prev_src <- dplyr::case_when(
        prev_test$lab_specimen_source == "Blood - capillary" ~ "cap",
        prev_test$lab_specimen_source == "Blood - venous" ~ "ven",
        TRUE ~ "unk"
      )
    } else {
      prev_test <- all_tests
      prev_rsn <- prev_elev <- prev_src <- NA
    }

    rsn <- dplyr::case_when(
      nrow(prev_test) == 0 & src == "cap" ~ "cap_screening",
      nrow(prev_test) == 0 & src == "ven" ~ "ven_confirm_initial",
      nrow(prev_test) == 1 & src == "ven" & prev_rsn == "cap_screening" & prev_elev == TRUE ~ "ven_confirm_e",
      nrow(prev_test) == 1 & src == "ven" & prev_rsn == "cap_screening" & prev_elev == FALSE ~ "ven_confirm_n",
      nrow(prev_test) == 1 & src == "ven" & prev_src == "ven" ~ "ven_followup",
      TRUE ~ "unknown/other"
    )

    df1$test_reason_ks[i] <- rsn

    setTxtProgressBar(pb, i)
    # message(i) # If function gets hung, indicates which record
  }

  close(pb)

  df1
}





