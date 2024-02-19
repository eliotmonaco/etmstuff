#### classify_test_reason2() - apply version ####

# df1 = data_core for current quarter
# df2 = previous data_core containing test_reason_ks variable

classify_test_reason2 <- function(df1, df2 = NULL) {
  var_check(df1, var = c(
    "recno", "patient_record_number",
    "lab_result_number", "lab_result_elev"
  ))
  df1 <- df1 %>%
    arrange(lab_collection_date, patient_record_number) %>%
    mutate(test_reason_ks = "") # Changed NA to "" for apply version

  if (!is.null(df2)) {
    var_check(df2, var = c(
      "recno", "patient_record_number",
      "lab_result_number", "lab_result_elev", "test_reason_ks"
    ))
    df2 <- df2 %>%
      arrange(lab_collection_date, patient_record_number)
  }

  f <- function(r, df2) {
    # From df1
    cmr <- r[["patient_record_number"]]
    date <- as.Date(r[["lab_collection_date"]])
    t1 <- date - 92
    t2 <- date - 1
    src <- case_when(
      stringr::str_detect(r[["lab_specimen_source"]], regex("capillary", ignore_case = TRUE)) ~ "cap",
      stringr::str_detect(r[["lab_specimen_source"]], regex("venous", ignore_case = TRUE)) ~ "ven",
      TRUE ~ "unk"
    )

    # paste(cmr, date, t1, t2, src)

    # From df_lookup (df1 + df2)
    df_lookup <- rbind(df1, df2)
    all_tests <- df_lookup %>% # All tests within prior 92 days for same individual
      filter(patient_record_number == cmr, lab_collection_date >= t1, lab_collection_date <= t2)
    if (nrow(all_tests) > 0) {
      prev_test <- all_tests %>% # Most recent test within prior 92 days for same individual
        filter(lab_collection_date == max(lab_collection_date))
      prev_rsn <- prev_test$test_reason_ks
      prev_elev <- prev_test$lab_result_elev
      prev_src <- case_when(
        stringr::str_detect(prev_test$lab_specimen_source, regex("capillary", ignore_case = TRUE)) ~ "cap",
        stringr::str_detect(prev_test$lab_specimen_source, regex("venous", ignore_case = TRUE)) ~ "ven",
        TRUE ~ "unk"
      )
    } else {
      prev_test <- all_tests
      prev_rsn <- prev_elev <- prev_src <- NA
    }

    case_when(
      nrow(prev_test) == 0 & src == "cap" ~ "cap_screening",
      nrow(prev_test) == 0 & src == "ven" ~ "ven_confirm_initial",
      nrow(prev_test) == 1 & src == "ven" & prev_rsn == "cap_screening" & prev_elev == TRUE ~ "ven_confirm_e",
      nrow(prev_test) == 1 & src == "ven" & prev_rsn == "cap_screening" & prev_elev == FALSE ~ "ven_confirm_n",
      nrow(prev_test) == 1 & src == "ven" & prev_src == "ven" ~ "ven_followup",
      TRUE ~ "unknown/other"
    )
  }

  apply(df1, 1, f, df2)
}
