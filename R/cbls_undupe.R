#' Undupe lab records prepared for CBLS submission
#'
#' This function deduplicates (using [undupe()]) a data set prepared for the CBLS submission, keeping only one record per child per sample date (`lab_collection_date`) as dictated by the CBLS submission guidelines. Records with an unknown sample type are ignored (the guidelines do not indicate how to evaluate records with an unknown sample type). A message is generated when all records in a dupeset have an unknown sample type.
#'
#' @param df A dataframe of records ready for submission to CBLS.
#' @param row_id A unique row identifier variable.
#'
#' @return A dataframe with the added variable `cbls_dupe`.
#' @export
#'
# @examples
#'
cbls_undupe <- function(df, row_id) {
  var_check(df, var = c(
    row_id, "patient_id", "age",
    "lab_collection_date",
    "lab_result_number",
    "lab_specimen_source"
  ))

  if (!all(df$age < 6)) {
    stop("`df$age` must be < 6 for all records", call. = FALSE)
  }

  # Find duplicates based on `patient_id` and sample date
  df_dupesets <- undupe(
    df,
    visible_var = c("patient_id", "lab_collection_date"),
    prefix = "cbls_dupe"
  )[["df_dupesets"]]

  dupe_ids <- data.frame(id = unique(df_dupesets$cbls_dupe_id))

  # Dataframe to hold retained duplicate tests
  df_keep <- df[0, ]

  # Algorithm to resolve duplicate tests
  for (i in 1:nrow(dupe_ids)) {
    id <- dupe_ids$id[i]
    df2 <- df_dupesets %>%
      filter(cbls_dupe_id == id)
    # if (any(df2$SAMP_TYPE == 1)) {
    if (any(stringr::str_detect(df2$lab_specimen_source, "Blood - venous"))) {
      # If samples are all venous, take the highest test result.
      # If samples are mixed capillary and venous, take the (highest) venous.
      df2 <- df2 %>%
        filter(stringr::str_detect(df2$lab_specimen_source, "Blood - venous")) %>%
        filter(lab_result_number == max(lab_result_number))
      df_keep <- rbind(df_keep, df2[1, ])
    } else if (any(stringr::str_detect(df2$lab_specimen_source, "Blood - capillary"))) {
      # If the samples are all capillary, take the lowest test result.
      df2 <- df2 %>%
        filter(stringr::str_detect(df2$lab_specimen_source, "Blood - capillary")) %>%
        filter(lab_result_number == min(lab_result_number))
      df_keep <- rbind(df_keep, df2[1, ])
    } else {
      m <- paste0(
        "Unknown sample types in `df$", row_id, "`: ",
        paste(df2[[row_id]], collapse = ", ")
      )
      message(m)
    }
  }

  df_keep <- df_keep %>%
    select(-c(cbls_dupe_id, cbls_dupe_order)) %>%
    mutate(cbls_dupe = "true_keep")

  df_remove <- df_dupesets %>%
    anti_join(df_keep, by = "row_id_src") %>%
    select(-c(cbls_dupe_id, cbls_dupe_order)) %>%
    mutate(cbls_dupe = "true_remove")

  df_nondupes <- df %>%
    anti_join(df_keep, by = "row_id_src") %>%
    anti_join(df_remove, by = "row_id_src") %>%
    mutate(cbls_dupe = "false")

  rbind(df_keep, df_remove, df_nondupes) %>%
    arrange(lab_collection_date, patient_id, cbls_dupe)
}
