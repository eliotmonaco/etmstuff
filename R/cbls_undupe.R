#' Undupe records prepared for CBLS submission
#'
#' This function deduplicates (using [undupe()]) a data set prepared for the CBLS submission, keeping only one record per child per sample date (`lab_collection_date`) as dictated by the CBLS submission guidelines. Records with an unknown sample type are ignored (the guidelines do not indicate how to evaluate records with an unknown sample type). A message is generated when all records in a dupeset have an unknown sample type.
#'
#' @param df A dataframe of records ready for submission to CBLS.
#' @param row_id A unique row identifier variable in `df`.
#'
#' @return A dataframe with the added variable `cbls_duplicate`.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
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
    stop("`age` must be < 6 for all records")
  }

  # Find duplicates based on `patient_id` and sample date
  vars <- c("patient_id", "lab_collection_date")
  df_dupesets <- undupe(df, var = vars, prefix = "cbls_dupe")$df_dupesets

  dupe_ids <- unique(df_dupesets$cbls_dupe_id)

  # Empty dataframe to hold duplicates that will be kept for the submission
  df_keep <- df[0, ]

  # Algorithm to resolve duplicate tests
  for (i in 1:length(dupe_ids)) {
    df2 <- df_dupesets %>%
      dplyr::filter(.data$cbls_dupe_id == dupe_ids[i])
    if (any(df2$lab_specimen_source == "Blood - venous")) {
      # If samples are all venous, take the highest test result
      # If samples are mixed capillary and venous, take the (highest) venous
      df2 <- df2 %>%
        dplyr::filter(.data$lab_specimen_source == "Blood - venous") %>%
        dplyr::slice_max(.data$lab_result_number, n = 1, with_ties = FALSE)
      df_keep <- rbind(df_keep, df2)
    } else if (any(df2$lab_specimen_source == "Blood - capillary")) {
      # If the samples are all capillary, take the lowest test result
      df2 <- df2 %>%
        dplyr::filter(.data$lab_specimen_source == "Blood - capillary") %>%
        dplyr::slice_min(.data$lab_result_number, n = 1, with_ties = FALSE)
      df_keep <- rbind(df_keep, df2)
    } else {
      m <- paste0(
        "Unknown sample types in `df$", row_id, "`: ",
        paste(df2[[row_id]], collapse = ", ")
      )
      message(m)
    }
  }

  df_keep <- df_keep %>%
    dplyr::select(-c("cbls_dupe_id", "cbls_dupe_order")) %>%
    dplyr::mutate(cbls_duplicate = "true_keep")

  df_remove <- df_dupesets %>%
    dplyr::anti_join(df_keep, by = row_id) %>%
    dplyr::select(-c("cbls_dupe_id", "cbls_dupe_order")) %>%
    dplyr::mutate(cbls_duplicate = "true_remove")

  df_nondupes <- df %>%
    dplyr::anti_join(df_keep, by = row_id) %>%
    dplyr::anti_join(df_remove, by = row_id) %>%
    dplyr::mutate(cbls_duplicate = "false")

  rbind(df_keep, df_remove, df_nondupes) %>%
    dplyr::arrange(.data$lab_collection_date, .data$patient_id, .data$cbls_duplicate)
}
