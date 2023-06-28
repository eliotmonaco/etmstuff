#' Merge new records with the CBLS child registry
#'
#' @param df A dataframe of records prepared for the CBLS submission.
#' @param registry The most recent CBLS child registry.
#'
#' @return A list consisting of `df_data` (`df` plus the variable `child_registry_id`), and `df_registry` (the child registry plus any new children from `df`).
#' @export
#'
#' @importFrom magrittr %>%
#'
# @examples
#'
merge_child_registry <- function(df, registry) {
  var_check(df, var = c("patient_id", "lab_collection_date"))
  var_check(registry, var = c("child_registry_id", "patient_id"))

  # Deduplicate registry
  registry2 <- registry %>%
    dplyr::distinct(patient_id, .keep_all = TRUE)

  # Match `patient_id` values in `df` and `registry2`, and pull the corresponding `registry2$child_registry_id`
  f <- function(r) {
    if (r[["patient_id"]] %in% registry2$patient_id) {
      registry2$child_registry_id[which(registry2$patient_id == r[["patient_id"]])]
    } else {
      NA_character_
    }
  }

  df$child_registry_id <- apply(df, 1, f, simplify = TRUE)

  # Get highest `registry$child_registry_id` value
  id_start <- max(as.numeric(registry$child_registry_id)) + 1

  # Split `df` into addresses already in registry, and new addresses
  df_existing <- df %>%
    dplyr::filter(!is.na(child_registry_id))

  df_new <- df %>%
    dplyr::filter(is.na(child_registry_id)) %>%
    dplyr::select(-child_registry_id)

  # Assign ID to new addresses
  df_new <- id_distinct_rows(
    df_new,
    var = "patient_id",
    id_name = "child_registry_id",
    seq_start = id_start,
    digits = 8
  )

  # Reunite `df`
  df <- df_existing %>%
    dplyr::bind_rows(df_new) %>%
    dplyr::arrange(lab_collection_date, patient_id)

  # Add new addresses to `registry`
  registry <- registry %>%
    dplyr::bind_rows(
      df_new %>%
        dplyr::select(child_registry_id, patient_id) %>%
        dplyr::distinct()
    ) %>%
    dplyr::arrange(child_registry_id)

  list(
    df_data = df,
    df_registry = registry
  )
}
