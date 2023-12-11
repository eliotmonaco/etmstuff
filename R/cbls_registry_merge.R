#' Merge new CBLS data with the relevant registry
#'
#' This function merges data (either geocoded addresses or children) produced by the CBLS data processing workflow with the relevant CBLS registry. Registry IDs are assigned to items already present in the registry, and new IDs are created for items new to the registry.
#'
#' @param df_data A dataframe of addresses or children produced during the CBLS data processing workflow.
#' @param df_reg A dataframe representing the most recently updated CBLS address or child registry.
#' @param type The type of registry being updated. One of `c("address", "child")`.
#'
#' @return A list containing
#' * `df_data`: `df_data` plus the registry ID variable
#' * `df_registry`: `df_reg` plus the new addresses or children from `df_data`
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
# @examples
#'
cbls_registry_merge <- function(df_data, df_reg, type = c("address", "child")) {
  if (type == "address") {
    reg_id = "address_registry_id"
    reg_vars_id <- c("AddressLine1", "Suite", "City", "State", "PostalCode", "CountyName")
  } else if (type == "child") {
    reg_id = "child_registry_id"
    reg_vars_id <- "patient_id"
  } else {
    stop("`type` must be one of c(\"address\", \"child\")")
  }

  # All registry variables
  reg_vars_all <- colnames(df_reg)

  # All registry variables other those that determine distinct items in the registry (`reg_vars_id`) and the ID variable
  reg_vars_other <- reg_vars_all[!reg_vars_all %in% c(reg_id, reg_vars_id)]

  var_check(df_data, var = c("dupe_id", reg_vars_id, reg_vars_other))
  var_check(df_reg, var = c(reg_id, reg_vars_id))

  # Preserve original row order
  orig_order <- df_data$dupe_id

  # Add `reg_id` from `df_reg` to `df_data` if `reg_vars_id` match
  df_data <- df_data %>%
    dplyr::left_join(
      df_reg %>%
        dplyr::select(tidyselect::all_of(c(reg_id, reg_vars_id))),
      by = reg_vars_id
    )

  # Starting number for new `reg_id` values
  id_start <- max(as.numeric(df_reg[[reg_id]])) + 1

  # Subset items not already in `df_reg`
  df_data_new <- df_data %>%
    dplyr::filter(is.na(.data[[reg_id]])) %>%
    dplyr::select(-tidyselect::all_of(reg_id))

  # Assign `reg_id` to new items
  df_data_new <- id_distinct_rows(
    df_data_new,
    var = reg_vars_id,
    id_name = reg_id,
    seq_start = id_start,
    digits = 8
  )

  # Add `df_data_new` back to `df_data`
  df_data <- df_data %>%
    dplyr::filter(!is.na(.data[[reg_id]])) %>%
    dplyr::bind_rows(df_data_new)

  # Sort `df_data` by original row order
  df_data <- df_data[order(factor(df_data$dupe_id, levels = orig_order)),]

  # Reset row names (if not a tibble)
  if (!"tbl_df" %in% class(df_data)) {
    rownames(df_data) <- 1:nrow(df_data)
  }

  # Add new items to `df_reg`
  df_reg <- df_reg %>%
    dplyr::bind_rows(
      df_data_new %>%
        dplyr::select(tidyselect::all_of(reg_vars_all)) %>%
        dplyr::distinct(dplyr::across(tidyselect::all_of(reg_id)), .keep_all = TRUE)
    ) %>%
    dplyr::arrange(.data[[reg_id]])

  list(
    df_data = df_data,
    df_registry = df_reg
  )
}
