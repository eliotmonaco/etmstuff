#' Merge new geocoded addresses with the CBLS address registry
#'
#' @param df A dataframe of geocoded addresses for the CBLS submission.
#' @param registry The most recent CBLS address registry.
#'
#' @return A list consisting of `df_addr` (`df` plus the variable `address_registry_id`), and `df_registry` (the address registry plus any new addresses from `df`).
#' @export
#'
#' @importFrom magrittr %>%
#'
# @examples
#'
merge_address_registry <- function(df, registry) {
  var_check(df, var = c("dupe_id", md_response_vars))
  var_check(registry, var = c("address_registry_id", md_response_vars))

  # Preserve original row order
  orig_order <- df$dupe_id

  # Melissa Data response variables to define a distinct address
  reg_id_vars <- c("AddressLine1", "Suite", "City", "State", "PostalCode", "CountyName")

  # Add `address_registry_id` from `registry` to `df` if `reg_id_vars` match
  df <- df %>%
    dplyr::left_join(
      registry %>%
        dplyr::select(address_registry_id, tidyselect::all_of(md_response_vars)),
      by = md_response_vars
    )

  # Starting number for new `address_registry_id` values
  id_start <- max(as.numeric(registry$address_registry_id)) + 1

  # Subset new addresses
  df_new <- df %>%
    dplyr::filter(is.na(address_registry_id)) %>%
    dplyr::select(-address_registry_id)

  # Assign `address_registry_id` to new addresses
  df_new <- id_distinct_rows(
    df_new,
    var = reg_id_vars,
    id_name = "address_registry_id",
    seq_start = id_start,
    digits = 8
  )

  # Add new addresses back to `df`
  df <- df %>%
    dplyr::filter(!is.na(address_registry_id)) %>%
    dplyr::bind_rows(df_new) %>%
    dplyr::relocate(address_registry_id, .before = AddressDeliveryInstallation)

  # Sort `df` by original row order
  df <- df[order(factor(df$dupe_id, levels = orig_order)),]

  # Add new addresses to `registry`
  registry <- registry %>%
    dplyr::bind_rows(
      df_new %>%
        dplyr::select(address_registry_id, tidyselect::all_of(md_response_vars)) %>%
        dplyr::distinct(address_registry_id, .keep_all = TRUE)
    ) %>%
    dplyr::arrange(address_registry_id)

  list(
    df_addr = df,
    df_registry = registry
  )
}
