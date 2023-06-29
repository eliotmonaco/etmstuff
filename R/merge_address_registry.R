#' Merge new cleaned addresses with the CBLS address registry
#'
#' @param df A dataframe of cleaned addresses prepared for the CBLS submission.
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
  vars_addr <- c("street", "unit", "city", "state", "zip", "county")

  var_check(df, var = vars_addr)
  var_check(registry, var = c("address_registry_id", vars_addr, "cnty_fips"))

  # Create `lookup` value from address variables
  f1 <- function(df, var) {
    df %>%
      dplyr::select(tidyselect::all_of(var)) %>%
      dplyr::mutate(dplyr::across(tidyselect::everything(), stringr::str_to_upper)) %>%
      tidyr::unite(
        col = "lookup",
        tidyselect::everything(),
        sep = "+",
        remove = TRUE,
        na.rm = FALSE
      )
  }

  # Add `df$lookup`
  lookup1 <- f1(df, var = vars_addr)

  df <- df %>%
    dplyr::bind_cols(lookup1)

  # Add `registry$lookup`
  lookup2 <- f1(registry, var = vars_addr)

  registry <- registry %>%
    dplyr::bind_cols(lookup2)

  # Deduplicate registry
  registry2 <- registry %>%
    dplyr::distinct(lookup, .keep_all = TRUE)

  # Match `lookup` values in `df` and `registry2`, and pull the corresponding `registry2$address_registry_id`
  f2 <- function(r) {
    if (r[["lookup"]] %in% registry2$lookup) {
      registry2$address_registry_id[which(registry2$lookup == r[["lookup"]])]
    } else {
      NA_character_
    }
  }

  df$address_registry_id <- apply(df, 1, f2, simplify = TRUE)

  # Get highest `registry$address_registry_id` value
  id_start <- max(as.numeric(registry$address_registry_id)) + 1

  # Split `df` into addresses already in registry, and new addresses
  df_existing <- df %>%
    dplyr::filter(!is.na(address_registry_id))

  # Add county FIPS codes to new addresses
  df_new <- df %>%
    dplyr::filter(is.na(address_registry_id)) %>%
    dplyr::select(-address_registry_id) %>%
    dplyr::left_join(
      fips %>%
        dplyr::mutate(county = paste(county, "County")) %>%
        dplyr::select(county, cnty_fips),
      by = "county"
    )

  # Assign ID to new addresses
  df_new <- id_distinct_rows(
    df_new,
    var = vars_addr,
    id_name = "address_registry_id",
    seq_start = id_start,
    digits = 8
  )

  # Reunite `df`
  df <- df_existing %>%
    dplyr::bind_rows(
      df_new %>%
        dplyr::select(-cnty_fips)
    ) %>%
    dplyr::select(-lookup) %>%
    dplyr::arrange(address_registry_id)

  # Add new addresses to `registry`
  registry <- registry %>%
    dplyr::select(-lookup) %>%
    dplyr::bind_rows(
      df_new %>%
        dplyr::select(-c(lookup, dupe_id, address_id)) %>%
        dplyr::distinct()
    ) %>%
    dplyr::arrange(address_registry_id)

  list(
    df_addr = df,
    df_registry = registry
  )
}
