#' Pull addresses from EpiTrax data
#'
#' @param df A dataframe.
#'
#' @return A dataframe of addresses only.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @family address processing functions
# @examples
#'
pull_addresses <- function(df) {
  var_check(df, var = c(
    "recno",
    "lab_collection_street", "lab_collection_unit_number", "lab_collection_city",
    "lab_collection_state", "lab_collection_postal_code", "lab_collection_county",
    "current_address_street", "current_address_unit_number", "current_address_city",
    "current_address_state", "current_address_zip", "current_address_county"
  ))

  # Subset and stack address at lab collection & current address fields
  df %>%
    dplyr::select(recno, lab_collection_street:lab_collection_postal_code) %>%
    dplyr::mutate(address_src = "1_lab") %>%
    dplyr::rename(
      street = lab_collection_street, unit = lab_collection_unit_number,
      city = lab_collection_city, state = lab_collection_state,
      zip = lab_collection_postal_code, county = lab_collection_county
    ) %>%
    dplyr::full_join(df %>%
      dplyr::select(recno, current_address_street:current_address_zip) %>%
      dplyr::mutate(address_src = "2_cur") %>%
      dplyr::rename(
        street = current_address_street, unit = current_address_unit_number,
        city = current_address_city, state = current_address_state,
        zip = current_address_zip, county = current_address_county
      )) %>%
    dplyr::relocate(county, .after = zip) %>%
    standardize(uppercase = FALSE, var_ignore = c("recno", "address_src")) %>%
    # Remove insufficient addresses
    dplyr::filter(!is.na(street) & (!is.na(city) | !is.na(zip)))
}
