#' Pull addresses from EpiTrax data
#'
#' @description
#' Subset address fields from EpiTrax data. The returned dataframe has one row per address. `recno` and the added variable `address_src` serve as a key to link each address to the original record. Addresses with insufficient data for geocoding are omitted.
#'
#' @section Address processing workflow:
#' 1. [pull_addresses()]
#' 2. [parse_street_addresses()]
#' 3. compare...
#' 4. [build_md_url()]
#' 5. [submit_to_md()]
#'
#' @section Address validation and geocoding:
#' The address processing workflow prepares addresses for validation by the Melissa Data Personator Consumer Web Service. Minimum data requirements for the request are either of these value combinations:
#'
#' * `street`, `city`, and `state`
#' * `street` and `zip`
#'
#' More information is at https://wiki.melissadata.com/index.php?title=Personator_Consumer
#'
#' @param df A dataframe of records from EpiTrax.
#'
#' @return A dataframe of addresses and keys.
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

  # Subset and stack addresses
  df %>%
    # Address at lab collection
    dplyr::select(recno, lab_collection_street:lab_collection_postal_code) %>%
    dplyr::mutate(address_src = "1_lab") %>%
    dplyr::rename(
      street = lab_collection_street,
      unit = lab_collection_unit_number,
      city = lab_collection_city,
      state = lab_collection_state,
      zip = lab_collection_postal_code,
      county = lab_collection_county
    ) %>%
    dplyr::full_join(df %>%
      # Current address
      dplyr::select(recno, current_address_street:current_address_zip) %>%
      dplyr::mutate(address_src = "2_cur") %>%
      dplyr::rename(
        street = current_address_street,
        unit = current_address_unit_number,
        city = current_address_city,
        state = current_address_state,
        zip = current_address_zip,
        county = current_address_county
      )) %>%
    dplyr::relocate(county, .after = zip) %>%
    standardize(uppercase = FALSE, var_ignore = c("recno", "address_src")) %>%
    # Remove insufficient addresses
    dplyr::filter(!is.na(street) & (!is.na(city) | !is.na(zip)))
}
