#' Pull addresses from EpiTrax data
#'
#' @description
#' Subset the complete address at lab collection from EpiTrax data. Addresses with insufficient data for validation via Melissa Data are omitted.
#'
#' @section Address validation workflow:
#'
#' 1. [pull_addresses()]
#' 2. Clean addresses
#'     - [validate_address()]
#'     - [clean_street_address()]
#'     - [replace_values()]
#' 3. [parse_street_addresses()]
#' 4. [compare_parsed_street()]
#' 5. [build_md_url()]
#' 6. [md_batch_request()]
#'
#' @section Melissa Data:
#' The address validation workflow prepares addresses for validation by the Melissa Data Personator Consumer Web Service. Minimum data requirements for requests are either of these value combinations:
#'
#' * `street`, `city`, and `state`
#' * `street` and `zip`
#'
#' More information is at <https://wiki.melissadata.com/index.php?title=Personator_Consumer>.
#'
#' @param df A dataframe of records from EpiTrax.
#' @param row_id The name of the unique identifier for rows. Defaults to `row_id_src`.
#'
#' @return A dataframe of addresses. Columns are renamed to their generic address components. `street_src` is an additional street variable intended to preserve the street address from the source data for comparison to the cleaned and parsed street addresses (created later).
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @family address processing functions
# @examples
#'
pull_addresses <- function(df, row_id = "row_id_src") {
  var_check(df, var = c(
    row_id,
    "lab_collection_street", "lab_collection_unit_number", "lab_collection_city",
    "lab_collection_state", "lab_collection_postal_code", "lab_collection_county"
  ))

  df %>%
    dplyr::mutate(
      street = stringr::str_squish(lab_collection_street),
      unit = stringr::str_squish(lab_collection_unit_number),
      city = stringr::str_squish(lab_collection_city),
      state = stringr::str_squish(lab_collection_state),
      zip = stringr::str_squish(lab_collection_postal_code),
      county = stringr::str_squish(lab_collection_county)
    ) %>%
    dplyr::mutate(street_src = street) %>%
    dplyr::select(
      tidyselect::all_of(row_id),
      street_src,
      street, unit, city, state, zip, county
    ) %>%
    # Remove insufficient addresses
    dplyr::filter(!is.na(street) & (!is.na(city) | !is.na(zip)))
}
