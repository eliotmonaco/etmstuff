#' Pull addresses from EpiTrax data
#'
#' @description
#' Subset the complete address at lab collection from EpiTrax data. Addresses with insufficient data for validation via Melissa Data are omitted.
#'
#' @section Address validation workflow:
#' The address validation workflow prepares addresses for validation by the Melissa Data Personator Consumer Web Service.
#'
#' 1. Prepare addresses
#'     - [pull_addresses()]
#' 2. Clean addresses
#'     - [validate_address()]
#'     - [clean_address()]
#'     - [replace_values()]
#' 3. Validate/geocode addresses
#'     - [build_md_url()]
#'     - [send_md_request()]
#' 4. Review results
#'     - [md_results_table()]
#'
#' @section Melissa Data:
#' To check addresses, one of the follow combinations of address fields must be provided at a minimum:
#'
#' * `street`, `city`, and `state`
#' * `street` and `zip`
#'
#' For more information on constructing the request, see the [Personator Consumer Quick Start Guide](https://www.melissa.com/quickstart-guides/personator-consumer) and the [Personator Consumer Request wiki page](https://wiki.melissadata.com/index.php?title=Personator_Consumer:Request). Additional information can be found at the [Melissa Data wiki](https://wiki.melissadata.com/index.php?title=Personator_Consumer).
#'
#' @param df A dataframe of records from EpiTrax.
#' @param row_id A unique row identifier variable in `df`.
#'
#' @return A dataframe of addresses. Columns are renamed to their generic address components. `street_src` is an additional street variable intended to preserve the street address from the source data for comparison to the cleaned and parsed street addresses (created later).
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @family address processing functions
# @examples
#'
pull_addresses <- function(df, row_id) {
  var_check(df, var = c(
    row_id,
    "lab_collection_street", "lab_collection_unit_number", "lab_collection_city",
    "lab_collection_state", "lab_collection_postal_code", "lab_collection_county"
  ))

  df %>%
    dplyr::mutate(
      street = stringr::str_squish(.data$lab_collection_street),
      unit = stringr::str_squish(.data$lab_collection_unit_number),
      city = stringr::str_squish(.data$lab_collection_city),
      state = stringr::str_squish(.data$lab_collection_state),
      zip = stringr::str_squish(.data$lab_collection_postal_code),
      county = stringr::str_squish(.data$lab_collection_county)
    ) %>%
    dplyr::mutate(street_src = .data$street) %>%
    dplyr::select(
      tidyselect::all_of(row_id),
      "street_src",
      "street", "unit", "city", "state", "zip", "county"
    ) %>%
    # Remove insufficient addresses
    dplyr::filter(!is.na(.data$street) & (!is.na(.data$city) | !is.na(.data$zip)))
}
