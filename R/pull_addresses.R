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
#' To use the Check action to validate and correct addresses, Melissa Data requires, at minimum, one of the follow combinations of address fields:
#'
#' * `street`, `city`, and `state`
#' * `street` and `zip`
#'
#' For more information on constructing the request, see the [Personator Consumer Quick Start Guide](https://www.melissa.com/quickstart-guides/personator-consumer) and the [Personator Consumer Request wiki page](https://wiki.melissadata.com/index.php?title=Personator_Consumer:Request). Additional information can be found at the [Melissa Data wiki](https://wiki.melissadata.com/index.php?title=Personator_Consumer).
#'
#' @param df A dataframe of records from EpiTrax.
#' @param row_id A unique row identifier variable in `df`.
#' @param var A vector of variable names for the following address components in this order: 1) house number and street, 2) unit number, 3) city, 4) state, 5) zip code, and 6) county. Defaults to `c("coll_add_street", "coll_add_unit", "coll_add_city", "coll_add_state", "coll_add_zip", "coll_add_county")`.
#'
#' @return A dataframe of addresses. Columns are renamed to their generic address components. `street_src` is a copy of `street`, which preserves the original string for comparison during cleaning.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @family address processing functions
# @examples
#'
pull_addresses <- function(df, row_id, var = c("coll_add_street", "coll_add_unit", "coll_add_city", "coll_add_state", "coll_add_zip", "coll_add_county")) {
  if (!is.data.frame(df)) stop("`df` must be a dataframe")
  if (length(row_id) != 1) stop("`row_id` must have length of 1")

  var_check(df, var = c(row_id, var))

  df %>%
    dplyr::mutate(
      street = stringr::str_squish(.data[[var[1]]]),
      unit = stringr::str_squish(.data[[var[2]]]),
      city = stringr::str_squish(.data[[var[3]]]),
      state = stringr::str_squish(.data[[var[4]]]),
      zip = stringr::str_squish(.data[[var[5]]]),
      county = stringr::str_squish(.data[[var[6]]])
    ) %>%
    dplyr::mutate(street_src = .data$street) %>%
    dplyr::select(
      tidyselect::all_of(row_id), "street_src",
      "street", "unit", "city", "state", "zip", "county"
    ) %>%
    # Remove insufficient addresses
    dplyr::filter(!is.na(.data$street) & (!is.na(.data$city) | !is.na(.data$zip)))
}
