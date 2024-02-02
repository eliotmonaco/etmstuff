#' Kansas counties and FIPS codes
#'
#' @name ks_county_fips
#'
#' @description A list of Kansas counties and their corresponding 2020 census codes. Table downloaded from Census.gov.
#'
#' @usage ks_county_fips
#'
#' @format A dataframe with 7 columns and 105 rows.
#' \describe{
#'   \item{county}{Short county name (does not include "County")}
#'   \item{county_full}{Full county name (includes "County")}
#'   \item{state_fips}{State FIPS code}
#'   \item{county_fips}{County FIPS code}
#'   \item{county_ns}{County NS code}
#'   \item{fips_class}{FIPS class code}
#'   \item{func_stat}{Functional status code}
#' }
#'
#' @source Downloaded from the US Census Bureau website (https://www.census.gov/library/reference/code-lists/ansi.html#cou).
#'
#' @keywords datasets
"ks_county_fips"
