#' Build a Melissa Data Personator REST request URL
#'
#' @description
#' This function builds a URL to query the Melissa Data Personator Consumer Web Service API.
#'
#' @inheritSection pull_addresses Address validation workflow
#' @inheritSection pull_addresses Melissa Data
#'
#' @param df A dataframe of addresses.
#' @param row_id A unique row identifier variable in `df`.
#' @param var A vector of variable names for the following address components in this order: 1) house number and street, 2) unit number, 3) city, 4) state, and 5) zip code. Defaults to `c("street", "unit", "city", "state", "zip")`.
#'
#' @return A vector of URLs.
#' @export
#'
#' @family address processing functions
#' @examples
#' df <- test_addresses
#'
#' df$md_url <- build_md_url(df, "address_id")
#'
build_md_url <- function(df, row_id, var = c("street", "unit", "city", "state", "zip")) {
  if (!is.data.frame(df)) stop("`df` must be a dataframe")
  if (length(row_id) != 1) stop("`row_id` must have length of 1")

  var_check(df, var = c(row_id, var))

  paste0(
    # REST request URL
    "http://personator.melissadata.net/v3/WEB/ContactVerify/doContactVerify",
    # Transmission reference (unique row ID)
    "?t=", utils::URLencode(df[[row_id]]),
    # Melissa license key
    "&id=HX1erDnVvumudTEscOCub-**",
    # Action = check (validate and correct)
    "&act=Check",
    # Specify column output
    "&cols=Suite,GrpParsedAddress,GrpAddressDetails,GrpCensus,GrpCensus2,GrpGeocode",
    # Options
    "&opt=UsePreferredCity:on",
    # Address components
    "&a1=", utils::URLencode(paste(stats::na.omit(df[[var[1]]], df[[var[2]]]))),
    "&city=", utils::URLencode(df[[var[3]]]),
    "&postal=", utils::URLencode(df[[var[5]]]),
    "&state=", utils::URLencode(df[[var[4]]]),
    "&format=JSON"
  )
}
