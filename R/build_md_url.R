#' Build a Melissa Data Personator REST request URL
#'
#' @description
#' This function builds a URL to query the Melissa Data Personator Consumer Web Service API. It assumes that the dataframe contains variables named `street`, `unit`, `city`, `state`, and `zip` which contain the corresponding address components.
#'
#' @inheritSection pull_addresses Address validation workflow
#' @inheritSection pull_addresses Melissa Data
#'
#' @param df A dataframe of cleaned addresses.
#' @param row_id A unique row identifier variable in `df`.
# @param street The name of the variable containing the street address. Unit information should be included if the `unit` argument is left `NULL`.
# @param unit The name of the variable containing the unit component of the street address (if the unit is not already included in `street`).
# @param city The name of the variable containing the city.
# @param state The name of the variable containing the state.
# @param zip The name of the variable containing the zip code.
#'
#' @return A vector of URLs.
#' @export
#'
#' @family address processing functions
# @examples
#'
build_md_url <- function(df, row_id) {
  var_check(df, var = c(row_id, "street", "unit", "city", "state", "zip"))

  row_id <- df[[row_id]]

  street <- df$street; unit <- df$unit; city <- df$city; state <- df$state; zip <- df$zip

  paste0(
    # REST request URL
    "http://personator.melissadata.net/v3/WEB/ContactVerify/doContactVerify",
    # Transmission reference (unique row ID)
    "?t=", utils::URLencode(row_id),
    # Melissa license key
    "&id=HX1erDnVvumudTEscOCub-**",
    # Action = check (validate and correct)
    "&act=Check",
    # Specify column output
    "&cols=Suite,GrpParsedAddress,GrpAddressDetails,GrpCensus,GrpCensus2,GrpGeocode",
    # Options
    "&opt=UsePreferredCity:on",
    # Address components
    "&a1=", utils::URLencode(paste(stats::na.omit(street, unit))),
    "&city=", utils::URLencode(city),
    "&postal=", utils::URLencode(zip),
    "&state=", utils::URLencode(state),
    "&format=JSON"
  )
}
