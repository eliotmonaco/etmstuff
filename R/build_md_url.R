#' Build a Melissa Data Personator URL request
#'
#' @description
#' Build a URL to query the Melissa Data Personator Consumer Web Service API. The URL is a request for a response from the Personator service in a JSON format.
#'
#' @inheritSection pull_addresses Address processing workflow
#' @inheritSection pull_addresses Address validation and geocoding
#'
#' @param df A dataframe of addresses including the `street_geo` variable.
#' @param row_id A unique row identifier. Should be `address_id`.
#'
#' @return A vector of URLs.
#' @export
#'
#' @family address processing functions
# @examples
#'
build_md_url <- function(df, row_id) {
  var_check(df, var = c(row_id, "street_geo", "city", "state", "zip"))

  f <- function(r) {
    paste0(
      "https://personator.melissadata.net/v3/WEB/ContactVerify/doContactVerify",
      "?t=", r[row_id],
      "&id=HX1erDnVvumudTEscOCub-**",
      "&act=Check",
      "&cols=Suite,GrpParsedAddress,GrpAddressDetails,GrpCensus,GrpCensus2,GrpGeocode",
      "&opt=UsePreferredCity:on",
      "&a1=", utils::URLencode(r["street_geo"]),
      "&city=", utils::URLencode(r["city"]),
      "&postal=", r["zip"],
      "&state=", r["state"],
      "&format=JSON"
    )
  }

  apply(df, 1, f)
}
