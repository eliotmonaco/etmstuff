#' Build a Melissa Data Personator request URL
#'
#' @description
#' Build a URL to query the Melissa Data Personator Consumer Web Service API. The URL is a request for a response from the Personator service in JSON format.
#'
#' @inheritSection pull_addresses Address validation workflow
#' @inheritSection pull_addresses Melissa Data
#'
#' @param df A dataframe of cleaned addresses.
#' @param row_id A unique row identifier variable name.
#' @param street The name of the variable containing the street address. Unit information should be included if the `unit` argument is left `NULL`.
#' @param unit The name of the variable containing the unit component of the street address (if the unit is not already included in `street`).
#' @param city The name of the variable containing the city.
#' @param state The name of the variable containing the state.
#' @param zip The name of the variable containing the zip code.
#'
#' @return A vector of URLs.
#' @export
#'
#' @family address processing functions
# @examples
#'
build_md_url <- function(df,
                         row_id = "md_id",
                         street = "street",
                         unit = NULL,
                         city = "city",
                         state = "state",
                         zip = "zip") {
  var_check(df, var = c(row_id, street, unit, city, state, zip))

  # If `unit` is provided, unite `street` and `unit`
  if (!is.null(unit)) {
    df <- tidyr::unite(
      df,
      col = "street",
      tidyselect::all_of(c(street, unit)),
      sep = " ",
      remove = FALSE,
      na.rm = TRUE
    )
  }

  f <- function(r) {
    paste0(
      "https://personator.melissadata.net/v3/WEB/ContactVerify/doContactVerify",
      "?t=", r[row_id],
      "&id=HX1erDnVvumudTEscOCub-**",
      "&act=Check",
      "&cols=Suite,GrpParsedAddress,GrpAddressDetails,GrpCensus,GrpCensus2,GrpGeocode",
      "&opt=UsePreferredCity:on",
      "&a1=", utils::URLencode(r[street]),
      "&city=", utils::URLencode(r[city]),
      "&postal=", r[zip],
      "&state=", r[state],
      "&format=JSON"
    )
  }

  apply(df, 1, f)
}
