#' Kansas cities and CDPs by county
#'
#' @name ks_place_by_county
#'
#' @description A list of Kansas incorporated and census designated places (CDPs) and their corresponding 2020 census codes. From the US Census Bureau website: "Contains at least one record for every place in the state/nation sorted by "STATEFP", then "COUNTYFP", and then "PLACEFP".  When the place extends into multiple counties, the table includes a separate record for each county."
#'
#' @usage ks_place_by_county
#'
#' @format A dataframe with 10 columns and 764 rows.
#' \describe{
#'   \item{STATE}{State postal abbreviation}
#'   \item{STATEFP}{State FIPS code}
#'   \item{COUNTYFP}{County FIPS code}
#'   \item{COUNTYNAME}{County name and legal/statistical area description}
#'   \item{PLACEFP}{Place FIPS code}
#'   \item{PLACENS}{Place NS code}
#'   \item{PLACENAME}{Place name and legal/statistical area description}
#'   \item{TYPE}{Place type}
#'   \item{CLASSFP}{FIPS class code}
#'   \item{FUNCSTAT}{Legal functional status}
#' }
#'
#' @source Downloaded from the US Census Bureau website (https://www.census.gov/library/reference/code-lists/ansi.html#place).
#'
#' @keywords datasets
"ks_place_by_county"
