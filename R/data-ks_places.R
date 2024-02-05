#' Kansas cities and CDPs
#'
#' @name ks_places
#'
#' @description A list of Kansas incorporated and census designated places (CDPs) and their corresponding 2020 census codes. From the US Census Bureau website: "Contains a single record for every place in the state/nation sorted by "STATEFP" and then "PLACEFP".  When the place extends into multiple counties, those counties are listed in the “COUNTIES” field."
#'
#' @usage ks_places
#'
#' @format A dataframe with 9 columns and 740 rows.
#' \describe{
#'   \item{STATE}{State postal abbreviation}
#'   \item{STATEFP}{State FIPS code}
#'   \item{PLACEFP}{Place FIPS code}
#'   \item{PLACENS}{Place NS code}
#'   \item{PLACENAME}{Place name and legal/statistical area description}
#'   \item{TYPE}{Place type}
#'   \item{CLASSFP}{FIPS class code}
#'   \item{FUNCSTAT}{Legal functional status}
#'   \item{COUNTIES}{Name of county or counties in which this place is located}
#' }
#'
#' @source Downloaded from the US Census Bureau website (https://www.census.gov/library/reference/code-lists/ansi.html#place).
#'
#' @keywords datasets
"ks_places"
