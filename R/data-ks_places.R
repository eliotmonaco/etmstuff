#' Kansas cities and CDPs
#'
#' @name ks_places
#'
#' @description A list of Kansas incorporated and census designated places (CDPs) and their corresponding 2020 census codes. There is one row per place. If a place overlaps with multiple counties, all are listed under `COUNTIES`. Table downloaded from Census.gov.
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
#' @source https://www.census.gov/library/reference/code-lists/ansi.html#place
#'
#' @keywords datasets
"ks_places"
