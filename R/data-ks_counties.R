#' Kansas counties
#'
#' @name ks_counties
#'
#' @description A list of Kansas counties and their corresponding 2020 census codes. Table downloaded from Census.gov.
#'
#' @usage ks_counties
#'
#' @format A dataframe with 7 columns and 105 rows.
#' \describe{
#'   \item{STATE}{State postal abbreviation}
#'   \item{STATEFP}{State FIPS code}
#'   \item{COUNTYFP}{County FIPS code}
#'   \item{COUNTYNS}{County NS code}
#'   \item{COUNTYNAME}{County name and legal/statistical area description}
#'   \item{CLASSFP}{FIPS class code}
#'   \item{FUNCSTAT}{Functional status}
#' }
#'
#' @source https://www.census.gov/library/reference/code-lists/ansi.html#cou
#'
#' @keywords datasets
"ks_counties"
