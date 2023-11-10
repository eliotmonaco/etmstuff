#' Tally addresses sent to Melissa Data by result code
#'
#' This function takes a dataframe of Melissa Data responses returned by [md_batch_request()] and returns a table tallying the responses by result codes that are relevant for the lead address validation and geocoding process.
#'
#' @param df A dataframe of Melissa Data responses.
#' @param var The variable containing result codes. The default is `Results`.
#'
#' @return A dataframe tallying addresses by certain Melissa Data result codes.
#' @export
#'
#' @importFrom magrittr %>%
#'
# @examples
#'
md_results_table <- function(df, var = "Results") {
  var_check(df, var = var)

  df %>%
    dplyr::reframe(
      code = c(
        "AS01",
        "AS01 + AS23",
        "AS02",
        "AS13",
        "AE02",
        "AE06",
        "AC02",
        "AC20",
        "NA"
      ),
      description = c(
        "valid address",
        "valid address + extraneous street information",
        "street only match",
        "address updated by LACS",
        "unknown street",
        "early warning system",
        "administrative area change",
        "house number change",
        "no response from Melissa Data"
      ),
      n = c(
        sum(stringr::str_detect(Results, "AS01(?!.{1,50}AS23)"), na.rm = T),
        sum(stringr::str_detect(Results, "AS01(?=.{1,50}AS23)"), na.rm = T),
        sum(stringr::str_detect(Results, "AS02"), na.rm = T),
        sum(stringr::str_detect(Results, "AS13"), na.rm = T),
        sum(stringr::str_detect(Results, "AE02"), na.rm = T),
        sum(stringr::str_detect(Results, "AE06"), na.rm = T),
        sum(stringr::str_detect(Results, "AC02"), na.rm = T),
        sum(stringr::str_detect(Results, "AC20"), na.rm = T),
        sum(is.na(Results))
      )
    ) %>%
    dplyr::mutate(pct = etmstuff::pct(n, nrow(df))) %>%
    dplyr::rename(
      "MD result code" = code,
      "addresses (n)" = n,
      "addresses (pct)" = pct
    )
}
