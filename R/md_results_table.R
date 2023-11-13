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
        "AS01",        # 1
        "AS01 + AS23", # 2
        "AS01 + AS26", # 3
        "AS02",        # 4
        "AS13",        # 5
        "AE02",        # 6
        "AE06",        # 7
        "AC02",        # 8
        "AC20",        # 9
        "NA"           # 10
      ),
      description = c(
        "valid address",                                 # 1
        "valid address + extraneous street information", # 2
        "valid address + unidentified data",             # 3
        "street only match",                             # 4
        "address updated by LACS",                       # 5
        "unknown street",                                # 6
        "early warning system",                          # 7
        "administrative area change",                    # 8
        "house number change",                           # 9
        "no response from Melissa Data"                  # 10
      ),
      n = c(
        sum(stringr::str_detect(Results, "AS01(?!(.{1,50}AS23)|(.{1,50}AS26))"), na.rm = T), # 1
        sum(stringr::str_detect(Results, "AS01(?=.{1,50}AS23)"), na.rm = T),                 # 2
        sum(stringr::str_detect(Results, "AS01(?=.{1,50}AS26)"), na.rm = T),                 # 3
        sum(stringr::str_detect(Results, "AS02"), na.rm = T),                                # 4
        sum(stringr::str_detect(Results, "AS13"), na.rm = T),                                # 5
        sum(stringr::str_detect(Results, "AE02"), na.rm = T),                                # 6
        sum(stringr::str_detect(Results, "AE06"), na.rm = T),                                # 7
        sum(stringr::str_detect(Results, "AC02"), na.rm = T),                                # 8
        sum(stringr::str_detect(Results, "AC20"), na.rm = T),                                # 9
        sum(is.na(Results))                                                                  # 10
      )
    ) %>%
    dplyr::mutate(pct = etmstuff::pct(n, nrow(df))) %>%
    dplyr::rename(
      "MD result code" = code,
      "addresses (n)" = n,
      "addresses (pct)" = pct
    )
}
