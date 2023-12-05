#' Count result codes in the Melissa Data response
#'
#' @description
#' This function creates a table with counts of the result codes that are most relevant to the lead address validation and geocoding process. The following result codes or result code combinations are included: AS01, AS01 + AS23, AS01 + AS26, AS02, AS13, AE02, AE06, , , , , , ,
#'
#' @inheritSection pull_addresses Address validation workflow
#' @inheritSection pull_addresses Melissa Data
#'
#' @param df A dataframe of Melissa Data responses returned by [md_batch_request()].
#' @param var The variable containing result codes. The default is `"Results"`.
#'
#' @return A dataframe tallying addresses by certain Melissa Data result codes.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
# @examples
#'
md_results_table <- function(df, var = "Results") {
  var_check(df, var = var)

  df %>%
    dplyr::reframe(
      code = md_summarize$code,
      description = md_summarize$description,
      n = c(
        sum(stringr::str_detect(.data$Results, md_summarize$pattern[1]), na.rm = T),
        sum(stringr::str_detect(.data$Results, md_summarize$pattern[2]), na.rm = T),
        sum(stringr::str_detect(.data$Results, md_summarize$pattern[3]), na.rm = T),
        sum(stringr::str_detect(.data$Results, md_summarize$pattern[4]), na.rm = T),
        sum(stringr::str_detect(.data$Results, md_summarize$pattern[5]), na.rm = T),
        sum(stringr::str_detect(.data$Results, md_summarize$pattern[6]), na.rm = T),
        sum(stringr::str_detect(.data$Results, md_summarize$pattern[7]), na.rm = T),
        sum(stringr::str_detect(.data$Results, md_summarize$pattern[8]), na.rm = T),
        sum(stringr::str_detect(.data$Results, md_summarize$pattern[9]), na.rm = T),
        sum(stringr::str_detect(.data$Results, md_summarize$pattern[10]), na.rm = T),
        sum(is.na(.data$Results))
      )
    ) %>%
    dplyr::mutate(pct = etmstuff::pct(.data$n, nrow(df))) %>%
    dplyr::rename(
      "MD result code" = "code",
      "Description" = "description",
      "Addresses (n)" = "n",
      "Addresses (pct)" = "pct"
    )
}
