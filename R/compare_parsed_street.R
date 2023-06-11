#' Compare the street address pre- and post-parsing
#'
#' @inheritSection pull_addresses Address validation workflow
#' @inheritSection pull_addresses Melissa Data
#'
#' @param df A dataframe of addresses.
#' @param street1 The pre-parsing street variable.
#' @param street2 The post-parsing street variable.
#'
#' @return A dataframe containing rows in which `street1 != street2`.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @family address processing functions
# @examples
#'
compare_parsed_street <- function(df, street1 = "street_unit", street2 = "street_unit_final") {
  var_check(df, var = c(street1, street2))

  p1 <- stats::setNames(
    directions_cardinal$replacement,
    paste0("(?<=^\\d{1,7}\\s)", directions_cardinal$pattern, "(?=\\s)")
  )

  p2 <- stats::setNames(
    street_suffix$replacement,
    paste0("(?<=\\s)", street_suffix$pattern, "$")
  )

  df$temp <- stringr::str_replace_all(
    df[[street1]],
    stringr::regex(p1, ignore_case = TRUE)
  )

  df$temp <- stringr::str_replace_all(
    df$temp,
    stringr::regex(p2, ignore_case = TRUE)
  )

  df %>%
    dplyr::filter(stringr::str_to_upper(temp) != stringr::str_to_upper(.data[[street2]])) %>%
    dplyr::select(-temp)
}
