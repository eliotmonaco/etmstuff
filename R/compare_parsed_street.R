#' Compare the street address pre- and post-parsing
#'
#' @inheritSection pull_addresses Address validation workflow
#' @inheritSection pull_addresses Melissa Data
#'
#' @param df A dataframe of addresses.
#' @param pre The pre-parsing street variable. Defaults to `street_cleaned`.
#' @param post The post-parsing street variable. Defaults to `street_parsed`.
#'
#' @return A dataframe containing rows in which `pre != post`.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @family address processing functions
# @examples
#'
compare_parsed_street <- function(df, pre = "street_cleaned", post = "street_parsed") {
  var_check(df, var = c(pre, post))

  p1 <- stats::setNames(
    directions_cardinal$replacement,
    paste0("(?<=^\\d{1,7}\\s)", directions_cardinal$pattern, "(?=\\s)")
  )

  p2 <- stats::setNames(
    pm_street_suffix$replacement,
    paste0("(?<=\\s)", pm_street_suffix$pattern, "$")
  )

  df$temp <- stringr::str_replace_all(
    df[[pre]],
    stringr::regex(p1, ignore_case = TRUE)
  )

  df$temp <- stringr::str_replace_all(
    df$temp,
    stringr::regex(p2, ignore_case = TRUE)
  )

  df %>%
    dplyr::filter(toupper(temp) != toupper(.data[[post]]) |
                    (!is.na(temp) & is.na(.data[[post]]))) %>%
    dplyr::select(-temp) %>%
    dplyr::relocate(tidyselect::all_of(post), .after = tidyselect::all_of(pre))
}
