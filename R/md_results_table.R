#' Tabulate result codes in the Melissa Data response
#'
#' @description
#' This function creates a table with counts of the result codes present in `df$Results`.
#'
#' @inheritSection pull_addresses Address validation workflow
#' @inheritSection pull_addresses Melissa Data
#'
#' @param df A dataframe of Melissa Data responses returned by [send_md_request()].
#' @param var The variable containing the result codes. The default is `"Results"`.
#'
#' @return A dataframe.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @family address processing functions
# @examples
#'
md_results_table <- function(df, var = "Results") {
  if (!is.data.frame(df)) stop("`df` must be a dataframe")
  if (length(var) != 1) stop("`var` must have length of 1")

  var_check(df, var = var)

  # Unique MD result codes in dataframe
  unq_codes <- sort(unique(unlist(strsplit(df$Results, ","))))

  # Table of unique result codes
  df_md_codes <- purrr::list_rbind(melissa_data_codes) %>%
    dplyr::filter(.data$code %in% unq_codes)

  # Add n occurrences
  df_md_codes$n <- sapply(
    df_md_codes$code,
    FUN = function(x) sum(stringr::str_detect(df$Results, x)),
    simplify = TRUE
  )

  # Add pct occurrences
  df_md_codes$pct <- sapply(
    df_md_codes$n,
    FUN = function(x) pct(x, nrow(df)),
    simplify = TRUE
  )

  colnames(df_md_codes) <- c("Result code", "Short description", "Long description", "n", "%")

  df_md_codes
}
