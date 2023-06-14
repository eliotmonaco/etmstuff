#' Parse street addresses using `postmastr` workflow
#'
#' @description
#' Parse street addresses using functions from the `postmastr` package. `df` requires an `address_id` variable before using `parse_street_addresses()`. The output contains a new variable, `street_unit`, which combines both `street` and `unit`.
#'
#' `postmastr` is available in a development version from GitHub.
#'
#' ```
#' remotes::install_github("slu-openGIS/postmastr")
#' ```
#'
#' @inheritSection pull_addresses Address validation workflow
#' @inheritSection pull_addresses Melissa Data
#'
#' @param df A dataframe of addresses.
#' @param street The street address variable name.
#'
#' @return A dataframe.
#' @export
#'
#' @family address processing functions
# @examples
#'
parse_street_addresses <- function(df, street = "street") {
  var_check(df, var = street)

  # Add `pm.id` (row ID) and `pm.uid` (unique address ID)
  df_pm_id <- postmastr::pm_identify(
    df,
    var = "street",
    locale = "us"
  )

  # Deduplicate addresses
  df <- df_pm_id %>%
    dplyr::select(pm.uid, pm.address = street) %>%
    dplyr::distinct(pm.uid, .keep_all = TRUE)

  # Parse house numbers
  df <- postmastr::pm_house_parse(df)

  # Extract fractional house numbers
  p <- "^\\d/\\d"
  df$frac <- stringr::str_extract(df$pm.address, p)
  df <- tidyr::unite(
    df,
    col = "pm.house",
    sep = " ",
    tidyselect::all_of(c("pm.house", "frac")),
    na.rm = TRUE
  )
  df$pm.address <- stringr::str_squish(stringr::str_remove(df$pm.address, p))

  # Parse street directions
  df <- postmastr::pm_streetDir_parse(df, dictionary = pm_direction_dictionary)

  # Parse street suffixes
  df <- postmastr::pm_streetSuf_parse(df)

  # Parse street names
  df <- postmastr::pm_street_parse(df, ordinal = TRUE, drop = TRUE)

  # Add parsed columns back to `df`
  df <- postmastr::pm_replace(df, source = df_pm_id)

  # Recombine parsed elements into a single string
  df <- postmastr::pm_rebuild(
    df,
    output = "short",
    new_address = "street_parsed",
    keep_parsed = "yes"
  )

  df
}
