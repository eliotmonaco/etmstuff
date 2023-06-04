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

  # Combine `street` and `unit` into one field
  df$street_unit <- stringr::str_trim(
    paste(
      df[[street]],
      tidyr::replace_na(df$unit, "")
    )
  )

  # Add `pm.id` (row ID) and `pm.uid` (unique address ID)
  df_pm_id <- postmastr::pm_identify(df, var = "street_unit", locale = "us")

  # Deduplicate addresses
  df <- postmastr::pm_prep(df_pm_id, var = "street_unit", type = "street")

  # Parse house numbers
  df <- postmastr::pm_house_parse(df)

  # Parse street directions
  df <- postmastr::pm_streetDir_parse(df, dictionary = postmastr_directions)

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
    new_address = "street_unit_pm",
    keep_parsed = "yes"
  )

  df
}
