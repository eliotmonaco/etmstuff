#' Parse street addresses using `postmastr` workflow
#'
#' @description
#' Parse street addresses using functions from the `postmastr` package. `df` requires an `address_id` variable before using `parse_street_addresses()`.
#'
#' @inheritSection pull_addresses Address processing workflow
#' @inheritSection pull_addresses Address validation and geocoding
#'
#' @param df A dataframe of addresses.
#' @param ref This will be changed...
#'
#' @return TBD...
#' @export
#'
#' @family address processing functions
# @examples
#'
parse_street_addresses <- function(df, ref) {
  var_check(df, var = c("address_id", "street", "unit", "city", "state", "zip", "county"))

  # Combine `street` and `unit` into one field
  df$street_unit <- stringr::str_trim(
    paste(
      df$street,
      tidyr::replace_na(df$unit, "")
    )
  )

  df_pm_id <- postmastr::pm_identify(df, var = "street_unit", locale = "us")

  df <- postmastr::pm_prep(df_pm_id, var = "street_unit", type = "street")

  df <- postmastr::pm_house_parse(df)

  df <- postmastr::pm_streetDir_parse(df, dictionary = ref)

  df <- postmastr::pm_streetSuf_parse(df)

  df <- postmastr::pm_street_parse(df, ordinal = TRUE, drop = TRUE)

  df <- postmastr::pm_replace(df, source = df_pm_id)

  df <- postmastr::pm_rebuild(df, output = "short", keep_parsed = "no")

  df$street_geo <- df_gc_full$pm.address # What is `df_gc_full`??????????????

  df
}
