#' Validate address components
#'
#' This function checks an address component in `df$var` against elements in a reference list, e.g., cities in Kansas. If an exact match isn't found, it uses [agrep()] to find an approximate match or matches. The returned dataframe has one new column, `replacement_text`. Only rows without an exact match are returned. After visually confirming the results and making any needed adjustments to the returned dataframe, use [replace_values()] to substitute the value in `replacement_text` for the original value in `var`.
#'
#' @param df A dataframe of addresses.
#' @param var A variable name in `df` containing the address component to validate.
#' @param type The type of address component to validate. One of `c("city", "zip")`. Defaults to value in `var`.
#' @param max_dist A value passed to `max.distance` in [agrep()] that determines the "maximum distance allowed for a match" to the elements in the reference list.
#'
#' @return A dataframe with a column of possible matches to replace the values in `var`.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @family address processing functions
# @examples
#'
validate_address <- function(df, var, type = var, max_dist = 0.1) {
  var_check(df, var = var)

  # Load `ref` based on `type`
  if (type == "city") {
    ref <- ks_cities
  } else if (type == "zip") {
    ref <- ks_zipcodes
  } else {
    m <- '`type` must be one of c("city", "zip")'
    stop(m, call. = FALSE)
  }

  # Filter values in `var` not found in `ref` (ignoring case)
  df2 <- df %>%
    dplyr::filter(!is.na(.data[[var]])) %>%
    dplyr::filter(!toupper(.data[[var]]) %in% toupper(ref))

  if (nrow(df2) == 0) {
    return(message("No invalid values found"))
  }

  # Find potential match
  f <- function(r, df, source, ref_list, max_dist) {
    matches <- agrep(
      pattern = df[var][r, ],
      x = ref,
      max.distance = max_dist,
      ignore.case = TRUE,
      value = TRUE
    )
    if (purrr::is_empty(matches)) {
      NA
    } else {
      paste(unique(matches), collapse = " | ")
    }
  }

  df2$replacement_text <- sapply(
    X = 1:nrow(df2),
    FUN = f,
    df = df2, source = var,
    ref_list = ref, max_dist = max_dist
  )

  df2 %>%
    dplyr::relocate(replacement_text, .after = tidyselect::all_of(var))
}
