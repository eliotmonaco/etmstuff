#' Clean dirty addresses
#'
#' @description
#' This function cleans addresses by using regular expressions to match common error patterns. Each `type` option corresponds to a pattern in `etmstuff::address_regex` that targets certain text for removal and provides a suggestion for replacement. The function returns a dataframe with two new variables, `removed_text` and `replacement_text`, adjacent to the variable targeted for cleaning. Only rows containing values that match the error pattern are returned. The user should examine the returned dataframe to confirm, reject, or modify the suggested replacements. [replace_values()] can be used to apply the corrections to the original dataframe.
#'
#' @inheritSection pull_addresses Address validation workflow
#' @inheritSection pull_addresses Melissa Data
#'
#' @param df A dataframe of addresses.
#' ```{r echo=FALSE}
#' all_types <- address_regex$type
#' all_types <- paste0('"', paste(all_types, collapse = '", "'), '"')
#' all_types <- paste0("c(", all_types, ")")
#' ```
#' @param type The type of cleaning to perform. One of ``r all_types``.
#' @param var A variable in `df` containing the targeted address component. Defaults to `street`.
#' @param row_id A unique row identifier variable in `df`. Defaults to `address_id`.
#'
#' @return A dataframe containing values that match the selected error pattern and the suggested replacements.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @family address processing functions
# @examples
#'
clean_address <- function(df, type, var = "street", row_id = "address_id") {
  var_check(df, var = c(var, row_id))

  # Stop if `row_id` values are not unique
  if (any(duplicated(df[row_id]))) stop("`row_id` is not a unique row identifier")

  all_types <- address_regex$type

  # Load `ref` based on the selected `type`
  if (type %in% all_types) {
    ref <- address_regex[which(address_regex$type == type),]
  } else {
    m <- paste0(
      "`type` must be one of c(",
      paste0('"', paste(all_types, collapse = '", "'), '"')
      , ")"
    )
    stop(m)
  }

  # Filter out rows where the targeted variable contains NA
  df <- df %>%
    dplyr::filter(!is.na(.data[[var]]))

  # Pull pattern matches
  extracted <- stringr::str_match_all(
    string = df[[var]],
    pattern = stringr::regex(ref$search_pattern, ignore_case = TRUE)
  )

  # If all list elements have length of 0, no matches have been found
  if (purrr::is_empty(purrr::compact(extracted))) {
    return(message("No matching values found"))
  }

  # If the number of capturing groups from `address_regex$search_pattern` is specified, get those matches only, as determined by `address_regex$n_cap_gps`, otherwise get full match only
  if (!is.na(ref$n_cap_gps)) {
    n <- ref$n_cap_gps + 1
    extracted <- lapply(extracted, "[", , 2:n)
  } else {
    extracted <- lapply(extracted, "[", , 1)
  }

  # Convert list of matches to a vector
  extracted <- lapply(extracted, function (x) replace(x, list = which(is.na(x)), values = ""))
  extracted <- lapply(extracted, paste, collapse = " ")
  extracted <- unlist(extracted)

  if (purrr::is_empty(which(extracted != ""))) {
    return(message("No matching values found"))
  }

  df$removed_text <- stringr::str_squish(extracted)

  # Set find and replace patterns
  if (!is.na(ref$replace_pattern)) {
    p <- stats::setNames(ref$replacement, ref$replace_pattern)
  } else {
    p <- stats::setNames(ref$replacement, ref$search_pattern)
  }

  # Create replacement suggestion
  replacement_text <- stringr::str_replace_all(
    string = df[[var]],
    pattern = stringr::regex(p, ignore_case = TRUE)
  )

  # Clean outer punctuation and extra spaces from replacement text
  df$replacement_text <- stringr::str_squish(
    stringr::str_remove_all(
      string = replacement_text,
      pattern = "(^[[:punct:]\\s]*)|([[:punct:]\\s]*$)"
    )
  )

  df$replacement_text[which(df$replacement_text == "")] <- NA

  df %>%
    dplyr::filter(.data$removed_text != "") %>%
    dplyr::relocate("replacement_text", "removed_text", .after = tidyselect::all_of(var))
}
