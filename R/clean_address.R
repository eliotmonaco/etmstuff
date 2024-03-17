#' Clean dirty addresses
#'
#' @description
#' This function cleans address strings by using regular expressions to match common error patterns.
#'
#' @details
#' Each `type` option corresponds to a row in `address_regex` with patterns to clean a specific type of error found in address strings. The function returns a dataframe with two new variables, `REMOVED_TEXT` (the part of the string matching the error pattern) and `REPLACEMENT_TEXT` (a replacement suggestion for the entire string). These new columns are placed to the right of `var`. Only rows in which a pattern match is found will be returned. The user should examine the returned dataframe to confirm, reject, or modify the suggested replacements. [replace_values()] can then be used to replace the values with errors in the original dataframe.
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
#' @param var The variable to target for cleaning. Defaults to `street`.
#' @param row_id A unique row identifier variable in `df`. Defaults to `address_id`.
#'
#' @return A dataframe consisting of rows where matches were found in `var`.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @family address processing functions
# @examples
#'
clean_address <- function(df, type, var = "street", row_id = "address_id") {
  if (!is.data.frame(df)) stop("`df` must be a dataframe")
  if (length(var) != 1 | length(row_id) != 1) stop("`var` and `row_id` must have length of 1")

  var_check(df, var = c(var, row_id))

  # Stop if `row_id` values are not unique
  if (any(duplicated(df[[row_id]]))) stop("`row_id` is not a unique row identifier")

  # Load `ref` based on the selected `type`
  all_types <- address_regex$type
  if (type %in% all_types) {
    ref <- address_regex[which(address_regex$type == type),]
  } else {
    stop(paste0(
      "`type` must be one of c(",
      paste0('"', paste(all_types, collapse = '", "'), '"'), ")"
    ))
  }

  # Filter out rows where the targeted variable contains NA
  df <- df %>%
    dplyr::filter(!is.na(.data[[var]]))

  # Extract matches to `ref$search_pattern`
  df$REMOVED_TEXT <- extract_matches(df[[var]], ref$search_pattern, ref$n_cap_gps)

  # Exit if no matches were found
  if (purrr::is_empty(which(df$REMOVED_TEXT != ""))) {
    return(message("No matching values found"))
  }

  # Set the search and replace patterns
  if (!is.na(ref$replace_pattern)) {
    p <- stats::setNames(ref$replacement, ref$replace_pattern)
  } else {
    p <- stats::setNames(ref$replacement, ref$search_pattern)
  }

  # Create replacement suggestions
  df$REPLACEMENT_TEXT <- create_replacement(df[[var]], p)

  df %>%
    dplyr::filter(.data$REMOVED_TEXT != "") %>%
    dplyr::relocate("REPLACEMENT_TEXT", "REMOVED_TEXT", .after = tidyselect::all_of(var))
}

extract_matches <- function(string, pattern, n_capturing_groups) {
  matches <- stringr::str_match_all(string, stringr::regex(pattern, ignore_case = TRUE))

  # If all list elements have length of 0, no matches have been found
  if (purrr::is_empty(purrr::compact(matches))) {
    return("")
  }

  # If the number of capturing groups from `ref$search_pattern` is specified, get those matches only as determined by `ref$n_cap_gps`, otherwise get full match only
  if (!is.na(n_capturing_groups)) {
    n <- n_capturing_groups + 1
    matches <- lapply(matches, "[", , 2:n)
  } else {
    matches <- lapply(matches, "[", , 1)
  }

  # Convert list of matches to a vector
  matches <- lapply(matches, function(x) replace(x, list = which(is.na(x)), values = ""))
  matches <- lapply(matches, paste, collapse = " ")
  stringr::str_squish(unlist(matches))
}

create_replacement <- function(string, pattern) {
  text <- stringr::str_replace_all(string, stringr::regex(pattern, ignore_case = TRUE))

  # Clean outer punctuation and extra spaces from replacement text
  text <- stringr::str_squish(stringr::str_remove_all(
    text, "(^[[:punct:]\\s]*)|([[:punct:]\\s]*$)"
  ))

  text[text == ""] <- NA

  text
}
