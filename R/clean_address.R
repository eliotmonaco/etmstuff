#' Clean dirty addresses
#'
#' This function cleans addresses by using regular expressions to match common error patterns. Each `type` option corresponds to a pattern in `etmstuff::address_regex` that targets certain text for removal and provides a suggestion for replacement. The function returns a dataframe with two new variables, `removed_text` and `replacement_text`, adjacent to the variable targeted for cleaning. Only rows containing values that match the error pattern are returned. The user should examine the returned dataframe to confirm, reject, or modify the suggested replacements. [replace_values()] can be used to apply the corrections to the original dataframe.
#'
#' @param df A dataframe of addresses.
#' ```{r echo=FALSE}
#' types <- rownames(etmstuff::address_regex)
#' types <- paste0('"', paste(types, collapse = '", "'), '"')
#' types <- paste0("c(", types, ")")
#' ```
#' @param type The type of cleaning to perform. One of ``r types``.
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

  # Load `ref` based on the selected `type`
  if (type %in% rownames(etmstuff::address_regex)) {
    ref <- etmstuff::address_regex[which(rownames(etmstuff::address_regex) == type), ]
  } else {
    types <- rownames(etmstuff::address_regex)
    m <- paste0("`type` must be one of c(", paste0('"', paste(types, collapse = '", "'), '"'), ")")
    stop(m)
  }

  # Filter out rows where the targeted variable contains NA
  df <- df %>%
    dplyr::filter(!is.na(.data[[var]]))

  # Pull pattern matches
  extracted <- stringr::str_match_all(
    string = df[[var]],
    pattern = stringr::regex(ref$pattern, ignore_case = TRUE)
  )

  # If all list elements have length of 0, no matches have been found
  if (purrr::is_empty(purrr::compact(extracted))) {
    return(message("No matching values found"))
  }

  # If the number of capturing groups from `address_regex$pattern` is specified, get those matches only, otherwise get full match only
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
  if (!is.na(ref$pattern_replace)) {
    p <- stats::setNames(ref$replacement, ref$pattern_replace)
  } else {
    p <- stats::setNames(ref$replacement, ref$pattern)
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
