#' Clean dirty addresses
#'
#' This function applies a pattern to clean street address strings in `df$var` based on the `type` chosen. The returned dataframe has two new columns, `removed_text` and `replacement_text`. Only cleaned rows are returned. After visually confirming the results and making any needed adjustments to the returned dataframe, use [replace_values()] to substitute the value in `replacement_text` for the original value in `var`.
#'
#' @param df A dataframe of addresses.
#' ```{r echo=FALSE}
#' types <- rownames(address_regex)
#' types <- paste0('"', paste(types, collapse = '", "'), '"')
#' types <- paste0("c(", types, ")")
#' ```
#' @param type The type of cleaning to perform. One of ``r types``.
#' @param var A variable in `df` containing the targeted address component.
#' @param row_id A unique row identifier variable in `df`. Defaults to `address_id`.
#'
#' @return A dataframe containing only the rows that have been cleaned.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @family address processing functions
# @examples
#'
clean_address <- function(df, type, var = "street", row_id = "address_id") {
  var_check(df, var = c(var, row_id))

  if (any(duplicated(df[row_id]))) stop("`row_id` is not a unique row identifier")

  cols_src <- colnames(df)

  # Load `ref` based on `type`
  if (type %in% rownames(address_regex)) {
    ref <- address_regex[which(rownames(address_regex) == type), ]
  } else {
    types <- rownames(address_regex)
    m <- paste0("`type` must be one of c(", paste0('"', paste(types, collapse = '", "'), '"'), ")")
    stop(m, call. = FALSE)
  }

  df2 <- df %>%
    dplyr::select(tidyselect::all_of(c(row_id, var))) %>%
    dplyr::filter(!is.na(.data[[var]]))

  extracted <- stringr::str_match_all(
    string = df2[[var]],
    pattern = stringr::regex(p, ignore_case = TRUE)
  )
  extracted <- lapply(extracted, "[", , 2)
  extracted <- lapply(extracted, function (x) replace(x, list = which(is.na(x)), values = ""))
  extracted <- lapply(extracted, paste, collapse = " ")
  extracted <- unlist(extracted)

  if (purrr::is_empty(which(extracted != ""))) {
    return(message("No matching values found"))
  }

  df2$removed_text <- stringr::str_squish(extracted)

  if (!is.na(ref$pattern_replace)) {
    p <- stats::setNames(ref$replacement, ref$pattern_replace)
  } else {
    p <- stats::setNames(ref$replacement, ref$pattern)
  }

  replacement_text <- stringr::str_replace_all(
    string = df2[[var]],
    pattern = stringr::regex(p, ignore_case = TRUE)
  )

  df2$replacement_text <- stringr::str_squish(
    stringr::str_remove_all(
      string = replacement_text,
      pattern = "(^[[:punct:]\\s]*)|([[:punct:]\\s]*$)"
    )
  )

  df2$replacement_text[which(df2$replacement_text == "")] <- NA

  # Column order of returned dataframe
  n <- which(cols_src == var)
  cols1 <- cols_src[1:n]
  cols2 <- cols_src[(n + 1):length(cols_src)]

  df2 %>%
    dplyr::filter(removed_text != "") %>%
    dplyr::left_join(
      df %>%
        dplyr::select(-tidyselect::all_of(var)),
      by = row_id
    ) %>%
    dplyr::select(
      tidyselect::all_of(cols1),
      replacement_text, removed_text,
      tidyselect::all_of(cols2)
    )
}
