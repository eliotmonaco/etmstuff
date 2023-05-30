#' Clean addresses in a dataframe
#'
#' This function applies a pattern to clean street address strings in `df$var` based on the `type` chosen. The returned dataframe has two new columns, `removed_text` and `replacement_text`. Only cleaned rows are returned. After visually confirming the results and making any needed adjustments to the returned dataframe, use [replace_values()] to substitute the value in `replacement_text` for the original value in `var`.
#'
#' @param df A dataframe of addresses.
#' @param var A variable name in `df` containing street addresses.
#' @param id_var A variable name in `df` that serves as a unique row identifier.
#' @param type The type of cleaning to perform.
#'
#' @return A dataframe containing only the rows that have been cleaned.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @family address processing functions
# @examples
#'
clean_values <- function(df, var, id_var, type) {
  var_check(df, var = c(var, id_var))

  if (type == "pobox") {
    ref <- regex_pobox
  } else if (type == "ordinal_dir") {
    ref <- directions_ordinal
  } else if (type %in% rownames(regex_various)) {
    ref <- regex_various[which(rownames(regex_various) == type), ]
  } else {
    types <- c("pobox", rownames(regex_various))
    m <- paste0("`type` must be one of c(", paste0('"', paste(types, collapse = '", "'), '"'), ")")
    stop(m, call. = FALSE)
  }

  df2 <- df %>%
    dplyr::select(dplyr::all_of(c(id_var, var)))

  p <- paste(ref$pattern, collapse = "|")

  extracted <- stringr::str_extract_all(
    string = df2[[var]],
    pattern = stringr::regex(p, ignore_case = TRUE),
    simplify = TRUE
  )

  if (purrr::is_empty(extracted)) {
    return(message("No matching values found"))
  }

  p <- stats::setNames(ref$replacement, ref$pattern)

  df2[3] <- tidyr::unite(as.data.frame(extracted), "removed_text", sep = " ")
  df2$removed_text <- stringr::str_squish(df2$removed_text)

  z <- "[[:punct:]\\s]*"
  punct <- paste0("(^", z, ")|(", z, "$)")

  df2$replacement_text <- stringr::str_squish(
    stringr::str_remove_all(
      stringr::str_replace_all(
        df2[[var]],
        stringr::regex(p, ignore_case = TRUE)
      ), punct
    )
  )

  df2 %>%
    dplyr::filter(removed_text != "") %>%
    dplyr::left_join(
      df %>%
        dplyr::select(-dplyr::all_of(var)),
      by = id_var
    ) %>%
    dplyr::relocate(replacement_text, .before = removed_text)
}
