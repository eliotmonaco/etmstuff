#' Clean addresses in a dataframe
#'
#' This function applies a pattern to clean street address strings in `df$var` based on the `type` chosen. The returned dataframe has two new columns, `removed_text` and `replacement_text`. Only cleaned rows are returned. After visually confirming the results and making any needed adjustments to the returned dataframe, use [replace_values()] to substitute the value in `replacement_text` for the original value in `var`.
#'
#' @param df A dataframe of addresses.
#' @param var A variable name in `df` containing street addresses.
#' ```{r echo=FALSE}
#' types <- sort(c("pobox", "ordinal_dir", rownames(regex_various)))
#' types <- paste0('"', paste(types, collapse = '", "'), '"')
#' types <- paste0("c(", types, ")")
#' ```
#' @param type The type of cleaning to perform. One of ``r types``.
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
clean_street_address <- function(df, var = "street", type, row_id = "address_id") {
  var_check(df, var = c(var, row_id))

  col_order_src <- colnames(df)

  # Load `ref` based on `type`
  if (type == "pobox") {
    ref <- regex_pobox
  } else if (type == "ordinal_dir") {
    ref <- directions_ordinal
  } else if (type %in% rownames(regex_various)) {
    ref <- regex_various[which(rownames(regex_various) == type), ]
  } else {
    types <- sort(c("pobox", "ordinal_dir", rownames(regex_various)))
    m <- paste0("`type` must be one of c(", paste0('"', paste(types, collapse = '", "'), '"'), ")")
    stop(m, call. = FALSE)
  }

  df2 <- df %>%
    dplyr::select(tidyselect::all_of(c(row_id, var))) %>%
    dplyr::filter(!is.na(.data[[var]]))

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

  df2[3] <- tidyr::unite(
    as.data.frame(extracted),
    col = "removed_text",
    sep = " "
  )

  df2$removed_text <- stringr::str_squish(df2$removed_text)

  z <- "[[:punct:]\\s]*"
  punct <- paste0("(^", z, ")|(", z, "$)")

  df2$replacement_text <- stringr::str_squish(
    stringr::str_remove_all(
      stringr::str_replace_all(
        df2[[var]],
        stringr::regex(p, ignore_case = TRUE)
      ),
      punct
    )
  )

  df2$replacement_text[which(df2$replacement_text == "")] <- NA

  # Column order of returned dataframe
  n <- which(col_order_src == var)
  cols1 <- col_order_src[1:n]
  cols2 <- col_order_src[(n + 1):length(col_order_src)]

  df2 %>%
    dplyr::filter(removed_text != "") %>%
    dplyr::left_join(
      df %>%
        dplyr::select(-tidyselect::all_of(var)),
      by = row_id
    ) %>%
    # dplyr::relocate(replacement_text, .before = removed_text) %>%
    dplyr::select(
      tidyselect::all_of(cols1),
      replacement_text, removed_text,
      tidyselect::all_of(cols2)
    )
}
