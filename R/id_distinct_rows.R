#' Assign a sequential ID to distinct rows
#'
#' Assign a sequential ID number to distinct rows in `df` based on selected variables (`var`). The new ID is a string, with the number of characters equal to `nchar(nrow(df))` (if there are 100 rows, the new ID will have three characters).
#'
#' @param df A dataframe.
#' @param var A vector of variable names that the new ID will be based on.
#' @param id_name A name for the new ID variable.
#' @param seq_start An integer for the beginning of the ID sequence. Defaults to `1`.
#' @param digits An integer for the number of digits in the ID (including leading zeros). Defaults to `nchar(nrow(df))`.
#'
#' @return A dataframe.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' n_rows <- 20
#' df <- data.frame(
#'   x = sample(c("cat", "horse", "howler monkey"), size = n_rows, replace = TRUE),
#'   y = sample(c(1, 10, 100, NA), size = n_rows, replace = TRUE),
#'   z = rep("ignore this", length.out = n_rows)
#' )
#' df_new <- id_distinct_rows(df, var = c("x", "y"), id_name = "new_id")
#'
id_distinct_rows <- function(df, var, id_name, seq_start = 1, digits = NULL) {
  var_check(df, var = var)

  min_width <- nchar(nrow(df) + seq_start)

  if (is.null(digits)) {
    digits <- min_width
  } else if (digits < min_width) {
    digits <- min_width
  }

  df_ids <- df %>%
    dplyr::select(tidyselect::all_of(var)) %>%
    dplyr::distinct() %>%
    dplyr::mutate({{ id_name }} := formatC(
      x = seq_start:(seq_start + nrow(.) - 1),
      width = digits,
      flag = "0"
    ))

  df %>%
    dplyr::left_join(df_ids, by = var)
}
