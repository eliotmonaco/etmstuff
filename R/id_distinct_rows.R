#' Assign a sequential ID number to distinct rows
#'
#' This function assigns a sequential ID number to distinct rows in a dataframe  based on selected variables (`var`). The new ID is a string, with the number of characters equal to `nchar(nrow(df))` (if there are 100 rows, the new ID will have three characters).
#'
#' @param df A dataframe.
#' @param id_name A name for the new ID variable.
#' @param prefix An optional prefix of characters to add to all ID values.
#' @param var The variable names whose unique values the new ID will be based on. Defaults to `colnames(df)`.
#' @param seq_start An integer for the beginning of the ID sequence. Defaults to `1`.
#' @param digits An integer for the number of digits in the ID (including leading zeros). Defaults to `nchar(nrow(df) + seq_start)`.
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' n_rows <- 50
#'
#' df <- data.frame(
#'   x = sample(c("cat", "horse", "howler monkey"), size = n_rows, replace = TRUE),
#'   y = sample(month.name, size = n_rows, replace = TRUE),
#'   z = seq(1, n_rows)
#' )
#'
#' df <- id_distinct_rows(df, id_name = "new_id", prefix = "A", var = c("x", "y"))
#'
id_distinct_rows <- function(df, id_name, prefix = NULL, var = colnames(df), seq_start = 1, digits = NULL) {
  if (!is.data.frame(df)) stop("`df` must be a dataframe")

  var_check(df, var = var)

  if (id_name %in% colnames(df)) {
    m <- paste0("`", id_name, "` is already a column in `df`. Choose a different `id_name`.")
    stop(m)
  }

  # Find the minimum number of digits for the ID value
  digits_min <- nchar(nrow(df) + seq_start)

  # Use `digits_min` as the ID character length if `digits` is null or fewer than `digits_min`
  if (is.null(digits) || digits < digits_min) {
    digits <- digits_min
  }

  # Deduplicate rows by `var`
  df_distinct <- df |>
    dplyr::select(tidyselect::all_of(var)) |>
    dplyr::distinct()

  #  Add sequential ID numbers to distinct rows
  df_distinct <- df_distinct |>
    dplyr::mutate({{ id_name }} := formatC(
      x = seq_start:(seq_start + nrow(df_distinct) - 1),
      width = digits,
      flag = "0"
    ))

  # Add `prefix` to ID
  if (!is.null(prefix)) {
    df_distinct[[id_name]] <- paste0(prefix, df_distinct[[id_name]])
  }

  # Join IDs back to full dataframe
  df |>
    dplyr::left_join(df_distinct, by = var)
}
