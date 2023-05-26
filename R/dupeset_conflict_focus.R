#' Focus on dupesets with conflicting values in selected variables
#'
#' Dupesets are records that have the same values in selected variables. However, within a dupeset, any other variables may contain conflicting values. `dupeset_conflict_focus()` produces a dataframe that keeps only those dupesets that have conflicting values in the variables named in `vars`. This makes it easier to visually inspect the data and look for patterns among dupesets, especially when [dupes2xl()] is run on the returned dataframe.
#'
#' @param df A dataframe of dupesets returned by [undupe()].
#' @param vars A character vector of variable names in `df`.
#' @param silent Logical: silence progress bar if `TRUE`.
#'
#' @return A dataframe.
#' @export
#'
#' @family undupe functions
#' @examples
#' n_rows <- 20
#' df <- data.frame(
#'   x = sample(c("cat", "horse", "howler monkey"), size = n_rows, replace = TRUE),
#'   y = sample(c(1, 10, 100, NA), size = n_rows, replace = TRUE),
#'   z = sample(c("banana", "carrot", "pickle"), size = n_rows, replace = TRUE)
#' )
#' undupe <- undupe(df, undupe_vars = c("x", "y"))
#' df_distilled <- dupeset_conflict_focus(undupe[["df_dupesets"]], vars = "z")
dupeset_conflict_focus <- function(df, vars, silent = FALSE) {
  var_check(df, var = c("dupe_type", vars))

  df2 <- df %>%
    mutate(n_row = row_number()) %>%
    select(all_of(vars), dupe_type, n_row)
  df2 <- df2 %>%
    relocate(dupe_type, .after = colnames(df2)[length(colnames(df2))])

  keep_rows <- c()
  n_vars <- ncol(df2) - 2

  seq_start <- which(toupper(df2$dupe_type) == "RETAINED")
  seq_end <- c(as.integer(seq_start - 1)[-1], nrow(df2))
  seq <- mapply(seq, seq_start, seq_end)

  # If n_uniq > 1, the set contains multiple unique values
  f <- function(col, seq) {
    set <- col[seq]
    n_uniq <- ifelse(purrr::is_empty(set[set != ""]),
      1,
      sum(!duplicated(set[set != ""]))
    )
    if (n_uniq != 1) seq
  }

  if (n_vars > 1 & !silent) {
    message("Finding differences in dupesets")
    pb <- txtProgressBar(1, n_vars, width = 50, style = 3)
  }

  for (i in 1:n_vars) {
    r <- sapply(seq, f, col = df2[[i]], simplify = T)
    keep_rows <- unique(c(keep_rows, unlist(r)))
    if (n_vars > 1 & !silent) setTxtProgressBar(pb, i)
  }

  if (n_vars > 1 & !silent) close(pb)

  # End function if no differences found
  if (is.null(keep_rows)) {
    return(message("No differences found among dupesets in the variable(s) provided"))
  }

  df_distilled <- data.frame(n_row = sort(unique(keep_rows)))

  df_distilled <- df %>%
    mutate(n_row = row_number()) %>%
    right_join(df_distilled, by = "n_row")

  df_distilled
}
