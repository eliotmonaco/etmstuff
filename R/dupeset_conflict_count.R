#' For each dataframe variable count the number of dupesets with conflicting values
#'
#' Dupesets are records that have the same values in selected variables. However, within a dupeset, any other variables may contain conflicting values. `dupeset_conflict_count()` tallies the number of dupesets with conflicting values for each variable in `df`.
#'
#' @param df A dataframe of dupesets returned by [undupe()].
#'
#' @return A table with one row per variable in `df` and both the count and percentage of dupesets in which value differences occur for each variable.
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
#' df_diff_count <- dupeset_conflict_count(undupe[["df_dupesets"]])
dupeset_conflict_count <- function(df) {
  var_check(df, var = "dupe_type")

  df <- df %>%
    relocate(dupe_type, .after = colnames(df)[length(colnames(df))])

  df_diffs <- data.frame(
    var_name = colnames(df)[-which(colnames(df) == "dupe_type")],
    diff_ct = 0
  )

  seq_start <- which(toupper(df$dupe_type) == "RETAINED")
  seq_end <- c(as.integer(seq_start - 1)[-1], nrow(df))
  seq <- mapply(seq, seq_start, seq_end)

  # If n_uniq > 1, the set contains multiple unique values
  f <- function(col, seq) {
    set <- col[seq]
    n_uniq <- ifelse(purrr::is_empty(set[set != ""]),
      1,
      sum(!duplicated(set[set != ""]))
    )
    n_uniq != 1
  }

  n_vars <- ncol(df) - 1

  message("Finding differences in dupesets")
  pb <- txtProgressBar(1, n_vars, width = 50, style = 3)

  for (i in 1:n_vars) {
    df_diffs$diff_ct[i] <- sum(sapply(seq, f, col = df[[i]], simplify = T))
    setTxtProgressBar(pb, i)
  }

  close(pb)

  n_sets <- sum(toupper(df$dupe_type) == "RETAINED")

  df_diffs <- df_diffs %>%
    mutate(diff_pct = round(diff_ct / n_sets * 100, digits = 2))

  df_diffs
}
