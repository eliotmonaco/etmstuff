#' Focus on dupesets with conflicting values in selected variables
#'
#' @description
#' `isolate_conflicts()` produces a dataframe that keeps only those dupesets that have conflicting values in the variables named in `var`. This makes it easier to visually inspect the data and look for patterns among dupesets, particularly when [dupes2xl()] is used on the returned dataframe to produce a formatted .XLSM file.
#'
#' @inheritSection count_conflicts Deduplication
#'
#' @param df A dataframe of dupesets returned by [undupe()].
#' @param var A character vector of variable names in `df`.
#' @param ignore_empty Logical: omit blank strings and `NA`s when finding conflicts if `TRUE`.
#' @param silent Logical: silence output to console if `TRUE`.
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
#' undupe <- undupe(df, visible_vars = c("x", "y"))
#' df_isolated <- isolate_conflicts(undupe[["df_dupesets"]], var = "z")
#'
isolate_conflicts <- function(df, var, ignore_empty = TRUE, silent = FALSE) {
  var_check(df, var = c("dupe_id", var))

  # Vector of unique `dupe_id`s
  dupe_ids <- unique(df$dupe_id)

  # Find the row sequence matching each `dupe_id` value
  f_seq <- function(x) {
    which(df$dupe_id == dupe_ids[x])
  }

  # Get the list of all duplicate sequences
  seq <- sapply(X = 1:length(dupe_ids), FUN = f_seq)

  # If the set has > 1 unique value, return `seq` (row numbers), otherwise nothing
  if (ignore_empty) {
    f_conf <- function(col, seq) {
      set <- col[seq]
      if (length(unique(set[!is.na(set) & set != ""])) > 1) seq
    }
  } else {
    f_conf <- function(col, seq) {
      set <- col[seq]
      if (length(unique(set))) seq
    }
  }

  if (length(var) > 1 & !silent) pb <- utils::txtProgressBar(1, length(var), width = 50, style = 3)

  # Vector to hold row numbers of dupesets with conflicts
  rows <- c()

  for (i in 1:length(var)) {
    r <- sapply(
      X = seq,
      FUN = f_conf,
      col = df[[var[i]]],
      simplify = TRUE)
    rows <- sort(unique(c(rows, unlist(r))))
    if (length(var) > 1 & !silent) utils::setTxtProgressBar(pb, i)
  }

  if (length(var) > 1 & !silent) close(pb)

  # End function if no conflicts found
  if (is.null(rows)) {
    return(message("No conflicts found"))
  }

  df[rows,]
}
