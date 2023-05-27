#' Count dupesets with conflicting values
#'
#' @description
#' For each variable in a dataframe, `count_conflicts()` counts the number of dupesets with any conflicting values.
#'
#' @section Deduplication:
#' Deduplication is the process of reducing a dataframe to only the distinct rows. Rows that have identical values across a set of selected variables are considered duplicates. These selected variables are "visible" to the process of deduplication. All other variables in the dataframe are "invisible" to deduplication, so they are ignored. Within dupesets, variables that were invisible to deduplication can have conflicting values.
#'
#' The [undupe()] function produces both a deduplicated dataframe and a dataframe of all duplicate rows (dupesets) pulled from the original dataframe.
#'
#' For a more detailed explanation of deduplication, see the [undupe()] documentation.
#'
#' @param df A dataframe of dupesets returned by [undupe()].
#' @param silent Logical: silence progress bar if `TRUE`.
#'
#' @return A dataframe with one row per variable in `df`. The returned dataframe has three columns:
#' * `var_name`: The variable name from `df`.
#' * `conflict_count`: The count of dupesets in `df` with conflicting values.
#' * `conflict_percent`: The percentage of dupesets in `df` with conflicting values.
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
#' df_dupeset_count <- count_conflicts(undupe[["df_dupesets"]])
#'
count_conflicts <- function(df,
                            silent = FALSE) {
  var_check(df, var = "dupe_type")

  df <- df %>%
    relocate(dupe_type, .after = colnames(df)[length(colnames(df))])

  df_conflicts <- data.frame(
    var_name = colnames(df)[-which(colnames(df) == "dupe_type")],
    conflict_count = 0
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

  if (n_vars > 1 & !silent) {
    message("Finding differences in dupesets")
    pb <- txtProgressBar(1, n_vars, width = 50, style = 3)
  }

  for (i in 1:n_vars) {
    df_conflicts$conflict_count[i] <- sum(sapply(seq, f, col = df[[i]], simplify = T))
    if (n_vars > 1 & !silent) setTxtProgressBar(pb, i)
  }

  if (n_vars > 1 & !silent) close(pb)

  n_sets <- sum(toupper(df$dupe_type) == "RETAINED")

  df_conflicts <- df_conflicts %>%
    mutate(conflict_percent = round(conflict_count / n_sets * 100, digits = 2))

  df_conflicts
}
