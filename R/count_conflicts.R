#' Count dupesets with conflicting values
#'
#' @description
#' For each variable in a dataframe, `count_conflicts()` counts the number of dupesets with any conflicting values.
#'
#' @section Deduplication:
#' Deduplication is the process of reducing a dataframe to distinct rows only. Rows that have identical values across a set of selected variables are considered duplicates. These selected variables are "visible" to the process of deduplication. All other variables in the dataframe are "invisible" to deduplication, so they are ignored. Within dupesets, variables that were invisible to deduplication can have conflicting values.
#'
#' The [undupe()] function produces both a deduplicated dataframe and a dataframe of all duplicate rows (dupesets) pulled from the original dataframe.
#'
#' For a more detailed explanation of deduplication, see the [undupe()] documentation.
#'
#' @param df A dataframe of dupesets returned by [undupe()].
#' @param dupe_id The variable name for the ID that groups all members of a duplicate set, created by [undupe()]. Defaults to `"dupe_id"`.
#' @param ignore_empty Logical: omit blank strings and `NA`s when finding conflicts if `TRUE`.
#' @param silent Logical: silence progress indicator if `TRUE`.
#'
#' @return A dataframe with one row per variable in `df` (omitting `dupe_id` and `dupe_order`). The returned dataframe has three columns:
#' * `var_name`: The variable name from `df`.
#' * `n`: The count of dupesets in `df` with conflicting values.
#' * `pct`: The percentage of dupesets in `df` with conflicting values.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @family undupe functions
#' @examples
#' n_rows <- 20
#' df <- data.frame(
#'   x = sample(c("cat", "horse", "howler monkey"), size = n_rows, replace = TRUE),
#'   y = sample(c(1, 10, 100, NA), size = n_rows, replace = TRUE),
#'   z = sample(c("banana", "carrot", "pickle"), size = n_rows, replace = TRUE)
#' )
#' undp <- undupe(df, visible_var = c("x", "y"))
#' df_count <- count_conflicts(undp[["df_dupesets"]])
#'
count_conflicts <- function(df, dupe_id = "dupe_id", ignore_empty = TRUE, silent = FALSE) {
  var_check(df, var = dupe_id)

  # Vector of unique `dupe_id`s
  dupe_ids <- unique(df[[dupe_id]])

  # Find the row sequence matching each `dupe_id` value
  f_seq <- function(x) {
    which(df[[dupe_id]] == dupe_ids[x])
  }

  # Get the list of all duplicate sequences
  seq <- sapply(X = 1:length(dupe_ids), FUN = f_seq)

  if ("dupe_order" %in% colnames(df)) {
    var_rm <- c(dupe_id, "dupe_order")
  } else {
    var_rm <- dupe_id
  }

  # Remove columns that shouldn't be counted
  df <- df %>%
    dplyr::select(-tidyselect::all_of(var_rm))

  # Create dataframe to hold counts of dupesets with conflicts
  df_ct <- data.frame(
    var_name = colnames(df),
    n = NA
  )

  # If the set has > 1 unique value, return 1, otherwise 0
  if (ignore_empty) {
    f_conf <- function(col, seq) {
      set <- col[seq]
      ifelse(length(unique(set[!is.na(set) & set != ""])) > 1, 1, 0)
    }
  } else {
    f_conf <- function(col, seq) {
      set <- col[seq]
      ifelse(length(unique(set)) > 1, 1, 0)
    }
  }

  if (!silent) pb <- utils::txtProgressBar(1, ncol(df), width = 50, style = 3)

  # For each column of `df`, sum the number of conflicts found by `f_conf()`
  for (i in 1:ncol(df)) {
    df_ct$n[i] <- sum(sapply(
      X = seq,
      FUN = f_conf,
      col = df[[i]],
      simplify = T))
    if (!silent) utils::setTxtProgressBar(pb, i)
  }

  if (!silent) close(pb)

  # Add percentage variable to `df_ct`
  df_ct <- df_ct %>%
    dplyr::mutate(pct = etmstuff::pct(.data$n, length(dupe_ids)))

  df_ct
}
