#' Handle conflicting values within dupe sets
#'
#' These functions take the output of [undupe()] (`df_dupesets` and/or `df_full`) as the first argument.
#'
#' ## Count conflicts
#'
#' `count_conflicts()` counts the occurrence of conflicting values within dupe sets for each variable in the dataframe. When a dupe set contains any conflicts within the values of a single variable (i.e., all values belonging to the members of that dupe set are not identical), the conflict count for that variable increases by one. The sum of all such conflicts across all dupe sets for each variable is returned, as well as the percentage of dupe sets containing conflicts for each variable.
#'
#' ## Isolate conflicts
#'
#' `isolate_conflicts()` returns a dataframe containing only those dupe sets that have conflicting values in the variables named in `var`. This makes it easier to visually inspect the data and look for patterns among dupe sets.
#'
#' ## Flatten conflicts
#'
#' `flatten_conflicts()` deduplicates a dataframe containing dupe sets (either `df_full` or `df_dupesets`). Within each dupe set, conflicting values are merged into one cell per dupe set for selected variables so that no data is lost as a result of deduplication. Only unique values are kept, omitting `NA`s and empty strings.
#'
#' @param df A dataframe containing dupe sets and the duplicate ID variable returned by [undupe()].
#' @param var Variable names in `df`. SAY WHAT HAPPENS SPECIFICALLY IN ISOLATE AND FLATTEN....
#' @param dupe_id The duplicate ID variable name. Defaults to `"dupe_id"`.
#' @param ignore_empty Logical: omit blank strings and `NA`s when finding conflicts if `TRUE`.
#' @param sep A string to separate the merged values from a dupe set. Defaults to `" | "`.
#' @param silent Logical: silence progress indicator if `TRUE`.
#'
#' @return
#' ## Count conflicts
#'
#' A dataframe with one row per variable in `df` (omitting `dupe_id` and `dupe_order`). The returned dataframe has three columns:
#' * `variable`: The variable name from `df`.
#' * `n`: The count of dupe sets in `df` with conflicting values.
#' * `pct`: The percentage of dupe sets in `df` with conflicting values.
#'
#' ## Isolate conflicts
#'
#' A subset of `df` containing only dupe sets with conflicts in the variables provided in `var`.
#'
#' ## Flatten conflicts
#'
#' A deduplicated dataframe containing one row per dupe set in `df` and one column per variable in `var` plus `dupe_id`.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @family undupe functions
#'
#' @examples
#' n_rows <- 20
#'
#' df <- data.frame(
#'   x = sample(c("cat", "horse", "howler monkey"), size = n_rows, replace = TRUE),
#'   y = sample(c(1, 10, 100, NA), size = n_rows, replace = TRUE),
#'   z = sample(c("banana", "carrot", "pickle"), size = n_rows, replace = TRUE)
#' )
#'
#' undp <- undupe(df, var = c("x", "y"))
#'
#' df_count <- count_conflicts(undp$df_dupesets)
#'
#' df_isolated <- isolate_conflicts(undp$df_dupesets, var = "z")
#'
#' df_flat <- flatten_conflicts(undp$df_dupesets, var = "z")
#'
#' @name dupeset_conflicts
NULL

#' @export
#' @rdname dupeset_conflicts
count_conflicts <- function(df, dupe_id = "dupe_id", ignore_empty = TRUE, silent = FALSE) {
  var_check(df, var = dupe_id)

  # Unique `dupe_id`s
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

  # Create dataframe to hold counts of dupe sets with conflicts
  df_ct <- data.frame(
    variable = colnames(df),
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

#' @export
#' @rdname dupeset_conflicts
isolate_conflicts <- function(df, var, dupe_id = "dupe_id", ignore_empty = TRUE, silent = FALSE) {
  var_check(df, var = c(dupe_id, var))

  # Unique `dupe_id`s
  dupe_ids <- unique(df[[dupe_id]])

  # Find the row sequence matching each `dupe_id` value
  f_seq <- function(x) {
    which(df[[dupe_id]] == dupe_ids[x])
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

  # Vector to hold row numbers of dupe sets with conflicts
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

#' @export
#' @rdname dupeset_conflicts
flatten_conflicts <- function(df, var = colnames(df), dupe_id = "dupe_id", sep = " | ", silent = FALSE) {
  var_check(df, var = c(var, dupe_id))

  # Unique `dupe_id`s
  dupe_ids <- unique(df[[dupe_id]])

  # DF to hold flattened `var` values
  df_flat <- data.frame(dupe_ids)
  colnames(df_flat) <- c(dupe_id)

  # Use each value in `seq` (`dupe_ids`) to flatten `var` values
  f <- function(seq, var, separator) {
    df_set <- df %>%
      dplyr::filter(.data[[dupe_id]] == seq)
    values <- stats::na.omit(unique(df_set[[var]]))
    if (is.character(values)) values <- values[values != ""]
    paste0(values, collapse = separator)
  }

  n_cols <- length(var)

  # Change future strategy to multisession (parallel processing). Save original strategy to `oplan` and call on exit.
  oplan <- future::plan(future::multisession)
  on.exit(future::plan(oplan), add = TRUE)

  # Loop through variables in `var` and flatten dupe set values
  for (i in 1:n_cols) {
    # df_flat[, var[i]] <- mapply(f, seq = dupe_ids, var = var[i], SIMPLIFY = TRUE)
    df_flat[, var[i]] <- future.apply::future_mapply(
      FUN = f,
      seq = dupe_ids,
      var = var[i],
      separator = sep,
      SIMPLIFY = TRUE)
    if (!silent) { # Progress indicator
      message(
        "\r",
        paste("Column", i, "of", n_cols, "complete"),
        appendLF = FALSE
      )
      if (i == n_cols) message("\n")
    }
  }

  df_flat
}
