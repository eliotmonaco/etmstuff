#' Standardize strings in a dataframe
#'
#' Remove excess spaces from strings, replace empty cells with `NA`, and convert strings to uppercase (optional). If neither `var` nor `var_ignore` are supplied, all character variables in `df` are standardized.
#'
#' @param df A dataframe.
#' @param var A character vector naming which variables to standardize.
#' @param var_ignore A character vector naming which variables to ignore.
#' @param uppercase Logical: change text to uppercase if `TRUE` (default).
#' @param silent Logical: silence output to console if `TRUE`.
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   x = c("pear", " kumquat ", "fuji   apple", " "),
#'   y = 1:4
#' )
#' df_stdz <- standardize(df)
#'
standardize <- function(df,
                        var = NULL,
                        var_ignore = NULL,
                        uppercase = TRUE,
                        silent = FALSE) {
  var_check(df, var = c(var, var_ignore))

  if (!is.null(var) & !is.null(var_ignore)) {
    stop("Only one of `var` or `var_ignore` can be supplied", call. = FALSE)
  } else if (is.null(var) & is.null(var_ignore)) {
    var <- colnames(df)
  } else if (is.null(var)) var <- colnames(df)[!colnames(df) %in% var_ignore]

  if (uppercase) {
    f <- function(c) {
      stringr::str_to_upper(dplyr::na_if(stringr::str_squish(c), ""))
    }
  } else if (!uppercase) {
    f <- function(c) {
      dplyr::na_if(stringr::str_squish(c), "")
    }
  }

  if (!silent) {
    message(paste("Standardizing", deparse(substitute(df))))
    pb <- utils::txtProgressBar(1, length(var), width = 50, style = 3)
  }

  for (i in 1:length(var)) {
    if (!silent) utils::setTxtProgressBar(pb, i)
    skip <- !is.character(df[[var[i]]])
    if (skip) {
      next
    }
    df[, var[i]] <- f(df[[var[i]]])
  }

  if (!silent) close(pb)

  df
}
