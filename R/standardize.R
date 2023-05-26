#' Standardize strings in a dataframe
#'
#' Remove excess spaces from strings, replace empty cells with `NA`, and convert strings to uppercase (optional). If neither `vars` nor `vars_ignore` are supplied, all character variables in `df` are standardized.
#'
#' @param df A dataframe.
#' @param vars A character vector naming which variables to standardize.
#' @param vars_ignore A character vector naming which variables to ignore.
#' @param uppercase Logical: change text to uppercase if `TRUE` (default).
#' @param silence Logical: silence progress bar if `TRUE`.
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
standardize <- function(df, vars = NULL, vars_ignore = NULL, uppercase = TRUE, silence = FALSE) {
  var_check(df, var = c(vars, vars_ignore))

  if (!is.null(vars) & !is.null(vars_ignore)) {
    stop("Only one of `vars` or `vars_ignore` can be supplied", call. = FALSE)
  } else if (is.null(vars) & is.null(vars_ignore)) {
    vars <- colnames(df)
  } else if (is.null(vars)) vars <- colnames(df)[!colnames(df) %in% vars_ignore]

  if (uppercase) {
    f <- function(c) str_to_upper(na_if(str_squish(c), ""))
  } else if (!uppercase) {
    f <- function(c) na_if(str_squish(c), "")
  }

  if (!silence) message(paste("Standardizing", deparse(substitute(df))))

  pbmax <- length(vars)
  pb <- txtProgressBar(1, pbmax, width = 50, style = 3)

  for (i in 1:length(vars)) {
    if (!silence) setTxtProgressBar(pb, i)
    skip <- !is.character(df[[vars[i]]])
    if (skip) {
      next
    }
    df[, vars[i]] <- f(df[[vars[i]]])
  }

  if (!silence) close(pb)

  df
}
