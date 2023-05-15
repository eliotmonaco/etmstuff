#' Standardize strings in a dataframe
#'
#' Remove excess spaces from strings, replace empty cells with `NA`, and convert strings to uppercase (optional).
#'
#' @param df A dataframe.
#' @param uppercase Logical: change text to uppercase if `TRUE` (default).
#' @param vars_ignore A character vector naming which variables to ignore.
#' @param silence Logical: silence progress bar if `TRUE`.
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' df <- data.frame(x = c("pear", " kumquat ", "fuji   apple", " "),
#'                  y = 1:4)
#' df_stdz <- standardize(df)

standardize <- function(df, uppercase=T, vars_ignore=NULL, silence=FALSE) {

  var_check(df, var = vars_ignore)

  if (uppercase) {
    f <- function(c) str_to_upper(na_if(str_squish(c), ""))
  } else if (!uppercase) {
    f <- function(c) na_if(str_squish(c), "")
  }

  if (!silence) message(paste("Standardizing", deparse(substitute(df))))

  pbmax <- ncol(df)
  pb <- txtProgressBar(1, pbmax, width = 50, style = 3)

  for (i in 1:ncol(df)) {
    if (!silence) setTxtProgressBar(pb, i)
    skip1 <- colnames(df)[i] %in% vars_ignore
    skip2 <- !is.character(df[[i]])
    if (skip1 | skip2) {
      next
    }
    df[, i] <- f(df[[i]])
  }

  if (!silence) close(pb)

  df

}
