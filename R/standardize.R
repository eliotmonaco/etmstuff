# Remove extraneous white space, choose a replacement value for blank, NA,
# and "NULL" in cells, and optionally convert characters to uppercase.

standardize <- function(df, replace=NA, uppercase=T, vars_ignore=NULL, silence=F) {

  suppressWarnings({
    library(stringr)
  })

  if (is.na(replace) & uppercase) {
    f <- function(c) str_to_upper(na_if(str_trim(str_squish(str_remove(c, regex("^NULL$", ignore_case = T)))), ""))
  } else if (is.na(replace) & !uppercase) {
    f <- function(c) na_if(str_trim(str_squish(str_remove(c, regex("^NULL$", ignore_case = T)))), "")
  } else if (!is.na(replace) & uppercase) {
    f <- function(c) str_to_upper(str_replace_na(na_if(str_trim(str_squish(str_remove(c, regex("^NULL$", ignore_case = T)))), ""), replace))
  } else if (!is.na(replace) & !uppercase) {
    f <- function(c) str_replace_na(na_if(str_trim(str_squish(str_remove(c, regex("^NULL$", ignore_case = T)))), ""), replace)
  }

  cat(paste0("Standardizing ", deparse(substitute(df)), "\n"))

  pbmax <- ncol(df) - length(vars_ignore)
  pb <- txtProgressBar(1, pbmax, width = 50, style = 3)

  for (i in 1:ncol(df)) {
    if (colnames(df)[i] %in% vars_ignore) {
      next
    }
    df[, i] <- f(df[[i]])
    if (!silence) setTxtProgressBar(pb, i)
  }

  if (!silence) close(pb)

  df

}



