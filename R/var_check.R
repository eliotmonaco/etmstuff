var_check <- function(df, var) {

  missing <- var[which(!var %in% colnames(df))]

  if (!purrr::is_empty(missing)) {
    m <- paste("Missing dataframe variable(s):", paste(missing, collapse = ", "))
    stop(m, call. = F)
  }

}



