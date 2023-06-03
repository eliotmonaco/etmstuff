#' Submit requests to Melissa Data Personator
#'
#' @description
#' This function submits the URL requests created by [build_md_url()] to the Melissa Data Personator Consumer Web Service API. The response is added to `df` as additional columns.
#'
#' @inheritSection pull_addresses Address processing workflow
#' @inheritSection pull_addresses Address validation and geocoding
#'
#' @param df A dataframe with the variable `md_url`.
#'
#' @return A dataframe with responses from Melissa Data in additional columns.
#' @export
#'
#' @family address processing functions
# @examples
#'
submit_to_md <- function(df) {

  var_check(df, var = "md_url")

  # Submit URLs to the Melissa Data Personator API
  f <- function(r) {
    json_data <- jsonlite::fromJSON(url(r["md_url"]), flatten = TRUE)
    df_json <- as.data.frame(json_data)
    colnames(df_json) <- stringr::str_remove(colnames(df_json), "Records\\.")
    df_json
  }

  df2 <- apply(df, 1, f, simplify = TRUE)

  # Combine Melissa Data results by row
  df2 <- as.data.frame(do.call(rbind, df2))

  # Combine results columns to `df`
  cbind(df, df2)

}
