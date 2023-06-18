#' Send requests to the Melissa Data Personator API
#'
#' @description
#' These functions send the URL requests created by [build_md_url()] to the Melissa Data Personator Consumer Web Service API. `md_request()` sends a single request. `md_batch_request()` sends multiple requests and adds the responses from Melissa Data to `df` as additional columns.
#'
#' @inheritSection pull_addresses Address validation workflow
#' @inheritSection pull_addresses Melissa Data
#'
#' @param df A dataframe.
#' @param var_url The name of the variable in `df` containing the URL requests for Melissa Data Personator. Defaults to `md_url`.
#' @param url A single URL request for Melissa Data Personator.
#'
#' @return A dataframe with a single or multiple responses from Melissa Data Personator.
#'
# @examples
#'
#' @name md_request
NULL

#' @export
#' @rdname md_request
#' @family address processing functions
md_batch_request <- function(df, var_url = "md_url") {
  var_check(df, var = var_url)

  # Submit URLs to the Melissa Data Personator API
  f <- function(r) {
    tryCatch(
      {
        md_request(r[var_url])
      },
      error = function(e) {
        NA
      }
    )
  }

  df2 <- apply(df, 1, f, simplify = TRUE)

  # Combine Melissa Data results by row
  df2 <- as.data.frame(do.call(rbind, df2))

  # Add results columns to `df`
  cbind(df, df2)
}

#' @export
#' @rdname md_request
#' @family address processing functions
md_request <- function(url) {
  x <- jsonlite::fromJSON(
    url(url),
    flatten = TRUE
  )

  cbind(
    data.frame(x$Records),
    as.data.frame(x[2:5])
  )
}
