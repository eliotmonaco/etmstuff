#' Send requests to the Melissa Data Personator API
#'
#' @description
#' This function sends URL requests created by [build_md_url()] to the Melissa Data Personator Consumer Web Service API.
#'
#' @inheritSection pull_addresses Address validation workflow
#' @inheritSection pull_addresses Melissa Data
#'
#' @param url A character vector containing one or more REST requests for Melissa Data Personator.
#'
#' @return A dataframe.
#'
#' @family address processing functions
# @examples
#'
send_md_request <- function(url) {
  md_con <- lapply(url, FUN = url)

  md_list <- lapply(md_con, FUN = jsonlite::fromJSON, flatten = TRUE)

  md_list <- lapply(md_list, FUN = md_to_df)

  df_md <- purrr::list_rbind(md_list)

  df_md[df_md == " "] <- NA

  df_md
}

md_to_df <- function(list) {
  tibble::tibble(list$Records) |>
    tibble::add_column(tibble::as_tibble(list[2:5]))
}
