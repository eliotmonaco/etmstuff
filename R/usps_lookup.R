#' Validate addresses at USPS.com
#'
#' @description
#' This function submits a table of addresses to the ZIP Code by Address lookup tool at USPS.com for validation.
#'
#' @details
#' The `row_id` argument is optional but recommended to easily match the results to the submitted addresses. In the returned dataframe, `n_row_src` corresponds to the row number of the input dataframe, and `n_result` indicates if multiple addresses were returned for any submitted address.
#'
#' Resources
#'
#' * [ZIP Code by Address](https://tools.usps.com/zip-code-lookup.htm?byaddress) lookup tool
#' * [Information about Delivery Point Validation (DPV)](https://www.accuzip.com/webhelp/Appendix/DPV_Codes_and_Information.htm)
#'
#' @param df A dataframe of addresses.
#' @param row_id A unique row identifier variable in `df`.
#' @param var A vector of variable names for the following address components in this order: 1) house number and street, 2) unit number, 3) city, 4) state, and 5) zip code. Defaults to `c("street", "unit", "city", "state", "zip")`.
#'
#' @return A dataframe of results from USPS.com.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   id = c("KDHE Division of Public Health", "Curtis State Office Bldg"),
#'   street = c("1000 SW Jackson", "1000 SW Jackson"),
#'   unit = c("Suite 540", NA),
#'   city = c("Topeka", "Topeka"),
#'   state = c("KS", "KS"),
#'   zip = c("66612", "66612")
#' )
#'
#' \dontrun{
#' df_results <- usps_lookup(df, row_id = "id")
#' }
#'
usps_lookup <- function(df, row_id = NULL, var = c("street", "unit", "city", "state", "zip")) {
  if (!is.data.frame(df)) stop("`df` must be a dataframe")
  if (!is.null(row_id) && length(row_id) != 1) stop("`row_id` must have length of 1")

  var_check(df, var = c(row_id, var))

  var2 <- c(
    row_id, "n_row_src", "n_result", "street", "city",
    "state", "zip5", "zip4", "county", "DPV"
  )

  df_usps <- tibble::as_tibble(matrix(
    nrow = 0, ncol = length(var2), dimnames = list(c(), var2)
  ))

  df_results <- usps_lookup_loop(df, df_usps, var, row_id)

  df_results
}

usps_lookup_loop <- function(df_in, df_out, var, row_id, shiny = FALSE) {
  j <- 1
  k <- -1

  for (i in 1:nrow(df_in)) {
    if (!is.null(row_id)) id <- df_in[i, row_id]

    if (j > 1) {
      k <- k + j - 1
    }

    html <- try(usps_get_page(
      street = df_in[i, var[1]],
      unit = df_in[i, var[2]],
      city = df_in[i, var[3]],
      state = df_in[i, var[4]],
      zip = df_in[i, var[5]]
    ))

    if ("try-error" %in% class(html) | length(html$addressList) == 0) {
      n_results <- 1
    } else {
      n_results <- length(html$addressList)
    }

    for (j in 1:n_results) {
      r <- i + j + k

      df_out[r,] <- ""
      if (!is.null(row_id)) df_out[r, row_id] <- id
      df_out$n_row_src[r] <- i
      df_out$n_result[r] <- j

      if ("try-error" %in% class(html) | length(html$addressList) == 0) {
        df_out$street[r] <- "No result or unexpected/missing input"
        next
      }

      result <- usps_get_result(html, j)

      df_out$street[r] <- result[1]
      df_out$city[r] <- result[2]
      df_out$state[r] <- result[3]
      df_out$zip5[r] <- result[4]
      df_out$zip4[r] <- result[5]
      df_out$county[r] <- result[6]
      df_out$DPV[r] <- result[7]
    }

    if (shiny) shiny::incProgress(1 / nrow(df_in))
  }

  df_out
}

usps_get_page <- function(street, unit = NULL, city = NULL, state = NULL, zip = NULL) {
  my_url <- "https://tools.usps.com/tools/app/ziplookup/zipByAddress"

  my_ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/105.0.0.0 Safari/537.36"

  form <- list(
    address1 = street,
    address2 = unit,
    city = city,
    state = state,
    zip = zip
  )

  response <- httr::POST(
    my_url,
    httr::user_agent(my_ua),
    body = form,
    encode = "form"
  )

  html <- httr::content(response)

  html
}

usps_get_result <- function(html, n) {
  result <- c(
    html$addressList[[n]]$addressLine1,
    html$addressList[[n]]$city,
    html$addressList[[n]]$state,
    html$addressList[[n]]$zip5,
    html$addressList[[n]]$zip4,
    html$addressList[[n]]$countyName,
    html$addressList[[n]]$dpvConfirmation
  )

  result
}

usps_lookup_shiny <- function(df) {
  if (!is.data.frame(df)) {
    stop("Input must be a dataframe")
  } else if (!all(var %in% colnames(df))) {
    stop("Expected column names not found")
  }

  var <- c("street", "unit", "city", "state", "zip")

  # If one extra column is present in `df`, it becomes the row ID in `df_usps`
  if ((length(colnames(df)) - length(var)) == 1) {
    row_id <- colnames(df)[which(!colnames(df) %in% var)]
  } else {
    row_id <- NULL
  }

  var2 <- c(
    row_id, "n_row_src", "n_result", "street", "city",
    "state", "zip5", "zip4", "county", "DPV"
  )

  df_usps <- tibble::as_tibble(matrix(
    nrow = 0, ncol = length(var2), dimnames = list(c(), var2)
  ))

  shiny::withProgress(message = "Getting addresses", {
    df_results <- usps_lookup_loop(df, df_usps, var, row_id, shiny = TRUE)
  })

  df_results
}
