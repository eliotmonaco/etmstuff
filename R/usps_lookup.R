#' Look up addresses at USPS.com
#'
#' This function submits a table of addresses to the ZIP Code by Address lookup tool at USPS.com and returns the lookup results. The variables `street`, `unit`, `city`, `state`, and `zip` are required in `df`. `row_id` is optional but recommended. The variables `n_row_src` and `n_result` are added to enable matching the results to the submitted addresses.
#'
#' Resources
#'
#' * [ZIP Code by Address](https://tools.usps.com/zip-code-lookup.htm?byaddress)
#' * [Information about Delivery Point Validation (DPV)](https://www.accuzip.com/webhelp/Appendix/DPV_Codes_and_Information.htm)
#'
#' @param df A dataframe of addresses with the variables `street`, `unit`, `city`, `state`, and `zip`.
#' @param street The street component of the address.
#' @param unit The unit component of the address.
#' @param city The city component of the address.
#' @param state The state component of the address.
#' @param zip The zip code component of the address.
#' @param row_id A unique row identifier variable in `df` (optional).
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
usps_lookup <- function(df, street = "street", unit = "unit", city = "city", state = "state", zip = "zip", row_id = NULL) {
  vars_addr <- c(street, unit, city, state, zip)

  var_check(df, var = vars_addr)

  # Create `df_usps` with or without `row_id`
  if (!is.null(row_id)) {
    df_usps <- data.frame(matrix(nrow = 0, ncol = 10))
    colnames(df_usps) <- c(
      row_id, "n_row_src", "n_result", "street", "city",
      "state", "zip5", "zip4", "county", "DPV"
    )
  } else {
    df_usps <- data.frame(matrix(nrow = 0, ncol = 9))
    colnames(df_usps) <- c(
      "n_row_src", "n_result", "street", "city",
      "state", "zip5", "zip4", "county", "DPV"
    )
  }

  df_usps <- usps_lookup_loop(
    input = df,
    output = df_usps,
    street = street,
    unit = unit,
    city = city,
    state = state,
    zip = zip,
    row_id = row_id
  )

  df_usps
}

usps_lookup_shiny <- function(df) {
  vars_addr <- c("street", "unit", "city", "state", "zip")

  if (!is.data.frame(df)) {
    stop("Input is not a dataframe")
  } else if (!all(vars_addr %in% colnames(df))) {
    stop("Expected column names not found")
  }

  # If one extra column is present in `df`, it becomes the row ID in `df_usps`
  if ((length(colnames(df)) - length(vars_addr)) == 1) {
    row_id <- colnames(df)[which(!colnames(df) %in% vars_addr)]
    df_usps <- data.frame(matrix(nrow = 0, ncol = 10))
    colnames(df_usps) <- c(
      row_id, "n_row_src", "n_result", "street", "city",
      "state", "zip5", "zip4", "county", "DPV"
    )
  } else {
    row_id <- NULL
    df_usps <- data.frame(matrix(nrow = 0, ncol = 9))
    colnames(df_usps) <- c(
      "n_row_src", "n_result", "street", "city",
      "state", "zip5", "zip4", "county", "DPV"
    )
  }

  shiny::withProgress(message = "Getting addresses", {
    df_usps <- usps_lookup_loop(
      input = df,
      output = df_usps,
      street = street,
      unit = unit,
      city = city,
      state = state,
      zip = zip,
      row_id = row_id,
      shiny = TRUE
    )
  })

  df_usps
}

# usps_lookup_loop <- function(input, row_id = NULL, output, shiny = FALSE) {
usps_lookup_loop <- function(input, output, street, unit, city, state, zip, row_id = NULL, shiny = FALSE) {
  df <- input
  df_usps <- output

  j <- 1
  k <- -1

  for (i in 1:nrow(df)) {
    if (!is.null(row_id)) id <- df[i, row_id]

    if (j > 1) {
      k <- k + j - 1
    }

    html <- try(
      usps_get_page(
        street = df[i, street],
        unit = df[i, unit],
        city = df[i, city],
        state = df[i, state],
        zip = df[i, zip]
      )
    )

    if ("try-error" %in% class(html) | length(html$addressList) == 0) {
      n_results <- 1
    } else {
      n_results <- length(html$addressList)
    }

    for (j in 1:n_results) {
      r <- i + j + k

      df_usps[r,] <- ""
      if (!is.null(row_id)) df_usps[r, row_id] <- id
      df_usps$n_row_src[r] <- i
      df_usps$n_result[r] <- j

      if ("try-error" %in% class(html) | length(html$addressList) == 0) {
        df_usps$street[r] <- "No result or unexpected/missing input"
        next
      }

      result <- usps_get_result(html, j)

      df_usps$street[r] <- result[1]
      df_usps$city[r] <- result[2]
      df_usps$state[r] <- result[3]
      df_usps$zip5[r] <- result[4]
      df_usps$zip4[r] <- result[5]
      df_usps$county[r] <- result[6]
      df_usps$DPV[r] <- result[7]
    }

    if (shiny) shiny::incProgress(1 / nrow(df))
  }

  df_usps
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

  resp <- httr::POST(
    my_url,
    httr::user_agent(my_ua),
    body = form,
    encode = "form"
  )

  html <- httr::content(resp)

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
