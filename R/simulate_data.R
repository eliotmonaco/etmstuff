#' Simulate addresses to test functions
#'
#' Create a dataframe of simulated addresses. Street names are from `street_names`, which is based on a list of the most common street names in the US. Cities and zip codes are from `ks_cities` and `ks_zipcodes`, which are the reference lists used to check those components in [validate_address()]. Other elements (e.g., house numbers, directions, street suffixes, and units) are generated pseudorandomly using `sample()`. By setting `dirty = TRUE`, the function will alter values in `street`, `city`, and `zip` for the purpose of testing [validate_address()] and [clean_street_address()].
#'
#' @param nrow An integer to set the number of rows in the output.
#' @param dirty Logical: creates dirty data in `street`, `city`, and `zip` when set to `TRUE`.
#'
#' @return A dataframe.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' df <- simulate_data(nrow = 1000, dirty = TRUE)
#'
simulate_data <- function(nrow, dirty = FALSE) {
  # Create `street` column
  df <- data.frame(
    street = stringr::str_squish(paste(
      sample(c(1:9999), nrow, replace = TRUE),
      sample(
        c(directions, ""),
        nrow,
        replace = TRUE,
        prob = c(rep(.35 / 8, length.out = 8), .65)
      ),
      sample(street_names, nrow, replace = TRUE),
      sample(
        c(common_street_suffixes, ""),
        nrow,
        replace = TRUE,
        prob = c(rep(.65 / 7, length.out = 7), .35)
      )
    ))
  )

  # Create `unit` column
  df$unit <- NA
  n <- nrow %/% 5
  units <- paste0(
    sample(
      c(1:50, ""),
      n,
      replace = TRUE,
      prob = c(rep(.85 / 50, length.out = 50), .15)
    ),
    sample(
      c(LETTERS[1:8], ""),
      n,
      replace = TRUE,
      prob = c(rep(.65 / 8, length.out = 8), .35)
    )
  )
  units <- units[!units == ""]
  units <- stringr::str_trim(paste(
    sample(
      c(common_unit_prefixes, ""),
      length(units),
      replace = TRUE,
      prob = c(rep(.5 / 6, length.out = 6), .5)
    ),
    units
  ))
  n <- sample(1:nrow, length(units))
  df$unit[n] <- units

  # Create `city` and `zip` columns
  # n <- sample(length(ks_cities), nrow, replace = TRUE)
  df <- cbind(
    df,
    data.frame(
      city = sample(stringr::str_to_title(ks_cities), nrow, replace = TRUE),
      zip = sample(ks_zipcodes, nrow, replace = TRUE)
    )
  )

  # Create `state` column
  df$state <- "KS"

  if (dirty) {
    # Add PO Box to `street`
    n <- nrow %/% 50
    n <- ifelse(n < 2, 2, n)
    pobox <- c("PO Box", "P.O. Box", "Box")
    pobox <- paste(
      sample(pobox, n, replace = TRUE),
      sample(10:9999, n, replace = TRUE)
    )
    n <- sample(1:nrow, length(pobox))
    df$street[n] <- pobox

    # Misspell `city`: string_delete()
    n <- nrow %/% 100
    n <- ifelse(n < 2, 2, n)
    n <- sample(1:nrow, n)
    df$city[n] <- sapply(df$city[n], string_delete, simplify = TRUE)

    # Misspell `city`: string_add()
    n <- nrow %/% 100
    n <- ifelse(n < 2, 2, n)
    n <- sample(1:nrow, n)
    df$city[n] <- sapply(df$city[n], string_add, simplify = TRUE)

    # Replace `zip`
    n <- nrow %/% 100
    n <- ifelse(n < 2, 2, n)
    n <- sample(1:nrow, n)
    df$zip[n] <- sample(10000:99999, length(n), replace = TRUE)
  }

  df <- id_distinct_rows(df, var = colnames(df), id_name = "address_id")

  df %>%
    dplyr::relocate(address_id)
}

string_delete <- function(s) {
  c <- sample(1:nchar(s), 1)
  stringr::str_sub(s, c, c) <- ""
  s
}

string_add <- function(s) {
  c <- sample(1:nchar(s), 1)
  paste0(
    substr(s, 1, c),
    sample(letters, 1),
    substr(s, c + 1, nchar(s))
  )
}
