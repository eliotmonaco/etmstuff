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
#' df <- sim_address(nrow = 1000, dirty = TRUE)
#'
sim_address <- function(nrow, dirty = FALSE) {
  df <- data.frame(
    street = sim_street(nrow),
    unit = sim_unit(nrow),
    state = "KS"
  )

  ks_locations <- ks_locations %>%
    dplyr::filter(!stringr::str_detect(city, "Airport|University"))

  df <- df %>%
    dplyr::bind_cols(
      ks_locations[sample(1:nrow(ks_locations), size = nrow, replace = TRUE),]
    ) %>%
    dplyr::select(street, unit, city, state, zip, county)

  # Add dirty data to `df`
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

  # # Add `address_id`
  # df <- id_distinct_rows(df, var = colnames(df), id_name = "address_id")
  #
  # df %>%
  #   dplyr::relocate(address_id)

  df
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

sim_street <- function(n) {
  dir <- c(directions$abbr, directions$full)
  ldir <- length(dir)

  suf <- c(street_suffixes$abbr, street_suffixes$full)
  lsuf <- length(suf)

  stringr::str_squish(paste(
    # House number
    sample(
      c(1:9999),
      size = n,
      replace = TRUE
    ),
    # Direction
    sample(
      c(dir, ""),
      size = n,
      replace = TRUE,
      prob = c(rep(.35 / ldir, length.out = ldir), .65)
    ),
    # Street
    sample(
      street_names,
      size = n,
      replace = TRUE
    ),
    # Street suffix
    sample(
      c(suf, ""),
      size = n,
      replace = TRUE,
      prob = c(rep(.65 / lsuf, length.out = lsuf), .35)
    )
  ))
}

sim_unit <- function(n) {
  n2 <- n %/% 5

  # Combine number and letter
  units <- paste0(
    sample(
      c(1:50, ""),
      size = n2,
      replace = TRUE,
      prob = c(rep(.85 / 50, length.out = 50), .15)
    ),
    sample(
      c(LETTERS[1:8], ""),
      size = n2,
      replace = TRUE,
      prob = c(rep(.65 / 8, length.out = 8), .35)
    )
  )

  # Remove any blanks
  units <- units[!units == ""]

  # Add unit prefix
  pre <- c(unit_prefixes$abbr, unit_prefixes$full)
  lpre <- length(pre)

  units <- stringr::str_squish(paste(
    sample(
      c(pre, ""),
      size = length(units),
      replace = TRUE,
      prob = c(rep(.5 / lpre, length.out = lpre), .5)
    ),
    units
  ))

  v <- rep(NA, length.out = n)

  # Positions in `v` to assign the values in `units`
  pos <- sample(
    1:n,
    size = length(units),
    replace = FALSE
  )

  v[pos] <- units

  v
}












