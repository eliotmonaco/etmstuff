#' Simulate addresses
#'
#' Create a dataframe of simulated addresses. Street names are sampled from `street_names`. City, zip code, and county combinations are sampled from `ks_city_zip`. By setting `dirty = TRUE`, the function will alter values in `street`, `city`, and `zip` for the purpose of testing [validate_address()] and [clean_address()].
#'
#' @param nrow An integer to set the number of rows in the output.
#' @param dirty Logical: creates dirty data in `street`, `city`, and `zip` when set to `TRUE`.
#'
#' @return A dataframe.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' df <- sim_address(nrow = 1000, dirty = TRUE)
#'
sim_address <- function(nrow, dirty = FALSE) {
  v_street <- sim_street(nrow)
  v_unit <- sim_unit(nrow)

  df <- data.frame(
    street = v_street,
    unit = v_unit,
    state = "KS"
  )

  df2 <- etmstuff::ks_city_zip[sample(1:nrow(etmstuff::ks_city_zip), size = nrow, replace = TRUE),]

  df <- df %>%
    dplyr::bind_cols(df2) %>%
    dplyr::select("street", "unit", "city", "state", "zip", "county")

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
  streets <- etmstuff::street_names[sample(1:length(etmstuff::street_names), size = n, replace = TRUE)]

  samp <- model_house_num_dist(sample_size = 100 * n)

  numbers <- sample(
    samp,
    size = n,
    replace = TRUE
  )

  paste(numbers, streets)
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

# Function to model the distribution of house numbers
model_house_num_dist <- function(sample_size) {
  # Create intervals of 100 for house numbers
  hn_max <- 40000
  int_size <- 100
  n_int <- hn_max / int_size
  int_seq1 <- 1:n_int
  int_seq2 <- seq(0, hn_max, by = int_size)

  # Create bin count for each house number interval
  m <- 2.7
  sd <- 1.2
  p <- stats::dlnorm(int_seq1, meanlog = m, sdlog = sd)
  samp <- sample(int_seq1, sample_size, replace = TRUE, prob = p)
  bincounts <- tabulate(samp)

  # Add zero counts for any missing bins at the end of `bincounts`
  n_zeros <- 400 - length(bincounts)
  bincounts <- c(bincounts, rep(0, times = n_zeros))

  # Create distribution for each house number interval
  dist <- list()
  for (i in 1:(length(int_seq2) - 1)) {
    min <- int_seq2[i]
    max <- int_seq2[i + 1] - 1
    m <- 2.6
    sd <- 1.2
    p <- stats::dlnorm(1:100, meanlog = m, sdlog = sd)
    dist[[i]] <- sample(min:max, size = bincounts[i], replace = TRUE, prob = p)
  }

  unlist(dist)
}
