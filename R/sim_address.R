#' Simulate addresses
#'
#' Create a dataframe of simulated addresses. Street names are sampled from `street_names`. City, zip code, and county combinations are sampled from `ks_city_zip`. By setting `dirty = TRUE`, the function will alter values in `street`, `city`, and `zip` for the purpose of testing [validate_address()] and [clean_address()].
#'
#' @param n An integer to set the number of rows in the output.
#' @param dirty Logical: creates dirty data in `street`, `city`, and `zip` when set to `TRUE`.
#'
#' @return A dataframe.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
# df <- sim_address(n = 1000, dirty = TRUE)
#' df <- sim_address(n = 1000)
#'
sim_address <- function(n, dirty = FALSE) {
  # Street component
  v_streets <- sim_street(n)

  # Unit component
  v_units <- sim_unit(round(n * .14))

  # City, state, zip, & county components
  df <- etmstuff::ks_city_zip[sample(1:nrow(etmstuff::ks_city_zip), size = n, replace = TRUE),]

  df$street <- v_streets
  df$unit <- sample(c(v_units, rep(NA, times = n - length(v_units))), size = n, replace = FALSE)
  df$state <- "KS"

  df <- df %>%
    dplyr::select("street", "unit", "city", "state", "zip", "county")

  # Add dirty data to `df`
  if (dirty) {
  #   # Add PO Box to `street`
  #   n <- n %/% 50
  #   n <- ifelse(n < 2, 2, n)
  #   pobox <- c("PO Box", "P.O. Box", "Box")
  #   pobox <- paste(
  #     sample(pobox, n, replace = TRUE),
  #     sample(10:9999, n, replace = TRUE)
  #   )
  #   n <- sample(1:n, length(pobox))
  #   df$street[n] <- pobox
  #
  #   # Misspell `city`: string_delete()
  #   n <- n %/% 100
  #   n <- ifelse(n < 2, 2, n)
  #   n <- sample(1:n, n)
  #   df$city[n] <- sapply(df$city[n], string_delete, simplify = TRUE)
  #
  #   # Misspell `city`: string_add()
  #   n <- n %/% 100
  #   n <- ifelse(n < 2, 2, n)
  #   n <- sample(1:n, n)
  #   df$city[n] <- sapply(df$city[n], string_add, simplify = TRUE)
  #
  #   # Replace `zip`
  #   n <- n %/% 100
  #   n <- ifelse(n < 2, 2, n)
  #   n <- sample(1:n, n)
  #   df$zip[n] <- sample(10000:99999, length(n), replace = TRUE)
  }

  df
}

sim_street <- function(n) {
  streets <- etmstuff::street_names[sample(1:length(etmstuff::street_names), size = n, replace = TRUE)]

  samp <- sim_distribution(
    sample_size = 100 * n, max_value = 40000, bin_width = 100,
    m_full = 2.7, sd_full = 1.2, m_bin = 2.6, sd_bin = 1.2
  )

  numbers <- sample(samp, size = n, replace = TRUE)

  paste(numbers, streets)
}

sim_unit <- function(n) {
  # Unit prefix
  prefixes <- c("Apt", "Lot", "Ste", "Trlr", "Unit")
  p_pfx <- c(82, 14.5, .5, 1, 2)
  unit_pfx <- sample(prefixes, size = n, replace = TRUE, prob = p_pfx)

  # Unit location
  samp <- sim_distribution(
    sample_size = n * 100, max_value = 10000, bin_width = 100,
    m_full = 0, sd_full = 1, m_bin = 1.5, sd_bin = 1
  )

  loc_number <- sample(samp, size = n * .87, replace = TRUE)
  loc_number <- c(loc_number, rep("", times = n - length(loc_number)))

  p_letters <- c(
    .248, .245, .143, .112, .064, .047, .036, .028, .009, .012, .009, .007, .004,
    .005, .001, .004, .001, .002, .004, .005, .003, .001, .007, .002, .002, .001
  )

  loc_letter <- sample(LETTERS, size = n * .22, replace = TRUE, prob = p_letters)
  loc_letter <- c(rep("", times = n - length(loc_letter)), loc_letter)

  # Join
  units <- paste(unit_pfx, paste0(loc_number, loc_letter))

  # Shuffle
  units <- sample(units, size = n, replace = FALSE)

  units
}

# Function to model the distribution of house and unit numbers using log-normal distribution
sim_distribution <- function(sample_size, max_value, bin_width, m_full, sd_full, m_bin, sd_bin) {
  n_bins <- max_value / bin_width
  bins <- 1:n_bins
  bin_ticks <- seq(0, max_value, by = bin_width)
  bin_ticks[1] <- 1

  # Determine the number of values to populate each bin
  p <- stats::dlnorm(bins, meanlog = m_full, sdlog = sd_full)
  samp <- sample(bins, size = sample_size, replace = TRUE, prob = p)
  bincounts <- tabulate(samp)

  # Add zero counts for any missing bins at the end of `bincounts`
  n_zeros <- n_bins - length(bincounts)
  bincounts <- c(bincounts, rep(0, times = n_zeros))

  # Create the distribution within each bin
  dist <- list()
  for (i in 1:(length(bin_ticks) - 1)) {
    min <- bin_ticks[i]
    max <- bin_ticks[i + 1] - 1
    p <- stats::dlnorm(1:length(min:max), meanlog = m_bin, sdlog = sd_bin)
    dist[[i]] <- sample(min:max, size = bincounts[i], replace = TRUE, prob = p)
  }

  unlist(dist)
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
