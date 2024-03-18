#' Simulate addresses
#'
#' @description
#' Create a dataframe of simulated Kansas addresses.
#'
#' @details
#' Street names are sampled from `street_names`. City, zip code, and county combinations are sampled from `ks_city_zip`.
#'
#' @param n The number of addresses in the output.
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' df <- sim_address(n = 1000)
#'
sim_address <- function(n) {
  # Street component
  v_streets <- sim_street(n)

  # Unit component
  v_units <- sim_unit(round(n * .14))

  # City, state, zip, & county components
  df <- ks_city_zip[sample(1:nrow(ks_city_zip), n, TRUE),]

  df$street <- v_streets
  df$unit <- sample(c(v_units, rep(NA, n - length(v_units))), n, FALSE)
  df$state <- "KS"

  df |>
    dplyr::select("street", "unit", "city", "state", "zip", "county")
}

sim_street <- function(n) {
  streets <- street_names[sample(1:length(street_names), n, TRUE)]

  numbers <- sample_address_dist(n, type = "house")

  paste(numbers, streets)
}

sim_unit <- function(n) {
  # Unit prefix
  prefixes <- c("Apt", "Lot", "Ste", "Trlr", "Unit")
  p_pfx <- c(82, 14.5, .5, 1, 2)
  unit_pfx <- sample(prefixes, n, TRUE, prob = p_pfx)

  # Unit location
  loc_number <- sample_address_dist(n * .87, type = "unit")
  loc_number <- c(loc_number, rep("", n - length(loc_number)))
  p <- stats::dlnorm(1:26, meanlog = .8, sdlog = .75)
  loc_letter <- sample(LETTERS, n * .22, TRUE, prob = p)
  loc_letter <- c(rep("", n - length(loc_letter)), loc_letter)

  # Join
  units <- paste(unit_pfx, paste0(loc_number, loc_letter))

  # Shuffle
  sample(units, n, FALSE)
}

# Function to model the distribution of house and unit numbers using log-normal distribution
sample_address_dist <- function(n, type) {
  if (type == "house") {
    max_value <- 40000
    m_full <- 2.7; sd_full <- 1.2
    m_bin <- 2.6; sd_bin <- 1.2
  } else if (type == "unit") {
    max_value <- 10000
    m_full <- 0; sd_full <- 1
    m_bin <- 1.5; sd_bin <- 1
  }

  bin_width <- 100
  n_bins <- max_value / bin_width
  bins <- 1:n_bins
  bin_ticks <- seq(0, max_value, by = bin_width)
  bin_ticks[1] <- 1

  # Overall distribution: determine the number of values to populate each bin of 100
  p <- stats::dlnorm(bins, meanlog = m_full, sdlog = sd_full)
  samp <- sample(bins, 100000, TRUE, prob = p)
  bincounts <- tabulate(samp)

  # Add zero counts for any missing bins at the end of `bincounts`
  n_zeros <- n_bins - length(bincounts)
  bincounts <- c(bincounts, rep(0, n_zeros))

  # Distribution within each bin
  dist <- list()
  for (i in 1:(length(bin_ticks) - 1)) {
    min <- bin_ticks[i]
    max <- bin_ticks[i + 1] - 1
    p <- stats::dlnorm(1:length(min:max), meanlog = m_bin, sdlog = sd_bin)
    dist[[i]] <- sample(min:max, bincounts[i], TRUE, prob = p)
  }
  dist <- unlist(dist)

  sample(dist, n, TRUE)
}
