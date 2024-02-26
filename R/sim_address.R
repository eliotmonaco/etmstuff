#' Simulate addresses
#'
#' Create a dataframe of simulated addresses. Street names are sampled from `street_names`. City, zip code, and county combinations are sampled from `ks_city_zip`. By setting `dirty = TRUE`, the function will alter values in `street`, `city`, and `zip` for the purpose of testing [validate_address()] and [clean_address()].
#'
#' @param n_row An integer to set the number of rows in the output.
# @param dirty Logical: creates dirty data in `street`, `city`, and `zip` when set to `TRUE`.
#'
#' @return A dataframe.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' df <- sim_address(n_row = 1000, dirty = TRUE)
#'
sim_address <- function(n_row, dirty = FALSE) {
  v_street <- sim_street(n_row)
  v_unit <- sim_unit(n = round(n_row * .14), len = n_row)

  df <- data.frame(
    street = v_street,
    unit = v_unit,
    state = "KS"
  )

  df2 <- etmstuff::ks_city_zip[sample(1:nrow(etmstuff::ks_city_zip), size = n_row, replace = TRUE),]

  df <- df %>%
    dplyr::bind_cols(df2) %>%
    dplyr::select("street", "unit", "city", "state", "zip", "county")

  # # Add dirty data to `df`
  # if (dirty) {
  #   # Add PO Box to `street`
  #   n <- n_row %/% 50
  #   n <- ifelse(n < 2, 2, n)
  #   pobox <- c("PO Box", "P.O. Box", "Box")
  #   pobox <- paste(
  #     sample(pobox, n, replace = TRUE),
  #     sample(10:9999, n, replace = TRUE)
  #   )
  #   n <- sample(1:n_row, length(pobox))
  #   df$street[n] <- pobox
  #
  #   # Misspell `city`: string_delete()
  #   n <- n_row %/% 100
  #   n <- ifelse(n < 2, 2, n)
  #   n <- sample(1:n_row, n)
  #   df$city[n] <- sapply(df$city[n], string_delete, simplify = TRUE)
  #
  #   # Misspell `city`: string_add()
  #   n <- n_row %/% 100
  #   n <- ifelse(n < 2, 2, n)
  #   n <- sample(1:n_row, n)
  #   df$city[n] <- sapply(df$city[n], string_add, simplify = TRUE)
  #
  #   # Replace `zip`
  #   n <- n_row %/% 100
  #   n <- ifelse(n < 2, 2, n)
  #   n <- sample(1:n_row, n)
  #   df$zip[n] <- sample(10000:99999, length(n), replace = TRUE)
  # }

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

  # samp <- model_house_num_dist(sample_size = 100 * n)
  samp <- model_distribution(
    sample_size = 100 * n, max_value = 40000, bin_width = 100,
    m_full = 2.7, sd_full = 1.2, m_bin = 2.6, sd_bin = 1.2
  )

  numbers <- sample(samp, size = n, replace = TRUE)

  paste(numbers, streets)
}

sim_unit <- function(n, len) {
  # # Combine number and letter
  # units <- paste0(
  #   sample(
  #     c(1:50, ""),
  #     size = n,
  #     replace = TRUE,
  #     prob = c(rep(.85 / 50, length.out = 50), .15)
  #   ),
  #   sample(
  #     c(LETTERS[1:8], ""),
  #     size = n,
  #     replace = TRUE,
  #     prob = c(rep(.65 / 8, length.out = 8), .35)
  #   )
  # )
  #
  # # Remove any blanks
  # units <- units[!units == ""]

  # Unit prefix
  pfx <- c("Apt", "Lot", "Ste", "Trlr", "Unit")
  pfx_prob <- c(82, 14.5, .5, 1, 2)
  unit_pfx <- sample(pfx, size = len, replace = TRUE, prob = pfx_prob)

  # Unit location
  loc_letter <- sample(
    c(LETTERS, NA), size = len, replace = TRUE,
    prob = c(
      0.0550, 0.0543, 0.0317, 0.0249, 0.0142, 0.0104, 0.0079, 0.0062, 0.0020,
      0.0027, 0.0019, 0.0016, 0.0010, 0.0010, 0.0003, 0.0009, 0.0003, 0.0005,
      0.0008, 0.0011, 0.0006, 0.0003, 0.0016, 0.0004, 0.0004, 0.0002, 0.7780
    )
  )

  loc_number


  # units <- stringr::str_squish(paste(
  #   ,
  #   units
  # ))

  v <- rep(NA, length.out = len)

  # Positions in `v` to assign the values in `units`
  pos <- sample(
    1:len,
    size = length(units),
    replace = FALSE
  )

  v[pos] <- units

  v
}

# Function to model the distribution of house and unit numbers using log-normal distribution
model_distribution <- function(sample_size, max_value, bin_width, m_full, sd_full, m_bin, sd_bin) {
  n_bins <- max_value / bin_width
  bins <- 1:n_bins
  bin_ticks <- seq(0, max_value, by = bin_width)
  bin_ticks[1] <- 1

  # Determine the number of values to populate each bin
  p <- dlnorm(bins, meanlog = m_full, sdlog = sd_full)
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
    p <- dlnorm(1:length(min:max), meanlog = m_bin, sdlog = sd_bin)
    dist[[i]] <- sample(min:max, size = bincounts[i], replace = TRUE, prob = p)
  }

  unlist(dist)
}

# # Function to model the distribution of house numbers
# model_house_num_dist <- function(sample_size) {
#   # Create intervals of 100 for house numbers
#   hn_max <- 40000
#   int_size <- 100
#   n_int <- hn_max / int_size
#   int_seq1 <- 1:n_int
#   int_seq2 <- seq(0, hn_max, by = int_size)
#
#   # Create bin count for each house number interval
#   m <- 2.7
#   sd <- 1.2
#   p <- stats::dlnorm(int_seq1, meanlog = m, sdlog = sd)
#   samp <- sample(int_seq1, sample_size, replace = TRUE, prob = p)
#   bincounts <- tabulate(samp)
#
#   # Add zero counts for any missing bins at the end of `bincounts`
#   n_zeros <- 400 - length(bincounts)
#   bincounts <- c(bincounts, rep(0, times = n_zeros))
#
#   # Create distribution for each house number interval
#   dist <- list()
#   for (i in 1:(length(int_seq2) - 1)) {
#     min <- int_seq2[i]
#     max <- int_seq2[i + 1] - 1
#     m <- 2.6
#     sd <- 1.2
#     p <- stats::dlnorm(1:100, meanlog = m, sdlog = sd)
#     dist[[i]] <- sample(min:max, size = bincounts[i], replace = TRUE, prob = p)
#   }
#
#   unlist(dist)
# }
