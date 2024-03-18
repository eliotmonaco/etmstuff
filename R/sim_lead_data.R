#' Simulate lead test records
#'
#' @description
#' This function creates a simulated data set of lead test records.
#'
#' @details
#' `n` determines the number of people in the data set created by `sim_person()`, while `sim_test_seq()` creates a plausible test sequence consisting of one or more tests for each person within the date range of the data set.
#'
#' @param n The number of people to include in the data set.
#' @param dmin,dmax The minimum and maximum of the data set's date range. Must be dates formatted YYYY-MM-DD. `dmax` defaults to `Sys.Date()`.
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' df <- sim_lead_data(n = 1000, dmin = as.Date("2020-01-01"))
#'
sim_lead_data <- function(n, dmin, dmax = Sys.Date()) {
  if (!is.numeric(n) || n != round(n)) stop("`n` must be an integer")
  dmin <- as.Date(dmin, format = "%Y-%m-%d")
  dmax <- as.Date(dmax, format = "%Y-%m-%d")
  if (is.na(dmin) | is.na(dmax)) stop("`dmin` and `dmax` must be dates formatted YYYY-MM-DD")
  df <- sim_person(n)
  tests <- purrr::pmap(
    list(df$person_id, df$birth_date), sim_test_seq,
    dmin = dmin, dmax = dmax
  )
  tests <- lapply(tests, tibble::as_tibble)
  tests <- purrr::list_rbind(tests)
  df <- df |>
    dplyr::left_join(tests, by = "person_id")
  cols <- epitrax_vars$epht_name[epitrax_vars$epht_name %in% colnames(df)]
  df |>
    dplyr::select(tidyselect::all_of(cols))
}

sim_person <- function(n) {
  df_names <- tibble::tibble(
    first_name = c(names$first_male, names$first_female),
    birth_sex = c(rep("Male", length(names$first_male)), rep("Female", length(names$first_female)))
  )
  df <- df_names[sample(1:nrow(df_names), n, TRUE),] |>
    dplyr::mutate(
      last_name = sample(names$last, n, TRUE),
      middle_name = sample(LETTERS, n, TRUE),
      person_id = as.character(100001:(100000 + n)),
      patient_record_number = as.character(2000000001:(2000000000 + n)),
      birth_date = sample(seq.Date(Sys.Date() - 100 * 365, Sys.Date(), "day"), n, TRUE)
    )
  df_addr <- sim_address(n)
  colnames(df_addr) <- paste0("coll_add_", colnames(df_addr))
  cols <- epitrax_vars$epht_name[epitrax_vars$epht_name %in% c(colnames(df), colnames(df_addr))]
  df |>
    dplyr::bind_cols(df_addr) |>
    dplyr::select(tidyselect::all_of(cols))
}

sim_test_seq <- function(id, dob, dmin, dmax) {
  dmin <- max(dob, dmin)
  tests <- list()
  i <- 1
  continue <- TRUE
  while (continue) {
    tests$person_id[i] <- id
    tests$collection_date[i] <- sample(seq.Date(dmin, dmax, "day"), 1)
    tests$specimen_source[i] <- sample(c("Blood - capillary", "Blood - venous"), 1, prob = c(.33, .67))
    tests$result_value[i] <- sample_bll_dist(1)
    tests$units[i] <- "\u03bcg/dL"
    while (!is.na(suppressWarnings(as.numeric(tests$result_value[i]))) && as.numeric(tests$result_value[i]) >= 3.5) {
      p <- stats::dnorm(1:61, mean = 30, sd = 10)
      date <- tests$collection_date[i] + sample(60:120, 1, prob = p)
      if (date > dmax) break
      if (tests$specimen_source[i] == "Blood - capillary" & i == 1) {
        source <- sample(c("Blood - capillary", "Blood - venous"), 1, prob = c(.33, .67))
      } else {
        source <- "Blood - venous"
      }
      x <- -20:5
      p <- stats::dnorm(1:length(x), mean = length(x) / 2, sd = 5)
      result <- as.numeric(tests$result_value[i]) + sample(x, 1, prob = p)
      if (result < 1) result <- "< 1"
      i <- i + 1
      tests$person_id[i] <- id
      tests$collection_date[i] <- date
      tests$specimen_source[i] <- source
      tests$result_value[i] <- as.character(result)
      tests$units[i] <- "\u03bcg/dL"
    }
    continue <- sample(c(TRUE, FALSE), 1, prob = c(.1, .9))
    if (continue) {
      dmin <- as.Date(tests$collection_date[i] + 180)
      if (dmin > dmax) break
      i <- i + 1
    }
  }
  tests
}

sample_bll_dist <- function(n) {
  vals1 <- c("< 1", "< 2", "< 3")
  p1 <- c(.401, .062, .099)
  vals2 <- 1:100
  p2 <- stats::dlnorm(1:length(vals2), meanlog = 1, sdlog = 1)
  p2 <- p2 * (1 - sum(p1))
  vals <- c(vals1, vals2)
  p <- c(p1, p2)
  sample(vals, n, TRUE, prob = p)
}
