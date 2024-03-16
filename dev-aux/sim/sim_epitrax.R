
library(tidyverse)


# Simulated clean lead data set
# Create unique people
# Give them multiple realistic records, with plausible test sequences


vars <- etmstuff::epitrax_vars$epht_name



sim_person <- function(n) {
  df_names <- tibble::tibble(
    first_name = c(names$first_male, names$first_female),
    birth_sex = c(rep("Male", length(names$first_male)), rep("Female", length(names$first_female)))
  )
  df <- df_names[sample(1:nrow(df_names), n, TRUE),] |>
    dplyr::mutate(
      last_name = sample(names$last, n, TRUE),
      middle_name = sample(LETTERS, n, TRUE),
      person_id = as.character(100000:(100000 + n - 1)),
      patient_record_number = as.character(2000000000:(2000000000 + n - 1)),
      birth_date = sample(seq.Date(Sys.Date() - 100 * 365, Sys.Date(), "day"), n, TRUE)
    ) |>
    dplyr::select(person_id, patient_record_number, last_name, first_name, middle_name, birth_date, birth_sex)
  df_addr <- sim_address(n)
  colnames(df_addr) <- paste0("coll_add_", colnames(df_addr))
  df |>
    dplyr::bind_cols(df_addr)
}

# df <- sim_person(100)

sim_lead_test <- function(id, dob, dmin, dmax) {
  dmin <- max(dob, dmin)
  test <- list()
  test$person_id <- id
  test$collection_date <- sample(seq.Date(dmin, dmax, "day"), 1)
  test$specimen_source <- sample(c("Blood - capillary", "Blood - venous"), 1)
  vals <- c("< 1.0", "< 2.0", "< 3.0", "< 3.3", seq(3.3, 65, by = 0.1))
  p <- stats::dlnorm(1:length(vals), meanlog = 1.5, sdlog = 1)
  test$result_value <- sample(vals, 1, prob = p)
  test$units <- "\u03bcg/dL"
  test
}

# debugonce(sim_lead_test)
# sim_lead_test(df$person_id[1], df$birth_date[1], as.Date("2020-01-01"), Sys.Date())

sim_lead_data <- function(n, dmin, dmax = Sys.Date()) {
  df <- sim_person(100)
  tests <- lapply(
    df$person_id, FUN = sim_lead_test,
    dob = df$birth_date, dmin = dmin, dmax = dmax
  )
  tests <- lapply(tests, tibble::as_tibble)
  tests <- purrr::list_rbind(tests)
  df <- df |>
    dplyr::left_join(tests, by = "person_id")
  # browser()
  cols <- epitrax_vars$epht_name[epitrax_vars$epht_name %in% colnames(df)]
  df |>
    dplyr::select(tidyselect::all_of(cols))
}

# debugonce(sim_lead_data)
df <- sim_lead_data(100, as.Date("2020-01-01"))











# First names
# https://www.ssa.gov/oact/babynames/decades/century.html

# Last names
# https://www.census.gov/topics/population/genealogy/data/2010_surnames.html





epitrax_data <- readRDS("../bl_2023q2/data/final/data_core_2023q2.rds")



# sim_person_name ####

# `match` can be NULL, TRUE, or a variable name

sim_person_name <- function(df, first = NULL, middle = NULL, last = NULL, match = NULL) {
  vars <- c(first, middle, last)

  etmstuff::var_check(df, var = vars)

  # Subset `df` by matching variable
  if (is.logical(match) && match) {
    df_temp <- df %>%
      dplyr::distinct(dplyr::across(tidyselect::all_of(vars)))
  } else if (!is.null(match)) {
    etmstuff::var_check(df, var = match)
    df_temp <- df %>%
      dplyr::distinct(dplyr::across(tidyselect::all_of(match)))
  } else {
    df <- df %>%
      dplyr::mutate(sim_temp_id = dplyr::row_number())
    df_temp <- df %>%
      dplyr::distinct(sim_temp_id)
  }

  # Sample from `first_names`, `LETTERS`, and `last_names` to create substitute names
  if (!is.null(first)) {
    df_temp <- df_temp %>%
      dplyr::mutate(new_first = sample(
        x = c(first_names$male, first_names$female),
        size = nrow(.),
        replace = TRUE
      ))
  }

  if (!is.null(middle)) {
    df_temp <- df_temp %>%
      dplyr::mutate(new_middle = sample(
        x = LETTERS,
        size = nrow(.),
        replace = TRUE
      ))
  }

  if (!is.null(last)) {
    df_temp <- df_temp %>%
      dplyr::mutate(new_last = sample(
        x = last_names$name,
        size = nrow(.),
        replace = TRUE
      ))
  }

  # Join new names to `df` by matching variable
  if (is.logical(match) && match) {
    df <- df %>%
      dplyr::left_join(df_temp, by = vars)
  } else if (!is.null(match)) {
    df <- df %>%
      dplyr::left_join(df_temp, by = match)
  } else {
    df <- df %>%
      dplyr::left_join(df_temp, by = "sim_temp_id") %>%
      dplyr::select(-sim_temp_id)
  }

  new_vars <- c()

  # Substitute new names for old
  if (!is.null(first)) {
    df <- df %>%
      dplyr::mutate({{ first }} := new_first)
    new_vars <- c(new_vars, "new_first")
  }

  if (!is.null(middle)) {
    df <- df %>%
      dplyr::mutate({{ middle }} := new_middle)
    new_vars <- c(new_vars, "new_middle")
  }

  if (!is.null(last)) {
    df <- df %>%
      dplyr::mutate({{ last }} := new_last)
    new_vars <- c(new_vars, "new_last")
  }

  df %>%
    dplyr::select(-tidyselect::all_of(new_vars))
}

debugonce(sim_person_name)

df <- sim_person_name(
  epitrax_data,
  first = "person_first_name",
  middle = "person_middle_name",
  last = "person_last_name",
  match = "dupe_id"
)



# sim_number ####

sim_number <- function(df, var, min = NULL, max = NULL, match = NULL) {
  etmstuff::var_check(df, var = var)

  # Determine min. & max. values for the sample sequence
  if (is.null(min)) {
    min <- signif(min(as.numeric(df[[var]]), na.rm = TRUE), digits = 1)
  }
  if (is.null(max)) {
    max <- signif(max(as.numeric(df[[var]]), na.rm = TRUE), digits = 1) - 1
  }

  # Determine increment for the sample sequence (based on number of digits after decimal, if present)
  x <- as.character(df[[var]])
  x <- x[which(stringr::str_detect(x, "\\."))]
  if (length(x) > 0) {
    x <- max(nchar(stringr::str_extract(x, "(?<=\\.).*")), na.rm = TRUE)
    if (x > 5) {
      incr <- 0.00001
    } else {
      incr <- as.numeric(paste0("0.", paste0(rep("0", times = x - 1), collapse = ""), "1"))
    }
  } else {
    incr <- 1
  }

  # Subset `df` by matching variable
  if (is.logical(match) && match) {
    df_temp <- df %>%
      dplyr::distinct(dplyr::across(tidyselect::all_of(var)))
  } else if (!is.null(match)) {
    etmstuff::var_check(df, var = match)
    df_temp <- df %>%
      dplyr::distinct(dplyr::across(tidyselect::all_of(match)))
  } else {
    df <- df %>%
      dplyr::mutate(sim_temp_id = dplyr::row_number())
    df_temp <- df %>%
      dplyr::distinct(sim_temp_id)
  }
browser()
  # Sample from `min:max` to create substitute numeric values
  df_temp <- df_temp %>%
    dplyr::mutate(new_temp_var = sample(
      x = seq(min, max, by = incr),
      size = nrow(.),
      replace = TRUE
    ))

  # Join new numbers to `df` by matching variable
  if (is.logical(match) && match) {
    df %>%
      dplyr::left_join(df_temp, by = var) %>%
      dplyr::mutate({{ var }} := new_temp_var) %>%
      dplyr::select(-new_temp_var)
  } else if (!is.null(match)) {
    df %>%
      dplyr::left_join(df_temp, by = match) %>%
      dplyr::mutate({{ var }} := new_temp_var) %>%
      dplyr::select(-new_temp_var)
  } else {
    df %>%
      dplyr::left_join(df_temp, by = "sim_temp_id") %>%
      dplyr::mutate({{ var }} := new_temp_var) %>%
      dplyr::select(-new_temp_var, -sim_temp_id)
  }
}

debugonce(sim_number)

df <- sim_number(epitrax_data, var = "lab_result_number", max = 105)

## Add: preserve frequency distribution of original data?



# sim_date ####

sim_date <- function(df, var, match = NULL) {
  etmstuff::var_check(df, var = var)

  # Get min. & max. dates for the sample sequence
  min <- min(df[[var]], na.rm = TRUE)
  max <- max(df[[var]], na.rm = TRUE)

  # Expand `min` & `max` based on `rng`
  rng <- as.numeric(max - min)
  if (rng > 7 & rng <= 184) {
    min <- lubridate::floor_date(min, "month")
    max <- lubridate::ceiling_date(max, "month") - 1
  } else if (rng > 184) {
    min <- lubridate::floor_date(min, "year")
    max <- lubridate::ceiling_date(max, "year") - 1
  }

  # If `max` exceeds current date, reset `max` to current date - 1
  if (max > Sys.Date()) {
    max <- Sys.Date() - 1
  }

  # Subset `df` by matching variable
  if (is.logical(match) && match) {
    df_temp <- df %>%
      dplyr::distinct(dplyr::across(tidyselect::all_of(var)))
  } else if (!is.null(match)) {
    etmstuff::var_check(df, var = match)
    df_temp <- df %>%
      dplyr::distinct(dplyr::across(tidyselect::all_of(match)))
  } else {
    df <- df %>%
      dplyr::mutate(sim_temp_id = dplyr::row_number())
    df_temp <- df %>%
      dplyr::distinct(sim_temp_id)
  }

  # Sample from `min:max` to create substitute dates
  df_temp <- df_temp %>%
    dplyr::mutate(new_temp_var = sample(
      x = seq.Date(from = min, to = max, by = "day"),
      size = nrow(.),
      replace = TRUE
    ))

  # Join new dates to `df` by matching variable
  if (is.logical(match) && match) {
    df %>%
      dplyr::left_join(df_temp, by = var) %>%
      dplyr::mutate({{ var }} := new_temp_var) %>%
      dplyr::select(-new_temp_var)
  } else if (!is.null(match)) {
    df %>%
      dplyr::left_join(df_temp, by = match) %>%
      dplyr::mutate({{ var }} := new_temp_var) %>%
      dplyr::select(-new_temp_var)
  } else {
    df %>%
      dplyr::left_join(df_temp, by = "sim_temp_id") %>%
      dplyr::mutate({{ var }} := new_temp_var) %>%
      dplyr::select(-new_temp_var, -sim_temp_id)
  }
}

debugonce(sim_date)

df <- sim_date(epitrax_data, var = "patient_birth_date")

## Add: date sequencing between variables



# sim_factor ####

sim_factor <- function(df, var, match = NULL) {
  etmstuff::var_check(df, var = var)

  # Keep unique values of `var`
  factors <- unique(df[[var]])

  # Subset `df` by matching variable
  if (is.logical(match) && match) {
    df_temp <- df %>%
      dplyr::distinct(dplyr::across(tidyselect::all_of(var)))
  } else if (!is.null(match)) {
    etmstuff::var_check(df, var = match)
    df_temp <- df %>%
      dplyr::distinct(dplyr::across(tidyselect::all_of(match)))
  } else {
    df <- df %>%
      dplyr::mutate(sim_temp_id = dplyr::row_number())
    df_temp <- df %>%
      dplyr::distinct(sim_temp_id)
  }

  # Sample from `factors` to create substitute data
  df_temp <- df_temp %>%
    dplyr::mutate(new_temp_var = sample(
      x = factors,
      size = nrow(.),
      replace = TRUE
    ))

  # Join new factors to `df` by matching variable
  if (is.logical(match) && match) {
    df %>%
      dplyr::left_join(df_temp, by = var) %>%
      dplyr::mutate({{ var }} := new_temp_var) %>%
      dplyr::select(-new_temp_var)
  } else if (!is.null(match)) {
    df %>%
      dplyr::left_join(df_temp, by = match) %>%
      dplyr::mutate({{ var }} := new_temp_var) %>%
      dplyr::select(-new_temp_var)
  } else {
    df %>%
      dplyr::left_join(df_temp, by = "sim_temp_id") %>%
      dplyr::mutate({{ var }} := new_temp_var) %>%
      dplyr::select(-new_temp_var, -sim_temp_id)
  }
}

debugonce(sim_factor)

df <- sim_factor(epitrax_data, var = "lab_specimen_source")




