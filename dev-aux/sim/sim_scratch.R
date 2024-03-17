


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



# sim_address() with option to create dirty addresses, and accessory functions

sim_address <- function(n, dirty = FALSE) {
  # Street component
  v_streets <- sim_street(n)

  # Unit component
  v_units <- sim_unit(round(n * .14))

  # City, state, zip, & county components
  df <- ks_city_zip[sample(1:nrow(ks_city_zip), n, TRUE),]

  df$street <- v_streets
  df$unit <- sample(c(v_units, rep(NA, n - length(v_units))), n, FALSE)
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





# sim_person_name ####

# `match` can be NULL, TRUE, or a variable name

sim_person_name <- function(df, first = NULL, middle = NULL, last = NULL, match = NULL) {
  vars <- c(first, middle, last)

  var_check(df, var = vars)

  # Subset `df` by matching variable
  if (is.logical(match) && match) {
    df_temp <- df %>%
      dplyr::distinct(dplyr::across(tidyselect::all_of(vars)))
  } else if (!is.null(match)) {
    var_check(df, var = match)
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
  var_check(df, var = var)

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
    var_check(df, var = match)
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
  var_check(df, var = var)

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
    var_check(df, var = match)
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
  var_check(df, var = var)

  # Keep unique values of `var`
  factors <- unique(df[[var]])

  # Subset `df` by matching variable
  if (is.logical(match) && match) {
    df_temp <- df %>%
      dplyr::distinct(dplyr::across(tidyselect::all_of(var)))
  } else if (!is.null(match)) {
    var_check(df, var = match)
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




