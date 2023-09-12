sim_epitrax <- function(n_rows) {

  df <- as.data.frame(matrix(
    nrow = n_rows,
    ncol = length(etmstuff:::epitrax_vars_reordered)
  ))

  colnames(df) <- etmstuff:::epitrax_vars_reordered

  df$patient_id <- as.character(sample(
    1000:9999999,
    size = n_rows,
    replace = FALSE
  ))

  df$patient_record_number <- as.character(sample(
    1000000000:999999999999,
    size = n_rows,
    replace = FALSE
  ))

  df$patient_birth_date <- sample(
    seq.Date(
      from = as.Date("1980-01-01"),
      to = as.Date("2022-12-31"),
      by = "day"
    ),
    size = n_rows,
    replace = TRUE
  )

  df$person_last_name <- sample(
    last_names$name,
    size = n_rows,
    replace = TRUE
  )

  df$person_first_name <- c(
    sample(
      first_names$female,
      size = n_rows / 2,
      replace = TRUE
    ),
    sample(
      first_names$male,
      size = n_rows / 2,
      replace = TRUE
    )
  )

  df$person_middle_name <- sample(
    LETTERS,
    size = n_rows,
    replace = TRUE
  )

  df$lab_collection_date <- sample(
    seq.Date(
      from = as.Date("2023-01-01"),
      to = as.Date("2023-09-01"),
      by = "day"
    ),
    size = n_rows,
    replace = TRUE
  )

  df$lab_result_value <- as.character(sample(
    0:65,
    size = n_rows,
    replace = FALSE
  ))







  df$patient_birth_sex <- c(
    rep("Female", n_rows / 2),
    rep("Male", n_rows / 2)
  )






  df

}




library(tidyverse)

epitrax_data <- readRDS("../bl_2023q2/data/final/data_core_2023q2.rds")



# Names ####

first_names <- readRDS("dev_aux/sim/first_names.rds")
last_names <- readRDS("dev_aux/sim/last_names.rds")

sim_person_name <- function(df, first = NULL, middle = NULL, last = NULL) {
  vars <- c(first, middle, last)

  df_names <- df %>%
    dplyr::distinct(dplyr::across(tidyselect::all_of(vars)))

  if (!is.null(first)) {
    df_names <- df_names %>%
      dplyr::mutate(
        new_first = sample(
          c(first_names$male, first_names$female),
          size = nrow(.),
          replace = TRUE
        )
      )
  }

  if (!is.null(middle)) {
    df_names <- df_names %>%
      dplyr::mutate(
        new_middle = sample(
          LETTERS,
          size = nrow(.),
          replace = TRUE
        )
      )
  }

  if (!is.null(last)) {
    df_names <- df_names %>%
      dplyr::mutate(
        new_last = sample(
          last_names$name,
          size = nrow(.),
          replace = TRUE
        )
      )
  }

  df <- df %>%
    dplyr::left_join(df_names, by = vars)

  new_vars <- c()

  if (!is.null(first)) {
    df <- df %>%
      dplyr::mutate(person_first_name = new_first)
    new_vars <- c(new_vars, "new_first")
  }

  if (!is.null(middle)) {
    df <- df %>%
      dplyr::mutate(person_middle_name = new_middle)
    new_vars <- c(new_vars, "new_middle")
  }

  if (!is.null(last)) {
    df <- df %>%
      dplyr::mutate(person_last_name = new_last)
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
  last = "person_last_name"
)



# Numeric ####

sim_numeric <- function(df, var) {
  df_num <- df %>%
    dplyr::distinct(dplyr::across(tidyselect::all_of(var))) %>%
    dplyr::mutate(new_num = as.numeric(.data[[var]]))

  min <- signif(min(df_num$new_num, na.rm = TRUE), digits = 1)
  max <- signif(max(df_num$new_num, na.rm = TRUE), digits = 1)

  df_num <- df_num %>%
    dplyr::mutate(new_num = sample(
      min:max,
      size = nrow(.),
      replace = TRUE
    ))

  browser()
}

debugonce(sim_numeric)

df <- sim_numeric(epitrax_data, var = "patient_id")









