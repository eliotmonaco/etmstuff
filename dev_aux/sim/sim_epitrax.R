sim_epitrax <- function(rows) {

  df <- as.data.frame(matrix(
    nrow = rows,
    ncol = length(etmstuff:::epitrax_variables_reordered)
  ))

  colnames(df) <- etmstuff:::epitrax_variables_reordered

  df$patient_id <- as.character(sample(
    1000:9999999,
    size = rows,
    replace = FALSE
  ))

  df$patient_record_number <- as.character(sample(
    1000000000:999999999999,
    size = rows,
    replace = FALSE
  ))

  df$patient_birth_date <- sample(
    seq.Date(
      from = as.Date("1980-01-01"),
      to = as.Date("2022-12-31"),
      by = "day"
    ),
    size = rows,
    replace = TRUE
  )

  df$person_last_name <- sample(
    last_names$name,
    size = rows,
    replace = TRUE
  )

  df$person_first_name <- c(
    sample(
      first_names$female,
      size = rows / 2,
      replace = TRUE
    ),
    sample(
      first_names$male,
      size = rows / 2,
      replace = TRUE
    )
  )

  df$person_middle_name <- sample(
    LETTERS,
    size = rows,
    replace = TRUE
  )

  df$patient_birth_date <- sample(
    seq.Date(
      from = as.Date("1980-01-01"),
      to = as.Date("2023-01-01"),
      by = "day"
    ),
    size = rows,
    replace = TRUE
  )







  df$patient_birth_sex <- c(
    rep("Female", rows / 2),
    rep("Male", rows / 2)
  )






  df

}
