#' Add an age column to a dataframe
#'
#' Calculates age as (`d2` - `d1`) / 365.25. For EpiTrax data, `d1 = "patient_birth_date"` and `d2 = "lab_collection_date"`.
#'
#' @param df A dataframe.
#' @param d1 A variable name from `df` with the birth/start date for the age calculation.
#' @param d2 A variable name from `df` with the end date for the age calculation.
#'
#' @return A dataframe with a new variable, `df$age`.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' df <- data.frame(
#'   DOB = sample(seq.Date(as.Date("1970-01-01"), as.Date("1980-01-01"),
#'     by = "day"
#'   ), size = 10),
#'   event = sample(seq.Date(as.Date("1990-01-01"), as.Date("2020-01-01"),
#'     by = "day"
#'   ), size = 10)
#' )
#' df_new <- calculate_age(df, d1 = "DOB", d2 = "event")
#'
calculate_age <- function(df, d1, d2) {
  var_check(df, var = c(d1, d2))

  # Check if d1 & d2 are formatted as date
  is_date_d1 <- lubridate::is.Date(df[[d1]])
  is_date_d2 <- lubridate::is.Date(df[[d2]])

  if (!is_date_d1 | !is_date_d2) {
    s <- paste(
      stats::na.omit(c(
        ifelse(!is_date_d1, paste0("`", d1, "`"), NA),
        ifelse(!is_date_d2, paste0("`", d2, "`"), NA)
      )),
      collapse = ", "
    )
    m <- paste("Variable not formatted as date:", s)
    stop(m, call. = FALSE)
  }

  # Check for NA in d1 & d2
  df_check <- df %>%
    dplyr::mutate(
      d1_is_na = is.na(.data[[d1]]),
      d2_is_na = is.na(.data[[d2]])
    ) %>%
    dplyr::filter(d1_is_na | d2_is_na)

  # Calculate age
  df <- df %>%
    dplyr::mutate(age = as.numeric((.data[[d2]] - .data[[d1]]) / 365.25)) %>%
    dplyr::relocate(age, .after = {{ d1 }})

  if (nrow(df_check) == 0) {
    m <- "Age calculated for all records"
  } else {
    m <- paste("Date value is `NA` in at least one record")
    # m <- paste("Date variable contains NA in the following record(s):", paste(df_check$recno, collapse = ", "))
  }

  message(m)

  df
}
