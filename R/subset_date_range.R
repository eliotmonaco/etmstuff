#' Subset a dataframe by a date variable
#'
#' Subset a dataframe by year(s), year-quarter(s), or dates. Supply the date variable `var` and `range`, a character vector of length 1-2 containing
#' * A single year (e.g., "2023") or year-quarter (e.g., "2023q1")
#' * A year and/or year-quarter pair (start & end)
#' * A pair (start & end) of dates (e.g., c("2022-02-02", "2023-03-03"))
#'
#' @param df A dataframe.
#' @param var A variable name from `df` containing dates.
#' @param range A character vector of length 1-2.
#'
#' @return A dataframe.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' df <- data.frame(
#'   date = sample(
#'     seq.Date(
#'       from = as.Date("2020-01-01"),
#'       to = as.Date("2022-12-31"),
#'       by = "day"
#'     ),
#'     size = 500
#'   )
#' )
#' df_new <- subset_date_range(df, var = "date", range = "2021q3")
#'
subset_date_range <- function(df, var, range) {
  var_check(df, var = var)

  continue_yq <- stringr::str_detect(range, "^\\d{4}$") |
    stringr::str_detect(range, "^\\d{4}[Qq]\\d$")
  continue_date <- stringr::str_detect(range, "^\\d{4}-\\d{2}-\\d{2}$")

  if (all(continue_yq) & length(continue_yq) %in% 1:2) {
    # Get beginning and ending year and quarter from `range`
    range_y <- as.numeric(stringr::str_extract(range, "^\\d{4}((?=[Qq])|$)"))
    range_q <- as.numeric(stringr::str_extract(range, "(?<=[Qq])\\d$"))
    y1 <- range_y[1]
    q1 <- range_q[1]
    y2 <- ifelse(!is.na(range_y[2]), range_y[2], range_y[1])
    q2 <- ifelse(!is.na(range_q[2]), range_q[2], range_q[1])

    # Turn quarter(s) into date(s)
    q_dates <- c("-01-01", "-04-01", "-07-01", "-10-01")

    if (is.na(q1)) {
      t1 <- paste0(y1, q_dates[1])
    } else if (q1 %in% 1:4) {
      t1 <- paste0(y1, q_dates[q1])
    } else {
      stop("The quarter must be a digit from 1 to 4")
    }

    if (is.na(q2) | q2 == 4) {
      t2 <- paste0(y2 + 1, q_dates[1])
    } else if (q2 %in% 1:3) {
      t2 <- paste0(y2, q_dates[q2 + 1])
    } else {
      stop("The quarter must be a digit from 1 to 4")
    }

    # Includes t1 but not t2
    df_subset <- df %>%
      dplyr::filter(.data[[var]] >= t1, .data[[var]] < t2)
  } else if (all(continue_date) &
    all(!is.na(as.Date(range, format = "%Y-%m-%d"))) &
    length(continue_date) == 2) {
    range_d <- as.Date(range, format = "%Y-%m-%d")
    t1 <- range_d[1]
    t2 <- range_d[2]

    # Includes both t1 and t2
    df_subset <- df %>%
      dplyr::filter(.data[[var]] >= t1, .data[[var]] <= t2)
  } else {
    stop(paste(
      "`range` must be a character vector of length 1-2 containing:\n",
      "- a single year (e.g., \"2023\") or year-quarter (e.g., \"2023q1\")\n",
      "- a year and/or year-quarter pair (start & end)\n",
      "- a pair (start & end) of dates (e.g., c(\"2022-02-02\", \"2023-03-03\"))"
    ))
  }

  if (nrow(df_subset) > 0) {
    m <- paste(
      "Subset data has range",
      min(df_subset[[var]]),
      "to", max(df_subset[[var]])
    )
  } else {
    m <- "No data found in that range"
  }

  message(m)

  df_subset
}
