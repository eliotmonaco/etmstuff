#' Subset a dataframe by a date variable
#'
#' @description
#' Subset a dataframe by one or two or strings representing years, year-quarters, or full dates.
#'
#' @details
#' Accepted values for `range1` and `range2` include 4-character years (e.g., "2019"), 6-character year-quarters (e.g., "2019q4"), or 10-character dates (e.g., "2019-10-25"). If `range2 == NULL` (the default), `df` will be subset by the full date range represented by `range1`. If `range2 != NULL`, `df` will be subset from the earliest date represented by `range1` to the latest date represented by `range2`. Note that if both `range1` and `range2` are provided, they must be either 1) both years or year-quarters or 2) both dates.
#'
#' @param df A dataframe.
#' @param var The date variable by which to subset `df`.
#' @param range1,range2 A string representing a range as a year, year-quarter, or date (YYYY-MM-dd).
#' @param silent Logical: silence message output if `TRUE`.
#'
#' @return A dataframe.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' dates <- seq.Date(as.Date("2020-01-01"), as.Date("2022-12-31"), by = "day")
#'
#' df <- data.frame(
#'   date = dates,
#'   number = as.numeric(dates),
#'   character = as.character(dates)
#' )
#'
#' dfa <- subset_dates(df, "date", "2021")
#' dfb <- subset_dates(df, "date", "2022q3")
#' dfc <- subset_dates(df, "date", "2021q4", "2022q1")
#' dfx <- subset_dates(df, "date", "2022-01-17", "2022-10-25")
#'
subset_dates <- function(df, var, range1, range2 = NULL, silent = FALSE) {
  if (length(var) != 1) stop("`var` must have length of 1")
  if (!lubridate::is.Date(df[[var]])) stop("`df$var` must be formatted as a date")
  if (length(range1) != 1 | length(range2) > 1) stop("`range1` and `range2` cannot have length > 1")

  var_check(df, var = var)

  # Determine if range args are valid
  valid_yq <- stringr::str_detect(c(range1, range2), "^\\d{4}$|^\\d{4}[Qq][1-4]$")
  dt <- as.Date(c(range1, range2), format = "%Y-%m-%d")
  valid_date <- !is.na(dt)

  if (all(valid_yq)) {
    # Parse year(s) and quarter(s)
    yr <- as.numeric(stringr::str_extract(c(range1, range2), "^\\d{4}"))
    qtr <- as.numeric(stringr::str_extract(c(range1, range2), "(?<=[Qq])\\d$"))

    if (is.null(range2)) {
      yr[2] <- yr[1]
      qtr[2] <- qtr[1]
    }

    # Turn quarter(s) into date(s)
    qtr_start <- c("-01-01", "-04-01", "-07-01", "-10-01")

    if (is.na(qtr[1])) {
      dt[1] <- as.Date(paste0(yr[1], qtr_start[1]), format = "%Y-%m-%d")
    } else {
      dt[1] <- as.Date(paste0(yr[1], qtr_start[qtr[1]]), format = "%Y-%m-%d")
    }

    if (is.na(qtr[2]) | qtr[2] == 4) {
      dt[2] <- as.Date(paste0(yr[2] + 1, qtr_start[1]), format = "%Y-%m-%d")
    } else {
      dt[2] <- as.Date(paste0(yr[2], qtr_start[qtr[2] + 1]), format = "%Y-%m-%d")
    }

    df_subset <- df %>%
      dplyr::filter(.data[[var]] >= dt[1], .data[[var]] < dt[2])
  } else if (all(valid_date)) {
    if (is.null(range2)) dt[2] <- dt[1]

    df_subset <- df %>%
      dplyr::filter(.data[[var]] >= dt[1], .data[[var]] <= dt[2])
  } else {
    stop("Invalid `range` value(s)")
  }

  if (!silent & nrow(df_subset) > 0) {
    message(paste(
      "Subset data has range",
      min(df_subset[[var]]),
      "to", max(df_subset[[var]])
    ))
  } else if (!silent) {
    message("No data found in that range")
  }

  df_subset
}
