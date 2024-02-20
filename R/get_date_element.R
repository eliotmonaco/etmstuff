#' Get year(s) or quarter(s) from a vector of dates
#'
#' @description
#' * `get_year()` finds the unique year(s) represented in a vector of dates.
#' * `get_quarter()` finds the unique quarter(s) represented in a vector of dates.
#'
#' @param dates A vector of dates or a vector of strings that can be formatted as dates.
#' @param format The date format, passed to [as.Date()]. Defaults to `"%Y-%m-%d"`.
#'
#' @return
#' * `get_year()` returns a numeric vector of years.
#' * `get_quarter()` returns a numeric vector of digits `1:4`.
#'
#' @examples
#' d <- seq.Date(
#'   from = as.Date("1980-10-01"),
#'   to = as.Date("1981-02-28"),
#'   by = "week"
#' )
#'
#' get_year(d)
#' get_quarter(d)
#'
#' @name get_date_element
NULL

#' @export
#' @rdname get_date_element
get_year <- function(dates, format = "%Y-%m-%d") {
  years <- as.numeric(stats::na.omit(unique(
    format(
      as.Date(dates, format = format),
      format = "%Y"
    )
  )))

  sort(years)
}

#' @export
#' @rdname get_date_element
get_quarter <- function(dates, format = "%Y-%m-%d") {
  y <- get_year(dates, format)

  if (length(y) > 1) message("`dates` represents multiple years")

  months <- as.numeric(stats::na.omit(unique(
    format(
      as.Date(dates, format = format),
      format = "%m"
    )
  )))

  q <- c(
    ifelse(any(1:3 %in% months), 1, NA),
    ifelse(any(4:6 %in% months), 2, NA),
    ifelse(any(7:9 %in% months), 3, NA),
    ifelse(any(10:12 %in% months), 4, NA)
  )

  q[which(!is.na(q))]
}
