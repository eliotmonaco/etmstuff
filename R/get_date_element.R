#' Get year(s) or quarter(s) from a vector of dates
#'
#' @description
#' * `get_year()` returns the unique year(s) represented in a vector of dates.
#' * `get_quarter()` returns the unique quarter(s) represented in a vector of dates.
#'
#' @param dates A vector of dates, or a vector of strings that can be formatted as dates.
#'
#' @return
#' * `get_year()` returns a numeric vector of years.
#' * `get_quarter()` returns a numeric vector of digits `1:4`.
#'
#' @examples
#' d <- seq.Date(from = as.Date("1980-10-01"), to = as.Date("1981-02-28"), by = "week")
#' get_year(d)
#' get_quarter(d)
#' @name get_date_element
NULL

#' @export
#' @rdname get_date_element
get_year <- function(dates) {
  yr <- as.numeric(
    stats::na.omit(
      unique(
        format(
          as.Date(
            dates,
            tryFormats = c("%Y-%m-%d", "%Y%m%d")
          ),
          format = "%Y"
        )
      )
    )
  )

  sort(yr)
}

#' @export
#' @rdname get_date_element
get_quarter <- function(dates) {
  y <- get_year(dates)

  if (length(y) > 1) message("`dates` represents multiple years")

  month <- as.numeric(
    stats::na.omit(
      unique(
        format(
          as.Date(
            dates,
            tryFormats = c("%Y-%m-%d", "%Y%m%d")
          ),
          format = "%m"
        )
      )
    )
  )

  q <- c()

  q[1] <- ifelse(any(1:3 %in% month), 1, 0)
  q[2] <- ifelse(any(4:6 %in% month), 2, 0)
  q[3] <- ifelse(any(7:9 %in% month), 3, 0)
  q[4] <- ifelse(any(10:12 %in% month), 4, 0)

  q[which(q != 0)]
}
