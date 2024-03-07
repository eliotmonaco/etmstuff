#' Plot a histogram with `plotly`
#'
#' @param x A vector of numeric values.
#' @param bin_size The size of each bin.
#' @param x_label The x-axis label.
#' @param title The plot title.
#' @param bar_gap A number from 0 to 1. The gap between bars as a fraction of the plot.
#'
#' @return None.
#' @export
#'
#' @examples
#' x <- rnorm(100)
#'
#' \dontrun{
#' my_hist(x)
#' }
#'
my_hist <- function(x, bin_size = 1, x_label = NULL, title = NULL, bar_gap = NULL) {
  if (!is.numeric(x)) stop("`x` must be numeric")
  if (length(x_label) > 1 | length(bin_size) != 1 | length(title) > 1) {
    stop("`x_label`, `bin_size`, and `title` must have length of 1")
  }
  if (!is.numeric(bin_size) || length(bin_size) != 1 || bin_size != round(bin_size)) {
    stop("`bin_size` must be an integer")
  }
  # if (!is.null(bar_gap) && !isnumeric(bar_gap)) stop("`bar_gap`")

  plotly::plot_ly(
    x = x,
    type = "histogram",
    xbins = list(size = bin_size)
  ) %>%
    plotly::layout(
      title = title,
      xaxis = list(
        title = x_label
      ),
      yaxis = list(
        title = "Frequency"
      ),
      bargap = bar_gap,
      margin = list(t = 100, b = 70)
    )
}
