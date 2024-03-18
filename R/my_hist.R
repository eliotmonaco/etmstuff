#' Plot a histogram with `plotly`
#'
#' @param x A vector of numeric values.
#' @param bin_size The size of each bin.
#' @param x_label The x-axis label.
#' @param title The plot title.
#' @param bar_gap A number from 0 to 1. The gap between bars as a fraction of a full bar width.
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
