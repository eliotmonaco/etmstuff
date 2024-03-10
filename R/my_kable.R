#' Create a table using `kableExtra` formatting
#'
#' @description
#' This function returns a table formatted using the [kableExtra] package.
#'
#' @param df A dataframe.
#' @param bootstrap_opts A character vector of bootstrap options to pass to [kableExtra::kable_styling()]. Options are `basic`, `striped`, `bordered`, `hover`, `condensed`, `responsive`, and `none`.
#' @param scroll_h A string for the height in pixels of a scroll box to contain the table (e.g., `"600px"`).
#' @param scroll_w A string for the width in pixels of a scroll box to contain the table (e.g., `"600px"`).
#' @param format A value passed to the `format` argument in [kableExtra::kbl()] (see that function's documentation for options). Default is `"html"`.
#' @param css CSS styling for HTML output.
#' @param ... Additional arguments passed to [kableExtra::kbl()].
#'
#' @return None.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' df <- mtcars[1:10,]
#'
#' tbl <- my_kable(df,
#'   caption = "Check out my sweeeeeet table!",
#'   row_names = TRUE,
#'   bootstrap_opts = "bordered"
#' )
#'
#' \dontrun{
#' # Apply conditional formatting to text in `mtcars$hp`
#' tbl %>%
#'   kableExtra::column_spec(
#'     column = which(colnames(df) == "hp") + 1,
#'     color = ifelse(df$hp > 100, "#FF801E", "#EFECE4")
#'   )
#' }
#'
my_kable <- function(df, bootstrap_opts = NULL, scroll_h = NULL, scroll_w = NULL, format = "html", css = "class='my-kable'", ...) {
  if (!is.data.frame(df)) stop("`df` must be a dataframe")
  if (nrow(df) == 0) return(message("The dataframe has 0 rows"))

  # Compose table
  df %>%
    kableExtra::kbl(
      format = format,
      format.args = list(
        big.mark = ",",
        scientific = FALSE
      ),
      table.attr = css,
      ...
    ) %>%
    kableExtra::kable_styling(
      bootstrap_options = bootstrap_opts,
      full_width = FALSE
    ) %>%
    kableExtra::scroll_box(height = scroll_h, width = scroll_w)
}
