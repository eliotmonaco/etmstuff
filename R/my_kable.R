#' Create a table using `kableExtra` formatting
#'
#' @description
#' This function returns a table formatted using the [kableExtra] package.
#'
#' @param df A dataframe.
#' @param caption A table caption.
#' @param row_names Logical: row names will be `rownames(df)` if `TRUE`.
#' @param bootstrap_opts A character vector of bootstrap options to pass to [kableExtra::kable_styling()]. Options are `basic`, `striped`, `bordered`, `hover`, `condensed`, `responsive`, and `none`.
#' @param scroll_box_ht A string for the number of pixels to use as the height of a scroll box containing the output table (e.g., `"600px"`.
#' @param format A value passed to the `format` argument in [kableExtra::kbl()] (see that function's documentation for options). Default is `"html"`.
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
my_kable <- function(df, caption = NULL, bootstrap_opts = NULL, row_names = FALSE, scroll_box_ht = NULL, format = "html") {
  if (!is.data.frame(df)) stop("`df` must be a dataframe")
  if (nrow(df) == 0) return(message("The dataframe has 0 rows"))

  # Table class name for CSS
  css_class <- "class='my-kable'"

  # Compose table
  df %>%
    kableExtra::kbl(
      format = format,
      # digits = 3,
      row.names = row_names,
      caption = caption,
      format.args = list(
        big.mark = ",",
        scientific = FALSE
      ),
      table.attr = css_class
    ) %>%
    kableExtra::kable_styling(
      bootstrap_options = bootstrap_opts,
      full_width = FALSE
    ) %>%
    kableExtra::scroll_box(height = scroll_box_ht)
}
