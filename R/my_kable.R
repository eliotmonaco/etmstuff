#' Apply custom `kable()` formatting
#'
#' @description
#' This function returns a table formatted using functions in the [kableExtra] package. Additional formatting can be added to the returned table, as in the example. The following colors are used:
#'
#' * Normal text: "#EFECE4"
#' * Emphasized text: "#FF801E"
#' * Header row background: "#0B1433"
#' * Body row background: "#434E77"
#' * Alternate row background: "#1D274F"
#'
#' @param df A dataframe.
#' @param row_names Logical: use `rownames(df)` if `TRUE`.
#' @param alt_row_bg Logical: alternate row background color if `TRUE`.
#' @param caption A table caption (optional).
#' @param bootstrap_opts Bootstrap options to pass to [kableExtra::kable_styling()].
#'
#' @return A table formatted using functions in the [kableExtra] package.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' df <- mtcars[1:10,]
#'
#' tbl <- my_kable(
#'   df,
#'   row_names = TRUE,
#'   alt_row_bg = TRUE,
#'   caption = "Check out my sweeeeeet table!",
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
my_kable <- function(df, row_names = FALSE, alt_row_bg = FALSE, caption = NULL, bootstrap_opts = NULL) {
  if (nrow(df) == 0) return(message("The dataframe has 0 rows"))

  # Table class name for CSS
  html_attr <- "class='my-kable'"

  # Cell background colors
  bg_dark <- "#0B1433"; bg_light <- "#434E77"; bg_med <- "#1D274F"

  # Text colors
  txt_clr <- "#EFECE4"; txt_emph <- "#FF801E"

  # Compose table
  tbl <- df %>%
    kableExtra::kbl(
      row.names = row_names,
      table.attr = html_attr,
      digits = 3,
      format.args = list(
        big.mark = ",",
        scientific = FALSE
      ),
      caption = caption
    ) %>%
    kableExtra::kable_styling(
      bootstrap_options = bootstrap_opts,
      full_width = FALSE
    ) %>%
    # Header row
    kableExtra::row_spec(
      row = 0,
      color = txt_clr,
      background = bg_dark
    ) %>%
    # Body rows
    kableExtra::row_spec(
      row = 1:nrow(df),
      color = txt_clr,
      background = bg_light
    )

  # Alternate row background colors
  if (alt_row_bg) {
    n <- 1:nrow(df)
    n <- n[which(n %% 2 == 0)]
    tbl <- tbl %>%
      kableExtra::row_spec(
        row = n,
        background = bg_med
      )
  }

  tbl
}
