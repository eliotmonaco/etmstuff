#' Create project directories
#'
#' @param type The type of project structure to create. One of `c("analysis1", "analysis2", "ksde")`. All create three subdirectories: "data", "data/1_source", "output", and "scripts". The differences are that
#'
#' * `"analysis1"` also creates "data/2_aux" and "data/3_final".
#' * `"analysis2"` also creates "data/2_final".
#' * `"ksde"` also creates "data/2_aux", "data/3_final", "output/1_test", and "output/2_prod".
#'
#' @param root The project root directory. Defaults to `getwd()`.
#'
#' @return The project root directory, invisibly.
#' @export
#'
# @examples
#'
create_project_dirs <- function(type = c("analysis1", "analysis2", "ksde"), root = getwd()) {
  if (type == "analysis1") {
    dirs <- c(
      "data", "data/1_source", "data/2_aux", "data/3_final",
      "output",
      "scripts"
    )
  } else if (type == "analysis2") {
    dirs <- c(
      "data", "data/1_source", "data/2_final",
      "output",
      "scripts"
    )
  } else if (type == "ksde") {
    dirs <- c(
      "data", "data/1_source", "data/2_aux", "data/3_final",
      "output", "output/1_test", "output/2_prod",
      "scripts"
    )
  }

  dirs <- paste(root, dirs, sep = "/")

  purrr::map(dirs, dir.create)

  invisible(root)
}
