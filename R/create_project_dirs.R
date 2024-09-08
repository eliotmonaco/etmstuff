#' Create project directories
#'
#' @param type The type of project structure to create. One of `c("analysis1", "analysis2", "ksde")`. All create three subdirectories: "data", "data/1-source", "output", and "scripts". The differences are that
#'
#' * `"analysis1"` also creates "data/2-aux" and "data/3-final".
#' * `"analysis2"` also creates "data/2-final".
#' * `"ksde"` also creates "data/2-aux", "data/3-final", "output/1-test", and "output/2-prod".
#'
#' @param root The project root directory. Defaults to `getwd()`.
#'
#' @return Returns the project root directory, invisibly.
#' @export
#'
# @examples
#'
create_project_dirs <- function(type = c("analysis1", "analysis2", "ksde"), root = getwd()) {
  if (length(type) != 1 || !type %in% c("analysis1", "analysis2", "ksde")) {
    stop("`type` must be one of c(\"analysis1\", \"analysis2\", \"ksde\")")
  }

  if (type == "analysis1") {
    dirs <- c(
      "data", "data/1-source", "data/2-aux", "data/3-final",
      "output",
      "scripts"
    )
  } else if (type == "analysis2") {
    dirs <- c(
      "data", "data/1-source", "data/2-final",
      "output",
      "scripts"
    )
  } else if (type == "ksde") {
    dirs <- c(
      "data", "data/1-source", "data/2-aux", "data/3-final",
      "output", "output/1-test", "output/2-prod",
      "scripts"
    )
  }

  dirs <- paste(root, dirs, sep = "/")

  lapply(dirs, dir.create)

  invisible(root)
}
