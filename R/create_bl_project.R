#' Create blood lead project and directories
#'
#' This function creates an R project and all directories for a blood lead data processing project. If `parent_dir` is not found or if a project directory with the same name already exists, the function will error out before creating anything. The R project is created using [usethis::create_project()].
#'
#' @param data_range The date range of the lead data that will be processed.
#'
#' * For one quarter, use the format YYYYqQ (e.g., "2023q1")
#' * For one year, use the format YYYY (e.g., "2023")
#' * For a range that spans multiple years, use the format YYYY-YYYY (e.g., "2015-2022")
#'
#' @param parent_dir The path to the directory in which the project will reside.
#'
#' @return A message indicating success or failure in creating all directories.
#' @export
#'
#' @examples
#' \dontrun{
#' create_bl_project(
#'   parent_dir = "~/my_projects/",
#'   data_range = "2023q1"
#' )
#' }
#'
create_bl_project <- function(data_range, parent_dir = "C:/Users/eliot.monaco/OneDrive - State of Kansas, OITS/Documents/r_projects") {
  if (!dir.exists(parent_dir)) {
    stop("`parent_dir` not found")
  }

  # Add "/" to end of `parent_dir` if it's not already the final character
  if (substr(parent_dir, nchar(parent_dir), nchar(parent_dir)) != "/") {
    parent_dir <- paste0(parent_dir, "/")
  }

  project_dir <- paste0(parent_dir, "bl_", data_range, "/")

  # Check if `project_dir` already exists
  if (dir.exists(project_dir)) {
    m <- paste0("Directory already exists:\n", project_dir)
    stop(m)
  }

  usethis::create_project(path = project_dir)

  # Remove the automatically created "R" folder from the project
  unlink(paste0(project_dir, "R"), recursive = TRUE)

  dir <- c(
    "data",
    "data/addresses",
    "data/cbls",
    "data/epitrax",
    "data/final",
    "data/helpers",
    "data/intermediate",
    "data/registries",
    "output",
    "output/addresses",
    "output/cbls",
    "output/epitrax",
    "scripts"
  )

  inv <- list()

  # Create subdirectories
  for (i in 1:length(dir)) {
    inv[[i]] <- (dir.create(paste0(project_dir, dir[i])))
  }

  if (all(dir.exists(project_dir), unlist(inv))) {
    message("Project directory and all subdirectories successfully created")
  } else {
    message("One or more directories failed")
  }
}
