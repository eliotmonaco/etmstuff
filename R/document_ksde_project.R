#' Document a KS Tracking Data Explorer project
#'
#' @param proj_dir The name of the RStudio project to document.
#' @param proj_type The project type.
#'
#' * `"rabies"` = A Rabies Surveillance display data project.
#' * `"geo"` = A core geography tables project.
#' * `"mw"` = A message widget table project.
#' * `"version"` = A version table project.
#'
#' @param doc_mode The part of the project to document.
#'
#' * `"code"` = Document all parts of the project necessary to reproduce the output, including the project structure, source data, and scripts.
#' * `"output"` = Document the project output only (CSV and XML files).
#'
#' @param overwrite If the destination directory for the documented project already exists, it will be removed when `overwrite` is set to `TRUE`.
#' @param test_drive Replace the destination path with the desktop ("C:/Users/eliot.monaco/OneDrive - State of Kansas, OITS/Desktop/") if `TRUE`.
#'
#' @return A list the same length as the number of destination directories into which project documentation was copied indicating success (`TRUE`) or failure (`FALSE`), invisibly.
#' @export
#'
# @examples
#'
document_ksde_project <- function(proj_dir, proj_type = c("rabies", "geo", "mw", "version"), doc_mode = c("code", "output"), overwrite = FALSE, test_drive = FALSE) {
  if (length(proj_type) != 1 || !proj_type %in% c("rabies", "geo", "mw", "version")) {
    stop("`proj_type` must be one of c(\"rabies\", \"geo\", \"mw\", \"version\")")
  }
  if (length(doc_mode) != 1 || !doc_mode %in% c("code", "output")) {
    stop("`doc_mode` must be one of c(\"code\", \"output\")")
  }

  default_dir <- "C:/Users/eliot.monaco/OneDrive - State of Kansas, OITS/Documents/r_projects/"

  if (proj_type == "rabies") {
    parent_dir <- "KS_Tracking_Data_Explorer/Rabies_Surveillance/"
    proj_path = paste0(default_dir, parent_dir, proj_dir)
  } else if (proj_type == "geo" | proj_type == "mw") {
    parent_dir <- "KS_Tracking_Data_Explorer/Geography_Tables/"
    proj_path = paste0(default_dir, parent_dir, proj_dir)
  } else if (proj_type == "version") {
    parent_dir <- "KS_Tracking_Data_Explorer/Version_Table/"
    proj_path = paste0(default_dir, parent_dir, proj_dir)
  }

  proj_info <- fn_project_info(proj_type)

  if (doc_mode == "code" & proj_type != "geo") {
    dest_path <- purrr::map(proj_info$dest_path, paste0, "02_Code/")
  } else if (doc_mode == "output" & proj_type != "geo") {
    dest_path <- purrr::map(proj_info$dest_path, paste0, "01_Data/")
  } else if (proj_type == "geo") {
    dest_path <- proj_info$dest_path
  }

  if (test_drive) {
    # Overwrite `dest_path` to save projects to desktop for testing
    test_dir <- "C:/Users/eliot.monaco/OneDrive - State of Kansas, OITS/Desktop/"
    dest_path <- purrr::map(dest_path, stringr::str_replace, pattern = ".+", replacement = test_dir)
  }

  # Check if project path exists
  if (!dir.exists(proj_path)) {
    stop(paste0("Project path not found: '", proj_path, "'"))
  }

  # Check if destination path exists
  if (!all(unlist(purrr::map(dest_path, dir.exists)))) {
    stop(paste0(
      "One or more destination paths not found:\n",
      paste(unlist(dest_path), collapse = "\n")
    ))
  }

  project_doc <- readRDS(paste0(proj_path, proj_info$project_doc_path))

  if (doc_mode == "code") {
    # Check if scripts in `project_doc` exist
    scripts <- sort(unique(unlist(purrr::map(project_doc, purrr::pluck, "script"))))
    scripts_path <- paste0(proj_path, "/scripts/", scripts)
    if (!all(file.exists(scripts_path))) {
      stop(paste0(
        "One or more scripts not found: ",
        paste(scripts, collapse = ", ")
      ))
    }

    # Create destination directory name(s)
    dest_dir <- purrr::map(project_doc, purrr::pluck, "proj_name")

    if (length(dest_dir) != length(dest_path)) {
      stop("Length mismatch: `dest_dir` & `dest_path`")
    }

    patterns <- fn_patterns()

    # Select directories to copy
    dirs_all <- list.dirs(proj_path, full.names = TRUE, recursive = TRUE)
    dirs_all <- dirs_all[!stringr::str_detect(dirs_all, patterns$p1)]

    # Select files to copy
    files_all <- list.files(proj_path, all.files = TRUE, full.names = TRUE, recursive = TRUE)
    files_all <- files_all[!stringr::str_detect(files_all, patterns$p2)]

    if (!any(stringr::str_detect(files_all, "/renv.lock$"))) {
      stop("No renv.lock file present")
    }
  } else if (doc_mode == "output") {
    # Assign destination directories
    dest_dir <- dest_path

    # Get project output paths
    proj_output_path <- paste0(proj_path, purrr::map(project_doc, purrr::pluck, "dir_output"))
  }

  items_copied <- list()

  for (i in 1:length(dest_dir)) {
    if (proj_type == "geo") {
      # Geography table data files and code are stored in a version folder created here
      dir_geo_vrsn <- list(
        parent = paste0(dest_path[[i]], project_doc[[i]]$dir_version),
        data = paste0(dest_path[[i]], project_doc[[i]]$dir_version, "/01_Data/"),
        code = paste0(dest_path[[i]], project_doc[[i]]$dir_version, "/02_Code/")
      )
      if (!dir.exists(dir_geo_vrsn$parent)) {
        purrr::map(dir_geo_vrsn, dir.create, showWarnings = FALSE)
      }
    }

    if (doc_mode == "code") {
      if (proj_type == "geo") {
        file_path <- paste0(dir_geo_vrsn$code, dest_dir[[i]])
        if (dir.exists(file_path) & !overwrite) {
          stop(paste0("The directory '", file_path, "' already exists!"))
        }
      } else {
        file_path <- paste0(dest_path[[i]], dest_dir[[i]])
      }

      if (proj_type == "geo" | proj_type == "mw") {
        # Remove unneeded data folders and files
        p <- paste0("data/1-source/(?!", paste(project_doc[[i]]$src_data, collapse = "|"), ")")
        dirs1 <- dirs_all[!dirs_all %in% dirs_all[stringr::str_detect(dirs_all, p)]]
        files1 <- files_all[!files_all %in% files_all[stringr::str_detect(files_all, p)]]
      } else {
        dirs1 <- dirs_all
        files1 <- files_all
      }

      # Check if `file_path` exists and overwrite or stop
      if (dir.exists(file_path)) {
        if (overwrite) {
          unlink(file_path, recursive = TRUE)
        } else if (!overwrite) {
          stop(paste0("The directory '", file_path, "' already exists!"))
        }
      }

      # Create destination path names
      dirs2 <- stringr::str_replace(dirs1, proj_path, file_path)
      # Create new project directories
      purrr::map(dirs2, dir.create) # WRITE

      # Remove unneeded scripts
      p <- paste0("scripts/(?!", paste(project_doc[[i]]$script, collapse = "|"), ")")
      files1 <- files1[!files1 %in% files1[stringr::str_detect(files1, p)]]

      # Create destination path names
      files2 <- stringr::str_replace(files1, proj_path, file_path)
      # Copy files
      purrr::map2(files1, files2, file.copy) # WRITE

      # Rename RMD script
      name1 <- paste0(file_path, "/scripts/", stringr::str_subset(project_doc[[i]]$script, ".+\\.Rmd$"))
      name2 <- paste0(file_path, "/scripts/", dest_dir[[i]], ".Rmd")
      file.rename(name1, name2) # RENAME

      # Rename .RPROJ file
      name1 <- files2[stringr::str_detect(files2, "\\.Rproj$")]
      name2 <- paste0(file_path, "/", dest_dir[[i]], ".Rproj")
      file.rename(name1, name2) # RENAME

      # Names of copied items
      objs <- list.files(file_path, all.files = TRUE, recursive = TRUE, include.dirs = TRUE)
    } else if (doc_mode == "output") {
      if (proj_type == "geo") {
        file_path <- dir_geo_vrsn$data
        if (length(list.files(file_path)) > 0 & !overwrite) {
          stop(paste0("The directory '", file_path, "' already contains files!"))
        }
      } else {
        file_path <- dest_dir[[i]]
      }

      # Select files to copy
      files1 <- list.files(proj_output_path[[i]], full.names = TRUE)
      # Create destination path names
      files2 <- stringr::str_replace(files1, proj_output_path[[i]], file_path)
      # Copy files
      purrr::map2(files1, files2, file.copy, overwrite = TRUE) # WRITE

      # Names of copied items
      objs <- list.files(file_path, all.files = TRUE, recursive = TRUE, include.dirs = TRUE)
      files2_names <- stringr::str_remove(files2, file_path)
      objs <- objs[objs %in% files2_names]
    }

    # Confirmation message
    message(
      paste0("Items copied to '", file_path, "':\n"),
      paste0(" - ", objs, collapse = "\n")
    )

    items_copied[[i]] <- ifelse(length(objs) > 0, TRUE, FALSE)
  }

  invisible(items_copied)
}

fn_project_info <- function(type) {
  if (type == "rabies") {
    list(
      project_doc_path = "/data/3-final/project_doc.rds",
      dest_path = list(
        test_county = "Z:/KSDataExplorer/02_TEST/01_DisplayDataFiles/RabiesSurveillance/01_County/",
        test_state = "Z:/KSDataExplorer/02_TEST/01_DisplayDataFiles/RabiesSurveillance/02_State/",
        prod_county = "Z:/KSDataExplorer/03_PRODUCTION/01_DisplayDataFiles/RabiesSurveillance/01_County/",
        prod_state = "Z:/KSDataExplorer/03_PRODUCTION/01_DisplayDataFiles/RabiesSurveillance/02_State/"
      )
    )
  } else if (type == "geo") {
    list(
      project_doc_path = "/data/3-final/project_doc_geo.rds",
      dest_path = list(
        test = "Z:/KSDataExplorer/02_TEST/02_GeographyTables/",
        prod = "Z:/KSDataExplorer/03_PRODUCTION/02_GeographyTables/"
      )
    )
  } else if (type == "mw") {
    list(
      project_doc_path = "/data/3-final/project_doc_mw.rds",
      dest_path = list(
        test = "Z:/KSDataExplorer/02_TEST/03_MessageWidget/",
        prod = "Z:/KSDataExplorer/03_PRODUCTION/03_MessageWidget/"
      )
    )
  } else if (type == "version") {
    list(
      project_doc_path = "/data/3-final/project_doc.rds",
      dest_path = list(
        test = "Z:/KSDataExplorer/02_TEST/05_VersionTable/",
        prod = "Z:/KSDataExplorer/03_PRODUCTION/05_VersionTable/"
      )
    )
  }
}

fn_patterns <- function() {
  list(
    # Folders to exclude
    p1 = paste(
      "/.git\\b",
      "/\\.Rproj\\.user\\b",
      "/data/1-source/past\\b",
      "/data/2-final/.+",
      "/data/3-final/.+",
      "/output/1_test/.+",
      "/output/2_prod/.+",
      "/renv\\b",
      sep = "|"
    ),
    # Files to exclude
    p2 = paste(
      "/.git/",
      "/.gitignore\\b",
      "/\\.Rproj\\.user/",
      "/data/2-final/",
      "/data/3-final/",
      "/log\\b",
      "/output/",
      "/renv/",
      sep = "|"
    )
  )
}
