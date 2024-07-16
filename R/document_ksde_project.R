#' Document a KS Tracking Data Explorer project
#'
#' @param proj_dir The name of the RStudio project to document.
#' @param type The project type.
#'
#' * `"display"` = display data
#' * `"geo"` = core geography tables
#' * `"mw"` = message widget table
#' * `"version"` = version table
#'
#' @param overwrite If the destination directory for the project already exists, it will be removed when `overwrite` is set to `TRUE`.
#' @param test_drive Replace the destination path with the desktop ("C:/Users/eliot.monaco/OneDrive - State of Kansas, OITS/Desktop/") if `TRUE`.
#'
#' @return No value returned.
#' @export
#'
# @examples
#'
document_ksde_project <- function(proj_dir, type = c("display", "geo", "mw", "version"), overwrite = FALSE, test_drive = FALSE) {
  if (type == "display") {
    proj_path = paste0("KS_Tracking_Data_Explorer/Rabies_Surveillance/", proj_dir)
  } else if (type == "geo" | type == "mw") {
    proj_path = paste0("KS_Tracking_Data_Explorer/Geography_Tables/", proj_dir)
  } else if (type == "version") {
    proj_path = paste0("KS_Tracking_Data_Explorer/Version_Table/", proj_dir)
  }

  proj_info <- fn_project_info(type)

  if (test_drive) {
    # Overwrite `proj_info$dest_path` to save projects to desktop for testing
    dir <- "C:/Users/eliot.monaco/OneDrive - State of Kansas, OITS/Desktop/"
    proj_info$dest_path <- purrr::map(
      proj_info$dest_path,
      stringr::str_replace, pattern = ".+", replacement = dir
    )
  }

  file_path <- paste0(proj_path, proj_info$code_doc_path)
  code_doc <- readRDS(file_path)

  # Create destination directory name(s)
  dest_dir <- purrr::map(code_doc, function(x) purrr::pluck(x, "name"))

  if (length(dest_dir) != length(proj_info$dest_path)) {
    stop("Length mismatch: `dest_dir` & `dest_path`")
  }

  # Select directories to copy
  dirs_all <- list.dirs(proj_path, full.names = TRUE, recursive = TRUE)
  p <- paste( # Exclude the following folders
    "/.git\\b",
    "/\\.Rproj\\.user\\b",
    "/data/1_source/past\\b",
    "/data/2_final/.+",
    "/data/3_final/.+",
    "/output/1_test/.+",
    "/output/2_prod/.+",
    "/renv\\b",
    sep = "|"
  )
  dirs_all <- dirs_all[!stringr::str_detect(dirs_all, p)]

  # Select files to copy
  files_all <- list.files(proj_path, all.files = TRUE, full.names = TRUE, recursive = TRUE)
  p <- paste( # Exclude paths containing the following strings
    "/.git/",
    "/.gitignore\\b",
    "/\\.Rproj\\.user/",
    "/data/2_final/",
    "/data/3_final/",
    "/log\\b",
    "/output/",
    "/renv/",
    sep = "|"
  )
  files_all <- files_all[!stringr::str_detect(files_all, p)]

  if (!any(stringr::str_detect(files_all, "\\brenv.lock\\b"))) {
    stop("No renv.lock file present")
  }

  for (i in 1:length(dest_dir)) {
    if (type == "geo") {
      # Geography table data files and code are stored in a version folder created here
      dir_geo_vrsn <- list(
        parent = paste0(proj_info$dest_path[[i]], code_doc[[i]]$version_dir),
        data = paste0(proj_info$dest_path[[i]], code_doc[[i]]$version_dir, "/01_Data/"),
        code = paste0(proj_info$dest_path[[i]], code_doc[[i]]$version_dir, "/02_Code/")
      )
      if (dir.exists(dir_geo_vrsn$parent) & !overwrite) {
        stop(paste0("The directory '", dir_geo_vrsn$parent, "' already exists!"))
      } else if (!dir.exists(dir_geo_vrsn$parent)) {
        purrr::map(dir_geo_vrsn, dir.create)
      }
      file_path <- paste0(dir_geo_vrsn$code, dest_dir[[i]])
    } else {
      # All other data is stored in a fixed location
      file_path <- paste0(proj_info$dest_path[[i]], dest_dir[[i]])
    }

    if (type == "geo" | type == "mw") {
      # Remove unneeded data folders and files
      p <- paste0("data/1_source/(?!", paste(code_doc[[i]]$src_data, collapse = "|"), ")")
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

    # Create new project directories
    dirs2 <- stringr::str_replace(dirs1, proj_path, file_path)
    purrr::map(dirs2, dir.create)

    # Remove unneeded scripts
    p <- paste0("scripts/(?!", paste(code_doc[[i]]$script, collapse = "|"), ")")
    files1 <- files1[!files1 %in% files1[stringr::str_detect(files1, p)]]

    # Copy files
    files2 <- stringr::str_replace(files1, proj_path, file_path)
    purrr::map2(files1, files2, file.copy)

    # Rename RMD script
    name1 <- paste0(
      file_path, "/scripts/",
      code_doc[[i]]$script[stringr::str_detect(code_doc[[i]]$script, ".Rmd$")]
    )
    name2 <- paste0(file_path, "/scripts/", dest_dir[[i]], ".Rmd")
    file.rename(name1, name2)

    # Rename .RPROJ file
    name1 <- paste0(file_path, "/", proj_dir, ".rproj")
    name2 <- paste0(file_path, "/", dest_dir[[i]], ".rproj")
    file.rename(name1, name2)
  }
}

fn_project_info <- function(type) {
  if (type == "display") {
    list(
      code_doc_path = "/data/3_final/code_doc.rds",
      dest_path = list(
        test_county = "Z:/KSDataExplorer/02_TEST/01_DisplayDataFiles/RabiesSurveillance/01_County/02_Code/",
        test_state = "Z:/KSDataExplorer/02_TEST/01_DisplayDataFiles/RabiesSurveillance/02_State/02_Code/",
        prod_county = "Z:/KSDataExplorer/03_PRODUCTION/01_DisplayDataFiles/RabiesSurveillance/01_County/02_Code/",
        prod_state = "Z:/KSDataExplorer/03_PRODUCTION/01_DisplayDataFiles/RabiesSurveillance/02_State/02_Code/"
      )
    )
  } else if (type == "geo") {
    list(
      code_doc_path = "/data/3_final/code_doc_geo.rds",
      dest_path = list(
        test = "Z:/KSDataExplorer/02_TEST/02_GeographyTables/",
        prod = "Z:/KSDataExplorer/03_PRODUCTION/02_GeographyTables/"
      )
    )
  } else if (type == "mw") {
    list(
      code_doc_path = "/data/3_final/code_doc_mw.rds",
      dest_path = list(
        test = "Z:/KSDataExplorer/02_TEST/03_MessageWidget/02_Code/",
        prod = "Z:/KSDataExplorer/03_PRODUCTION/03_MessageWidget/02_Code/"
      )
    )
  } else if (type == "version") {
    list(
      code_doc_path = "/data/2_final/code_doc.rds",
      dest_path = list(
        test = "Z:/KSDataExplorer/02_TEST/05_VersionTable/02_Code/",
        prod = "Z:/KSDataExplorer/03_PRODUCTION/05_VersionTable/02_Code/"
      )
    )
  }
}
