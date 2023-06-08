## Pre-setup ####

# Helpful links:
# - R Packages (https://r-pkgs.org/)
# - Happy Git and GitHub for the useR (https://happygitwithr.com/index.html)

# Create GitHub token with scope that allows package writing. Store in password manager (e.g., LastPass).



## Setup ####

install.packages("devtools")
install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
install.packages(c("credentials", "gitcreds"))

library(devtools)

usethis::create_package()

usethis::use_git()

usethis::use_github()



## Document ####

library(devtools)

# Adds package name to DESCRIPTION file
usethis::use_package("tidyselect")

# Adds package and function names to NAMESPACE file
usethis::use_import_from("", "")

devtools::document()

devtools::check()

devtools::load_all()



## Install & load package ####

# Ctrl + Shift + B to build

remove.packages("etmstuff")

devtools::install()

library(etmstuff)



## Sim data stuff ####

# Street: PO Box, symbol, starts with nondigit, directions?, fractional house num, phone, concat, etc...

directions <- str_trim(sort(unique(c(directions_cardinal$replacement, directions_ordinal$replacement))))

common_streets <- read.csv("../etmstuff_aux/helpers/streets.csv")
common_streets$new <- str_remove_all(common_streets$street,
                                     regex(paste0("^(", paste0(directions, collapse = "|"), ")\\s"),
                                           ignore_case = T))
common_streets$new <- str_extract(common_streets$new, "^[^\\s]*")
common_streets <- sort(unique(common_streets$new))

common_street_suffixes <- sort(c("St", "Rd", "Ave", "Dr", "Pl", "Ln", "Blvd"))

common_unit_prefixes <- sort(c("Apt", "Unit", "#", "Lot", "Trlr", "Ste"))

saveRDS(directions, "../etmstuff_aux/helpers/directions.rds")
saveRDS(common_streets, "../etmstuff_aux/helpers/common_streets.rds")
saveRDS(common_street_suffixes, "../etmstuff_aux/helpers/common_street_suffixes.rds")
saveRDS(common_unit_prefixes, "../etmstuff_aux/helpers/common_unit_prefixes.rds")






## globals.R ####

epitrax_variables <- readRDS("../etmstuff_aux/helpers/epitrax_variables.rds")

message(paste(paste0('"', epitrax_variables, '"'), collapse = ", "))



## .Rbuildignore ####

usethis::use_build_ignore(c("dev_aux"))



## Extra ####

devtools::loaded_packages()


# Rename all id argument names to "row_id"?




