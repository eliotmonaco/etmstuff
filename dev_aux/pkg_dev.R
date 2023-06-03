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



## Documentation ####

library(devtools)

# Adds package name to DESCRIPTION file
usethis::use_package("")

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



## Internal data ####

directions_cardinal <- readRDS("../etmstuff_aux/helpers/directions_cardinal.rds")
directions_ordinal <- readRDS("../etmstuff_aux/helpers/directions_ordinal.rds")
epitrax_variables <- readRDS("../etmstuff_aux/helpers/epitrax_variables.rds")
ks_cities <- readRDS("../etmstuff_aux/helpers/ks_cities.rds")
ks_zipcodes <- readRDS("../etmstuff_aux/helpers/ks_zipcodes.rds")
regex_pobox <- readRDS("../etmstuff_aux/helpers/regex_pobox.rds")
regex_various <- readRDS("../etmstuff_aux/helpers/regex_various.rds")
street_suffix <- readRDS("../etmstuff_aux/helpers/street_suffix.rds")
directions <- readRDS("../etmstuff_aux/helpers/directions.rds")
common_streets <- readRDS("../etmstuff_aux/helpers/common_streets.rds")
common_street_suffixes <- readRDS("../etmstuff_aux/helpers/common_street_suffixes.rds")
common_unit_prefixes <- readRDS("../etmstuff_aux/helpers/common_unit_prefixes.rds")

usethis::use_data(directions_cardinal, directions_ordinal,
                  epitrax_variables,
                  ks_cities, ks_zipcodes,
                  regex_pobox, regex_various,
                  directions, common_streets, common_street_suffixes, common_unit_prefixes,
                  internal = T, overwrite = T)



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




library(tidyverse)


# Sim data for Epitrax?


filename <- "dupesets_data"
path <- "helpers/"

xlpath <- paste0(gsub("/", "\\\\", getwd()), "\\", gsub("/", "\\\\", path), filename, ".xlsm")



df_addr <- df_addr_full %>%
  distinct(address_id, .keep_all = T)

df <- clean_values(df = df_addr,
                   var = "street",
                   id_var = "address_id",
                   type = "pobox")

df_addr4 <- replace_values(df = df_addr,
                           var = "street",
                           id_var = "address_id",
                           source = df)

all.equal(df_addr2, df_addr4)

dfA <- validate_values(df = df,
                       var = "city",
                       type = "city")

dfA <- validate_values(df = df,
                       var = "zip",
                       type = "zip")

df <- simulate_data(rows = 1000, dirty = T)




