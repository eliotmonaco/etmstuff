# Pre-setup ####

## Helpful links:
## - R Packages (https://r-pkgs.org/)
## - Happy Git and GitHub for the useR (https://happygitwithr.com/index.html)

## Create GitHub token with scope that allows package writing. Store in password manager (e.g., LastPass).



# Setup ####

install.packages("devtools")
install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
install.packages(c("credentials", "gitcreds"))

library(devtools)

usethis::create_package()

usethis::use_git()

usethis::use_github()

usethis::use_testthat(3)



# External packages/functions ####

## Add package to DESCRIPTION (for any package that includes a function called within etmstuff)
usethis::use_package("")

## Adds package and function to NAMESPACE (for any package & function explicitly imported using @importFrom)
usethis::use_import_from("", "")



# Test/build workflow ####

usethis::use_r("build_md_url")

usethis::use_test("")

devtools::load_all()

testthat::test_file("tests/testthat/test-.R")

devtools::test()

devtools::document()

devtools::check()

## Build: CTRL + SHIFT + B



# .Rbuildignore ####

usethis::use_build_ignore(c("dev-aux"))

usethis::use_data_raw()



# Extra ####

## Remove package from Imports in DESCRIPTION file
desc::desc_del_dep("")

remove.packages("etmstuff")

devtools::install()

library(etmstuff)

devtools::loaded_packages()




