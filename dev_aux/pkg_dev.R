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



## External packages/functions ####

# Add package to DESCRIPTION (for any package that includes a function called within etmstuff)
usethis::use_package("")

# Adds package and function to NAMESPACE (for any package & function explicitly imported using @importFrom)
usethis::use_import_from("", "")



## Test/build workflow ####

devtools::load_all()

devtools::document()

devtools::check()

# Build: CTRL+SHIFT+B



## globals.R ####

epitrax_variables <- readRDS("dev_aux/helpers/epitrax_variables.rds")

message(paste(paste0('"', epitrax_variables, '"'), collapse = ", "))



## .Rbuildignore ####

usethis::use_build_ignore(c("dev_aux"))



## Extra ####

# Remove package from Imports in DESCRIPTION file
desc::desc_del_dep("")

remove.packages("etmstuff")

devtools::install()

library(etmstuff)

devtools::loaded_packages()


# When a function needs to know the name of a unique row ID in `df`, the argument name is `row_id`.
# When a function needs the user to supply a name for a row ID variable that will be added to `df` by the function, the argument name is `id_name`.




