# Generate internal data for package

directions_cardinal <- readRDS("dev_aux/helpers/directions_cardinal.rds")
directions_ordinal <- readRDS("dev_aux/helpers/directions_ordinal.rds")
epitrax_variables <- readRDS("dev_aux/helpers/epitrax_variables.rds")
epitrax_variables_reordered <- readRDS("dev_aux/helpers/epitrax_variables_reordered.rds")
postmastr_directions <- readRDS("dev_aux/helpers/postmastr_directions.rds")
ks_cities <- readRDS("dev_aux/helpers/ks_cities.rds")
ks_zipcodes <- readRDS("dev_aux/helpers/ks_zipcodes.rds")
regex_pobox <- readRDS("dev_aux/helpers/regex_pobox.rds")
regex_various <- readRDS("dev_aux/helpers/regex_various.rds")
street_suffix <- readRDS("dev_aux/helpers/street_suffix.rds")
directions <- readRDS("dev_aux/helpers/directions.rds")
common_streets <- readRDS("dev_aux/helpers/common_streets.rds")
common_street_suffixes <- readRDS("dev_aux/helpers/common_street_suffixes.rds")
common_unit_prefixes <- readRDS("dev_aux/helpers/common_unit_prefixes.rds")

usethis::use_data(
  directions_cardinal,
  directions_ordinal,
  epitrax_variables,
  epitrax_variables_reordered,
  postmastr_directions,
  ks_cities,
  ks_zipcodes,
  regex_pobox,
  regex_various,
  street_suffix,
  directions,
  common_streets,
  common_street_suffixes,
  common_unit_prefixes,
  internal = T, overwrite = T
)



# Modify `regex_various` ####

library(tidyverse)


## Concatenated number/direction & number/word

new <- data.frame(
  pattern = c(
    "(?<=^\\d{1,1000})[ENSW]\\b",
    "(?<=^\\d{1,1000})[:alpha:]{2,}"
  ),
  replacement = c("", "")
)

rownames(new) <- c("num_dir", "num_word")

regex_various <- regex_various %>%
  bind_rows(new)

saveRDS(regex_various, "dev_aux/helpers/regex_various.rds")


## Embedded punctuation

new <- data.frame(
  pattern = "(?<=[:alnum:])[:punct:]+(?=[:alnum:])",
  replacement = " "
)

rownames(new) <- "embed_punct"

regex_various <- regex_various %>%
  bind_rows(new)

saveRDS(regex_various, "dev_aux/helpers/regex_various.rds")


## Unknown address

new <- data.frame(
  pattern = "^.*(9999|address|needed|unknown).*$",
  replacement = ""
)

rownames(new) <- "unknown"

regex_various <- regex_various %>%
  bind_rows(new)

saveRDS(regex_various, "dev_aux/helpers/regex_various.rds")


# regex_various <- regex_various[-12,]



# Modify `ks_cities` ####

new_cities <- toupper(c("Bavaria", "Blaine", "Carlton", "Petrolia", "Quincy", "Radium", "Gas"))

ks_cities <- sort(unique(c(ks_cities, new_cities)))

saveRDS(ks_cities, "dev_aux/helpers/ks_cities.rds")


