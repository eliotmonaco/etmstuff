# Generate internal data for package

directions <- readRDS("dev_aux/helpers/directions.rds")
directions_cardinal <- readRDS("dev_aux/helpers/directions_cardinal.rds")
directions_ordinal <- readRDS("dev_aux/helpers/directions_ordinal.rds")
epitrax_variables <- readRDS("dev_aux/helpers/epitrax_variables.rds")
epitrax_variables_reordered <- readRDS("dev_aux/helpers/epitrax_variables_reordered.rds")
fips <- readRDS("dev_aux/helpers/fips.rds")
ks_cities <- readRDS("dev_aux/helpers/ks_cities.rds")
ks_zipcodes <- readRDS("dev_aux/helpers/ks_zipcodes.rds")
pm_direction_dictionary <- readRDS("dev_aux/helpers/pm_direction_dictionary.rds")
pm_street_suffix <- readRDS("dev_aux/helpers/pm_street_suffix.rds")
regex_pobox <- readRDS("dev_aux/helpers/regex_pobox.rds")
regex_various <- readRDS("dev_aux/helpers/regex_various.rds")
street_names <- readRDS("dev_aux/helpers/street_names.rds")
street_suffixes <- readRDS("dev_aux/helpers/street_suffixes.rds")
unit_prefixes <- readRDS("dev_aux/helpers/unit_prefixes.rds")

usethis::use_data(
  directions,
  directions_cardinal,
  directions_ordinal,
  epitrax_variables,
  epitrax_variables_reordered,
  fips,
  ks_cities,
  ks_zipcodes,
  pm_direction_dictionary,
  pm_street_suffix,
  regex_pobox,
  regex_various,
  street_names,
  street_suffixes,
  unit_prefixes,
  internal = T, overwrite = T
)



# Modify fips ####

fips <- read.csv("dev_aux/helpers/fips.csv")
colnames(fips) <- tolower(colnames(fips))
colnames(fips)
colnames(fips) <- c("fips", "st_fips", "cnty_fips", "cnty_code", "state", "county")
fips$cnty_fips <- as.character(substr(fips$fips, 3, 5))
fips$fips <- as.character(fips$fips)
fips$st_fips <- as.character(fips$st_fips)

saveRDS(fips, "dev_aux/helpers/fips.rds")



# Modify `regex_various` ####

library(tidyverse)

## Unit

regex_various["unit", 1]

# Old: "(?<=\\s)((apt|ap|unit|ste|suite|lot|trlr)(\\s|[:punct:])|#)[:graph:]+$"
# New: "(?<=\\s)(#|ap|apt|lot|rm|room|ste|suite|trlr|unit)(\\s|[:punct:])*[:alnum:]+$"

regex_various["unit", 1] <- "(?<=\\s)(#|ap|apt|lot|rm|room|ste|suite|trlr|unit)(\\s|[:punct:])*[:alnum:]+$"

saveRDS(regex_various, "dev_aux/helpers/regex_various.rds")



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



# Modify `ks_cities` ####

new_cities <- toupper(c("Bavaria", "Blaine", "Carlton", "Petrolia", "Quincy", "Radium", "Gas"))

ks_cities <- sort(unique(c(ks_cities, new_cities)))

saveRDS(ks_cities, "dev_aux/helpers/ks_cities.rds")



# Modify `pm_direction_dictionary` ####

pm_direction_dictionary <- postmastr::dic_us_dir

pm_dd_add <- data.frame(
  dir.output = c("NE", "NW", "SE", "SW"),
  dir.input = c("Ne", "Nw", "Se", "Sw")
)

pm_direction_dictionary <- pm_direction_dictionary %>%
  bind_rows(pm_dd_add)

saveRDS(pm_direction_dictionary, "dev_aux/helpers/pm_direction_dictionary.rds")










