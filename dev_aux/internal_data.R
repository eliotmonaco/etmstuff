# Generate internal data for package

directions <- readRDS("dev_aux/helpers/directions.rds")
directions_cardinal <- readRDS("dev_aux/helpers/directions_cardinal.rds")
directions_ordinal <- readRDS("dev_aux/helpers/directions_ordinal.rds")
epitrax_date_vars <- readRDS("dev_aux/helpers/epitrax_date_vars.rds")
epitrax_vars <- readRDS("dev_aux/helpers/epitrax_vars.rds")
epitrax_vars_reordered <- readRDS("dev_aux/helpers/epitrax_vars_reordered.rds")
fips <- readRDS("dev_aux/helpers/fips.rds")
ks_cities <- readRDS("dev_aux/helpers/ks_cities.rds")
ks_locations <- readRDS("dev_aux/helpers/ks_locations.rds")
ks_zipcodes <- readRDS("dev_aux/helpers/ks_zipcodes.rds")
pm_direction_dictionary <- readRDS("dev_aux/helpers/pm_direction_dictionary.rds")
pm_street_suffix <- readRDS("dev_aux/helpers/pm_street_suffix.rds")
regex_pobox <- readRDS("dev_aux/helpers/regex_pobox.rds")
regex_various <- readRDS("dev_aux/helpers/regex_various.rds")
street_names <- readRDS("dev_aux/helpers/street_names.rds")
street_suffixes <- readRDS("dev_aux/helpers/street_suffixes.rds")
styles_css <- readRDS("dev_aux/helpers/styles_css.rds")
unit_prefixes <- readRDS("dev_aux/helpers/unit_prefixes.rds")

usethis::use_data(
  directions,
  directions_cardinal,
  directions_ordinal,
  epitrax_date_vars,
  epitrax_vars,
  epitrax_vars_reordered,
  fips,
  ks_cities,
  ks_locations,
  ks_zipcodes,
  pm_direction_dictionary,
  pm_street_suffix,
  regex_pobox,
  regex_various,
  street_names,
  street_suffixes,
  styles_css,
  unit_prefixes,
  internal = T, overwrite = T
)



# epitrax_date_vars ####

epitrax_date_vars <- c(
  "patient_birth_date",
  "treatment_date",
  "lab_collection_date",
  "lab_test_date",
  "lab_created_at",
  "patient_investigation_completed_lhd_date",
  "lhd_investigation_start_date",
  "lhd_date_closed",
  "first_investigation_started_date",
  "last_investigation_completed_lhd_date",
  "first_accepted_by_lhd_date",
  "last_approved_by_lhd_date",
  "last_routed_to_lhd_date",
  "patient_results_reported_to_LHD"
)

saveRDS(epitrax_date_vars, "dev_aux/helpers/epitrax_date_vars.rds")



# Write and update styles.css ####

## Write styles.css
writeLines(
  styles_css,
  con = "dev_aux/helpers/styles.css"
)

## Read updated styles.css
styles_css <- readLines("dev_aux/helpers/styles.css")

## Save as RDS
saveRDS(styles_css, "dev_aux/helpers/styles_css.rds")



# Create ks_locations ####

## Import list copied from PDF to Excel

zips <- openxlsx::read.xlsx("dev_aux/helpers/ZipCodes2018.xlsx")

zips %>%
  filter(nchar(line) == 1)

zips %>%
  filter(str_detect(line, "•"))

zips <- zips %>%
  filter(nchar(line) > 1) %>%
  filter(!str_detect(line, "•")) %>%
  filter(!str_detect(line, "^2018\\s"))

zips$line2 <- zips$line

zips <- zips[-c(112:115),]

zips$line <- str_replace(zips$line, "(?<=[:alpha:]),", "+")
zips$line <- str_replace(zips$line, "\\.{2,}", "|")

zips$city <- str_extract(zips$line, ".*(?=\\+)")
zips$county <- str_extract(zips$line, "(?<=\\+\\s).*(?=\\|)")
zips$zip <- str_extract(zips$line, "(?<=\\|).*")

zips <- zips %>%
  mutate(zip = if_else(
    !str_detect(line, "[:alpha:]"),
    true = line,
    false = zip
  ))

zips <- zips[, -2]

openxlsx::write.xlsx(zips, "dev_aux/helpers/ZipCodes2018_2.xlsx")


## Import list after manual cleaning

zips <- openxlsx::read.xlsx("dev_aux/helpers/ZipCodes2018_2.xlsx")

zips <- zips %>%
  filter(!is.na(city) | !is.na(county) | !is.na(zip)) %>%
  select(-line)


## Convert zip code strings to columns

extract_seq <- function(x) {
  if (str_detect(x, "-")) {
    as.numeric(substr(x, 1, 5)):as.numeric(substr(x, 7, 11))
  } else {
    as.numeric(x)
  }
}

str_to_num <- function(s) {
  v <- unlist(strsplit(s, split = ", "))
  sort(unlist(lapply(v, extract_seq)))
}

zip_list <- list()

for (i in 1:nrow(zips)) {
  zip_list[[i]] <- data.frame(
    city = zips$city[i],
    county = zips$county[i],
    zip = str_to_num(zips$zip[i])
  )
}

ks_locations <- as.data.frame(do.call(rbind, zip_list))

ks_locations <- ks_locations %>%
  mutate(city = str_to_title(city))

saveRDS(ks_locations, "dev_aux/helpers/ks_locations.rds")



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

regex_various["unit", 1] <- paste0(
  "(?<=\\s)",
  "(#|ap|apt|apartment|lot|no|num|number|rm|room|ste|suite|trlr|trailer|unit)",
  "(\\s|[:punct:])*[:alnum:]+$"
)

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

ks_cities <- readRDS("dev_aux/helpers/ks_cities.rds")

new_cities <- toupper(c("Preston", "Langdon", "Climax", "Rago", "Yoder", "Leona"))

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










