## `ks_cities` dataset

load(file = "data/ks_cities.rda")
load(file = "data/ks_places.rda")

library(tidyverse)

v <- str_remove(ks_places$PLACENAME, "\\scity$|\\sCDP$")

v_saint <- ks_places %>%
  filter(str_detect(PLACENAME, "^St\\.")) %>%
  mutate(PLACENAME = str_remove(PLACENAME, "\\scity$|\\sCDP$")) %>%
  pull(PLACENAME)

v_saint1 <- str_replace(v_saint, "^St\\.", "St")

v_saint2 <- str_replace(v_saint, "^St\\.", "Saint")

v2 <- ks_cities[which(!ks_cities %in% str_to_upper(c(v, v_saint1, v_saint2)))]

v2 <- str_to_title(v2)

ks_cities <- sort(unique(c(v, v_saint1, v_saint2, v2, "Fort Riley North")))

usethis::use_data(ks_cities, overwrite = T)



## Modify 11/14/2023

load(file = "data/ks_cities.rda")

ks_cities <- sort(unique(c(ks_cities, "Kingsdown", "Modoc")))



## Modify 12/18/2023

load(file = "data/ks_cities.rda")

ks_cities <- sort(unique(c(ks_cities, "Kalvesta", "Bloom", "Houston", "Delavan", "Hiattville", "Coalvale", "Zimmerdale", "Westfall", "Medora", "Belvidere")))



## Modify 1/2/2024

load(file = "data/ks_cities.rda")

ks_cities <- sort(unique(c(ks_cities, "Cadmus")))



usethis::use_data(ks_cities, overwrite = T)
