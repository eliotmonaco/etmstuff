## `ks_zipcodes` dataset

load(file = "data/ks_zipcodes.rda")

new_zips <- c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "")

new_zips <- sort(unique(c(ks_zipcodes, new_zips)))

usethis::use_data(ks_zipcodes, overwrite = T)
