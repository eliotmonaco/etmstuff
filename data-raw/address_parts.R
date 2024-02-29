# Generates `unit_pfx.rda`

full <- c(
  "apartment", "basement", "building", "floor", "lot",
  "number", "room", "suite", "trailer", "unit", "#"
)

abbr <- c(
  "apt", "bsmt", "bldg", "fl", NA,
  "num", "rm", "ste", "trlr", NA, NA
)

unit_pfx <- tibble::tibble(full, abbr)

usethis::use_data(unit_pfx, overwrite = T)


# Generates `street_sfx.rda`

full <- c(
  "Avenue", "Boulevard", "Circle", "Court", "Crossing",
  "Drive", "Highway", "Lane", "Parkway", "Place", "Road",
  "Street", "Terrace", "Trafficway", "Trail", "Way"
)

abbr <- c(
  "Ave", "Blvd", "Cir", "Ct", "Xing", "Dr", "Hwy", "Ln",
  "Pkwy", "Pl", "Rd", "St", "Ter", "Trfy", "Trl", NA
)

street_sfx <- tibble::tibble(full, abbr)

usethis::use_data(street_sfx, overwrite = T)


# Generates `directions.rda`

full <- c(
  "North", "South", "East", "West",
  "Northeast", "Northwest", "Southeast", "Southwest"
)

abbr <- c("N", "S", "E", "W", "NE", "NW", "SE", "SW")

directions <- tibble::tibble(full, abbr)

usethis::use_data(directions, overwrite = T)
