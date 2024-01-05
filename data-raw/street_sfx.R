# Generates various objects consisting of address components



## Directions ####

full <- c("North", "South", "East", "West", "Northeast", "Northwest", "Southeast", "Southwest")

abbr <- c("N", "S", "E", "W", "NE", "NW", "SE", "SW")

directions <- data.frame(
  full = full,
  abbr = abbr
)

usethis::use_data(directions, overwrite = T)



## Street suffixes ####

full <- c("Avenue", "Boulevard", "Drive", "Lane", "Place", "Road", "Street", "Terrace")

abbr <- c("Ave", "Blvd", "Dr", "Ln", "Pl", "Rd", "St", "Ter")

street_sfx <- data.frame(
  full = full,
  abbr = abbr
)

usethis::use_data(street_sfx, overwrite = T)
