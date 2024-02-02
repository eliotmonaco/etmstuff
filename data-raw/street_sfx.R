# Generates `street_sfx.rda`

full <- c(
  "Avenue", "Boulevard", "Circle", "Court", "Crossing", "Drive", "Highway", "Lane",
  "Parkway", "Place", "Road", "Street", "Terrace", "Trafficway", "Trail", "Way"
)

abbr <- c(
  "Ave", "Blvd", "Cir", "Ct", "Xing", "Dr", "Hwy", "Ln",
  "Pkwy", "Pl", "Rd", "St", "Ter", "Trfy", "Trl", NA
)

street_sfx <- data.frame(
  full = full,
  abbr = abbr
)

usethis::use_data(street_sfx, overwrite = T)
