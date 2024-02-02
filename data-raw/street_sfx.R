# Generates `street_sfx.rda`

full <- c("Avenue", "Boulevard", "Drive", "Lane", "Place", "Road", "Street", "Terrace")

abbr <- c("Ave", "Blvd", "Dr", "Ln", "Pl", "Rd", "St", "Ter")

street_sfx <- data.frame(
  full = full,
  abbr = abbr
)

usethis::use_data(street_sfx, overwrite = T)
