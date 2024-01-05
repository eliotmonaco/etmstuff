# Generates `directions.rda`



full <- c("North", "South", "East", "West", "Northeast", "Northwest", "Southeast", "Southwest")

abbr <- c("N", "S", "E", "W", "NE", "NW", "SE", "SW")

directions <- data.frame(
  full = full,
  abbr = abbr
)

usethis::use_data(directions, overwrite = T)
