



data <- data_core_2023q1

str(data)


## Dates

class(data[[4]])



rows <- 100

df <- data.frame(
  id = 1:rows,
  d1 = sample(
    seq.Date(
      from = as.Date("2020-01-01"),
      to = as.Date("2022-12-31"),
      by = "day"
    ),
    size = rows
  ),
  d2 = sample(
    seq.Date(
      from = as.Date("2020-01-01"),
      to = as.Date("2022-12-31"),
      by = "day"
    ),
    size = rows
  )
)

start <- as.Date("2020-01-01")
end <- as.Date("2022-12-31")

sample(
  seq.Date(
    from = start,
    to = end,
    by = "day"
  ),
  size = rows
)



## First names
# https://www.ssa.gov/oact/babynames/decades/century.html



## Last names
# https://www.census.gov/topics/population/genealogy/data/2010_surnames.html













## Sim data stuff ####

# Street: PO Box, symbol, starts with nondigit, directions?, fractional house num, phone, concat, etc...

directions <- str_trim(sort(unique(c(directions_cardinal$replacement, directions_ordinal$replacement))))

common_streets <- read.csv("../etmstuff_aux/helpers/streets.csv")
common_streets$new <- str_remove_all(common_streets$street,
                                     regex(paste0("^(", paste0(directions, collapse = "|"), ")\\s"),
                                           ignore_case = T))
common_streets$new <- str_extract(common_streets$new, "^[^\\s]*")
common_streets <- sort(unique(common_streets$new))

common_street_suffixes <- sort(c("St", "Rd", "Ave", "Dr", "Pl", "Ln", "Blvd"))

common_unit_prefixes <- sort(c("Apt", "Unit", "#", "Lot", "Trlr", "Ste"))

saveRDS(directions, "../etmstuff_aux/helpers/directions.rds")
saveRDS(common_streets, "../etmstuff_aux/helpers/common_streets.rds")
saveRDS(common_street_suffixes, "../etmstuff_aux/helpers/common_street_suffixes.rds")
saveRDS(common_unit_prefixes, "../etmstuff_aux/helpers/common_unit_prefixes.rds")
















