library(tidyverse)
library(etmstuff)


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

first_names <- openxlsx::read.xlsx("dev_aux/sim/names.xlsx", sheet = 1)
last_names <- openxlsx::read.xlsx("dev_aux/sim/names.xlsx", sheet = 2)

colnames(first_names) <- c("male", "female")
colnames(last_names) <- "name"
last_names$name <- str_to_title(last_names$name)

saveRDS(first_names, "dev_aux/sim/first_names.rds")
saveRDS(last_names, "dev_aux/sim/last_names.rds")



hist(data_core_2023q1$lab_result_number)

mean(data_core_2023q1$lab_result_number)

pois <- rpois(n = 10600, lambda = 3.6)

hist(pois)



library(sn)

samp <- sn::rsn(n = 10600, alpha = 4, tau = 3.6)

hist(samp)



lrn <- data_core_2023q1$lab_result_number

min(lrn)
max(lrn)


x <- seq(0, 60, by = 0.1)
y <- dsn(x, xi = 0, omega = 1, alpha = 0)
hist(y)



















epitrax_sim <- as.data.frame(matrix(nrow = 0, ncol = length(etmstuff:::epitrax_variables_reordered)))

colnames(epitrax_sim) <- etmstuff:::epitrax_variables_reordered

skimr::skim(data_core_2023q1)






epitrax_sim <- sim_epitrax(100)









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
















