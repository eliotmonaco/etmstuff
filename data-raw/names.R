# Generates `names.rda`

# First names source: https://www.ssa.gov/oact/babynames/decades/century.html
# Last names source: https://www.census.gov/topics/population/genealogy/data/2010_surnames.html

df1 <- readxl::read_xlsx("data-raw/names.xlsx", sheet = 1)
df2 <- readxl::read_xlsx("data-raw/names.xlsx", sheet = 2)

names <- list()

names$first_male <- sort(df1$male)
names$first_female <- sort(df1$female)
names$last <- sort(stringr::str_to_title(df2$last))

usethis::use_data(names, overwrite = T)
