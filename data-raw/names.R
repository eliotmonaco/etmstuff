# Generates `names.rda`

df1 <- readxl::read_xlsx("data-raw/names.xlsx", sheet = 1)
df2 <- readxl::read_xlsx("data-raw/names.xlsx", sheet = 2)

names <- list()

names$first_male <- sort(df1$male)
names$first_female <- sort(df1$female)
names$last <- sort(stringr::str_to_title(df2$last))

usethis::use_data(names, overwrite = T)
