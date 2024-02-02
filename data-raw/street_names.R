# Generates `street_names.rda`

library(tidyverse)

df <- read.csv("data-raw/street_names_wapo.csv", col.names = "name")

street_names_wapo <- str_split_i(df$name, ",", 1)

street_names_wapo <- sort(unique(str_squish(na.omit(c(
  str_split_i(street_names_wapo, "/", 1),
  str_split_i(street_names_wapo, "/", 2)
)))))

street_names_wapo <- street_names_wapo[which(!str_detect(street_names_wapo, "^N\\s|^S\\s"))]



df <- read.csv("data-raw/street_names_geo.csv")

df <- df %>%
  mutate(across(everything(), ~ str_squish(str_remove(.x, "\\s\\d{5}$"))))

streets <- c()

for (i in 1:ncol(df)) {
  streets <- sort(unique(na.omit(c(streets, df[[i]]))))
}

df <- data.frame(streets = streets)

df$dir_pre <- str_squish(str_extract(df$street, paste0("^", etmstuff::directions$abbr, "\\s", collapse = "|")))
df$name <- str_squish(str_remove(df$street, paste0("^", etmstuff::directions$abbr, "\\s", collapse = "|")))

pattern <- paste0("(?i)", paste0(",\\s", etmstuff::directions$full, "$", collapse = "|"))
df$dir_suf <- str_squish(str_extract(df$street, pattern))
df$name <- str_squish(str_remove(df$street, paste0("^", etmstuff::directions$abbr, "\\s", collapse = "|")))










street_names_geo <- sort(unique(na.omit(apply(df, 2, c))))

street_names_geo[12000:13000]

usethis::use_data(street_names, overwrite = T)


df[[1]]



apply(directions, 2, c)




