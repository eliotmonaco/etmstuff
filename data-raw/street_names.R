# Generates `street_names.rda`

library(tidyverse)


## source: https://www.washingtonpost.com/blogs/govbeat/wp/2015/03/06/these-are-the-most-popular-street-names-in-every-state/

df_wapo <- read.csv("data-raw/street_names_wapo.csv", col.names = "name")

street_names_wapo <- str_split_i(df_wapo$name, ",", 1)

street_names_wapo <- sort(unique(str_squish(na.omit(c(
  str_split_i(street_names_wapo, "/", 1),
  str_split_i(street_names_wapo, "/", 2)
)))))

street_names_wapo <- street_names_wapo[which(!str_detect(street_names_wapo, "^N\\s|^S\\s"))]







## source: https://geographic.org/streetview/usa/ks/index.html

df_geo <- read.csv("data-raw/street_names_geo.csv")

df_geo <- df_geo %>%
  mutate(across(everything(), ~ str_squish(str_remove(.x, "\\s\\d{5}$"))))

df_geo[df_geo == ""] <- NA

streets <- c()

for (i in 1:ncol(df_geo)) {
  streets <- sort(unique(na.omit(c(df_geo[[i]], streets))))
}

df_geo <- data.frame(full_name = streets)

# Parse pre-directional
pattern <- paste0("^", etmstuff::directions$abbr, "\\s", collapse = "|")
df_geo$pre_dir <- str_squish(str_extract(df_geo$full_name, pattern))
df_geo$street <- str_squish(str_remove(df_geo$full_name, pattern))

# Parse post-directional 2
pattern <- paste0("(?i)", paste0(",\\s", etmstuff::directions$full, "$", collapse = "|"))
df_geo$post_dir2 <- str_squish(str_remove(str_extract(df_geo$street, pattern), ","))
df_geo$street <- str_squish(str_remove(df_geo$street, pattern))

# Parse post-directional 1
pattern <- paste0("\\s", c("N", "S", "E", "W"), "$", collapse = "|")
df_geo$post_dir1 <- str_squish(str_remove(str_extract(df_geo$street, pattern), ","))
df_geo$street <- str_squish(str_remove(df_geo$street, pattern))

# # Parse street suffix
pattern <- paste0("\\s", na.omit(c(etmstuff::street_sfx$full, etmstuff::street_sfx$abbr)), "$", collapse = "|")
df_geo$suffix <- str_squish(str_extract(df_geo$street, pattern))
df_geo$street <- str_squish(str_remove(df_geo$street, pattern))

df_geo <- df_geo %>%
  select(full_name, pre_dir, street, suffix, post_dir1, post_dir2)

street_names_geo <- sort(unique(df_geo$street))
# suffix <- sort(unique(df_geo$suffix))




pattern <- paste0(street_names_wapo, collapse = "|")
street_names_wapo[which(!street_names_wapo %in% unique(str_extract(street_names_geo, pattern)))]






usethis::use_data(street_names, overwrite = T)
