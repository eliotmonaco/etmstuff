# Generates `street_names.rda`

library(tidyverse)

## Street names from Washington Post article
## source: https://www.washingtonpost.com/blogs/govbeat/wp/2015/03/06/these-are-the-most-popular-street-names-in-every-state/

df_wapo <- read.csv("data-raw/street_names_wapo.csv", col.names = "name")

street_names_wapo <- str_split_i(df_wapo$name, ",", 1)

street_names_wapo <- sort(unique(str_squish(na.omit(c(
  str_split_i(street_names_wapo, "/", 1),
  str_split_i(street_names_wapo, "/", 2)
)))))

street_names_wapo <- street_names_wapo[which(!str_detect(street_names_wapo, "^N\\s|^S\\s"))]

## Kansas street names from geographic.org
## source: https://geographic.org/streetview/usa/ks/index.html

df_geo <- read.csv("data-raw/street_names_geo.csv")

df_geo <- df_geo %>%
  mutate(across(everything(), ~ str_squish(str_remove(.x, "\\s\\d{5}$"))))

df_geo[df_geo == ""] <- NA

street_names_geo <- c()

### Combine separate columns into one
for (i in 1:ncol(df_geo)) {
  street_names_geo <- sort(unique(na.omit(c(df_geo[[i]], street_names_geo))))
}

df_geo <- data.frame(full_street = street_names_geo)

### Parse predirectional
pattern <- paste0("^", etmstuff::directions$abbr, "\\s", collapse = "|")
df_geo$predir <- str_squish(str_extract(df_geo$full_street, pattern))
df_geo$street <- str_squish(str_remove(df_geo$full_street, pattern))

### Parse predirectional placed at end of string
pattern <- paste0("(?i)", paste0(",\\s", etmstuff::directions$full, "$", collapse = "|"))
df_geo$predir_post <- str_squish(str_remove(str_extract(df_geo$street, pattern), ","))
df_geo$street <- str_squish(str_remove(df_geo$street, pattern))

### Parse postdirectional
pattern <- paste0("\\s", c("N", "S", "E", "W"), "$", collapse = "|")
df_geo$postdir <- str_squish(str_remove(str_extract(df_geo$street, pattern), ","))
df_geo$street <- str_squish(str_remove(df_geo$street, pattern))

### Parse suffix
sfx <- na.omit(c(etmstuff::street_sfx$full, etmstuff::street_sfx$abbr))
pattern <- paste0("(?i)", paste0("\\s", sfx, "$", collapse = "|"))
df_geo$street <- str_squish(str_remove(df_geo$street, pattern))
df_geo$street <- str_squish(str_remove(df_geo$street, pattern))

### Deduplicate street names
df_geo <- df_geo %>%
  distinct(street)

### Filter out remaining streets that contain suffix elements
pattern <- paste0("(?i)", paste0("\\b", sfx, "\\b", collapse = "|"))
df_geo <- df_geo %>%
  filter(!str_detect(street, pattern)) %>%
  filter(!str_detect(street, "^\\d+$")) %>%
  arrange(street)

### Get street names from `street_names_wapo` that don't appear in `street_names_geo`
street_names_geo <- sort(unique(df_geo$street))
pattern <- paste0(street_names_wapo, collapse = "|")
wapo_in_geo <- unique(str_extract(street_names_geo, pattern))
wapo_unq <- street_names_wapo[which(!street_names_wapo %in% wapo_in_geo)]

### Combine both sets of street names
street_names <- sort(unique(c(street_names_geo, street_names_wapo)))

usethis::use_data(street_names, overwrite = T)
