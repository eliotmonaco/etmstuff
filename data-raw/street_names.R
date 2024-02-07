# Generates `street_names.rda`

library(tidyverse)

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

### Add `address_id`
df_geo <- etmstuff::id_distinct_rows(df_geo, "full_street", "address_id", "AD")
df_geo <- df_geo %>%
  relocate(address_id)

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

### Replace postdirectional with predirectional
df_geo %>%
  group_by(predir_post) %>%
  count()
df_geo <- df_geo %>%
  mutate(predir = case_when(
    predir_post == "North" ~ "N", predir_post == "South" ~ "S",
    predir_post == "East" ~ "E", predir_post == "West" ~ "W",
    predir_post == "Northeast" ~ "NE", predir_post == "Northwest" ~ "NW",
    predir_post == "Southeast" ~ "SE", predir_post == "Southwest" ~ "SW",
    T ~ predir,
  )) %>%
  select(-predir_post) %>%
  distinct(predir, street, postdir, .keep_all = T)

### Parse suffix(es)
sfx <- na.omit(c(etmstuff::street_sfx$full, etmstuff::street_sfx$abbr))
pattern <- paste0("(?i)", paste0("\\s", sfx, "$", collapse = "|"))
df_geo$suffix2 <- str_squish(str_extract(df_geo$street, pattern))
df_geo$street <- str_squish(str_remove(df_geo$street, pattern))
df_geo$suffix1 <- str_squish(str_extract(df_geo$street, pattern))
df_geo$street <- str_squish(str_remove(df_geo$street, pattern))

### Filter out remaining streets that contain suffix elements
pattern <- paste0("(?i)", paste0("\\b", sfx, "\\b", collapse = "|"))
df <- df_geo %>%
  filter(str_detect(street, pattern))

### Clean street names
df <- df %>%
  mutate(
    street = str_replace(street, "^Street", "St"),
    street = str_squish(str_replace(street, "\\(P\\)", ""))
  )
df_geo <- etmstuff::replace_values(df_geo, "street", df, "street")
df <- df_geo %>%
  filter(str_detect(street, "[:upper:]\\d|\\d[:upper:]"))
df_geo <- df_geo %>%
  filter(!address_id %in% c(
    "AD00006", "AD04869", "AD04870", "AD04871",
    "AD06330", "AD06451", "AD09301", "AD09302"
  ))

### Reorder columns and deduplicate
df_geo <- df_geo %>%
  select(address_id, full_street, predir, street, suffix1, suffix2, postdir) %>%
  distinct(predir, street, suffix1, suffix2, postdir, .keep_all = T)

### Recombine components
f <- function(v) paste(na.omit(c(v)), collapse = " ")
df_geo$recombined_street <- apply(
  df_geo %>%
    select(predir:postdir),
  MARGIN = 1, FUN = f
)

street_names <- df_geo$recombined_street

# ## Street names from Washington Post article
# ## source: https://www.washingtonpost.com/blogs/govbeat/wp/2015/03/06/these-are-the-most-popular-street-names-in-every-state/
#
# df_wapo <- read.csv("data-raw/street_names_wapo.csv", col.names = "name")
#
# street_names_wapo <- str_split_i(df_wapo$name, ",", 1)
#
# street_names_wapo <- sort(unique(str_squish(na.omit(c(
#   str_split_i(street_names_wapo, "/", 1),
#   str_split_i(street_names_wapo, "/", 2)
# )))))
#
# street_names_wapo <- street_names_wapo[which(!str_detect(street_names_wapo, "^N\\s|^S\\s"))]
#
# ### Get street names from `street_names_wapo` that don't appear in `street_names_geo`
# street_names_geo <- sort(unique(df_geo$street))
# pattern <- paste0(street_names_wapo, collapse = "|")
# wapo_in_geo <- unique(str_extract(street_names_geo, pattern))
# wapo_unq <- street_names_wapo[which(!street_names_wapo %in% wapo_in_geo)]
#
# ### Combine both sets of street names
# street_names <- sort(unique(c(street_names_geo, street_names_wapo)))

usethis::use_data(street_names, overwrite = T)
