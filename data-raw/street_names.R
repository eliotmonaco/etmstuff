# Generates `street_names.rda`

## source: https://geographic.org/streetview/usa/ks/index.html

df_geo <- read.csv("data-raw/street_names_geo.csv")

df_geo <- df_geo |>
  tibble::as_tibble() |>
  dplyr::mutate(dplyr::across(tidyselect::everything(), ~ stringr::str_squish(stringr::str_remove(.x, "\\s\\d{5}$"))))

df_geo[df_geo == ""] <- NA

street_names_geo <- c()

# Combine separate columns into one
for (i in 1:ncol(df_geo)) {
  street_names_geo <- sort(unique(na.omit(c(df_geo[[i]], street_names_geo))))
}

df_geo <- tibble::tibble(full_street = street_names_geo)

# Add `address_id`
df_geo <- df_geo |>
  id_distinct_rows(id_name = "address_id", prefix = "AD", var = "full_street") |>
  dplyr::relocate(address_id)

# Parse predirectional
pattern <- paste0("^", directions$abbr, "\\s", collapse = "|")
df_geo$predir <- stringr::str_squish(stringr::str_extract(df_geo$full_street, pattern))
df_geo$street <- stringr::str_squish(stringr::str_remove(df_geo$full_street, pattern))

# Parse predirectional placed at end of string
pattern <- paste0("(?i)", paste0(",\\s", directions$full, "$", collapse = "|"))
df_geo$predir_post <- stringr::str_squish(stringr::str_remove(stringr::str_extract(df_geo$street, pattern), ","))
df_geo$street <- stringr::str_squish(stringr::str_remove(df_geo$street, pattern))

# Parse postdirectional
pattern <- paste0("\\s", c("N", "S", "E", "W"), "$", collapse = "|")
df_geo$postdir <- stringr::str_squish(stringr::str_remove(stringr::str_extract(df_geo$street, pattern), ","))
df_geo$street <- stringr::str_squish(stringr::str_remove(df_geo$street, pattern))

# Replace postdirectional with predirectional
df_geo |>
  dplyr::group_by(predir_post) |>
  dplyr::count()
df_geo <- df_geo |>
  dplyr::mutate(predir = dplyr::case_when(
    predir_post == "North" ~ "N", predir_post == "South" ~ "S",
    predir_post == "East" ~ "E", predir_post == "West" ~ "W",
    predir_post == "Northeast" ~ "NE", predir_post == "Northwest" ~ "NW",
    predir_post == "Southeast" ~ "SE", predir_post == "Southwest" ~ "SW",
    T ~ predir,
  )) |>
  dplyr::select(-predir_post) |>
  dplyr::distinct(predir, street, postdir, .keep_all = T)

# Parse suffix(es)
sfx <- na.omit(c(street_sfx$full, street_sfx$abbr))
pattern <- paste0("(?i)", paste0("\\s", sfx, "$", collapse = "|"))
df_geo$suffix2 <- stringr::str_squish(stringr::str_extract(df_geo$street, pattern))
df_geo$street <- stringr::str_squish(stringr::str_remove(df_geo$street, pattern))
df_geo$suffix1 <- stringr::str_squish(stringr::str_extract(df_geo$street, pattern))
df_geo$street <- stringr::str_squish(stringr::str_remove(df_geo$street, pattern))

# Filter out remaining streets that contain suffix elements
pattern <- paste0("(?i)", paste0("\\b", sfx, "\\b", collapse = "|"))
df <- df_geo |>
  dplyr::filter(stringr::str_detect(street, pattern))

# Clean street names
df <- df |>
  dplyr::mutate(
    street = stringr::str_replace(street, "^Street", "St"),
    street = stringr::str_squish(stringr::str_replace(street, "\\(P\\)", ""))
  )
df_geo <- replace_values(df_geo, "street", df, "street")
df <- df_geo |>
  dplyr::filter(stringr::str_detect(street, "[:upper:]\\d|\\d[:upper:]"))
df_geo <- df_geo |>
  dplyr::filter(!address_id %in% c(
    "AD00006", "AD04869", "AD04870", "AD04871",
    "AD06330", "AD06451", "AD09301", "AD09302"
  ))

# Reorder columns and deduplicate
df_geo <- df_geo |>
  dplyr::select(address_id, full_street, predir, street, suffix1, suffix2, postdir) |>
  dplyr::distinct(predir, street, suffix1, suffix2, postdir, .keep_all = T)

# Recombine components
f <- function(v) paste(na.omit(c(v)), collapse = " ")
df_geo$recombined_street <- apply(
  df_geo |>
    dplyr::select(predir:postdir),
  MARGIN = 1, FUN = f
)

street_names <- df_geo$recombined_street

usethis::use_data(street_names, overwrite = T)
