# Explore the variety of unit strings

library(tidyverse)

## Import addresses
df_md1 <- readRDS("~/r_projects/bl_2015-2022/data/addresses/df_md_val1_2015-2019.rds")
df_md2 <- readRDS("~/r_projects/bl_2015-2022/data/addresses/df_md_val1_2020-2022.rds")

df_md <- df_md1 %>%
  bind_rows(df_md2)

rm(df_md1, df_md2)

## Deduplicate addresses
df_md <- df_md %>%
  distinct(pick(AddressDeliveryInstallation:Suite), .keep_all = T)

## Percentage of addresses with unit information present
n_with_unit <- nrow(df_md[which(!is.na(df_md$Suite)),])
etmstuff::pct(n_with_unit, nrow(df_md))

## Pull units
df_units <- df_md %>%
  select(Suite) %>%
  drop_na() %>%
  arrange(Suite)

## Parse unit prefix and number
df_units$prefix <- str_extract(df_units$Suite, "^[:alpha:]+")
df_units$remainder <- str_squish(str_remove(df_units$Suite, "^[:alpha:]+"))

df <- df_units %>%
  filter(str_detect(remainder, "\\s"))

df_units <- df_units %>%
  filter(!str_detect(remainder, "\\s")) %>%
  rename(number = remainder)

## Prefix
df_summarize_prefix <- df_units %>%
  group_by(prefix) %>%
  count() %>%
  mutate(pct = etmstuff::pct(n, nrow(df_units)))

## Number
df_summarize_number <- df_units %>%
  group_by(
    num_only = str_detect(number, "^\\d+$"),
    letter_only = str_detect(number, "^[:alpha:]+$")
  ) %>%
  count() %>%
  mutate(pct = etmstuff::pct(n, nrow(df_units)))








df <- df_units %>%
  filter(!str_detect(number, "^\\d+$"))










