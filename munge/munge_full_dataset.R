# This script combines all group- and country-level data.

library(tidyverse)
library(rio)
library(here)

full_df <- import(here("data", "group_level.csv")) %>% 
  left_join(import(here("data", "country_level.csv"))) %>% 
  mutate(ccode = countrycode::countrycode(country, "country.name", "cown")) %>% 
  relocate(ccode, .after = country) %>% 
  arrange(kgcid, year)

export(full_df, here("data", "full_dataset.csv"))
