# This script combines all group- and country-level data.

library(tidyverse)
library(rio)
library(here)

full_df <- import(here("data", "group_level.csv")) %>% 
  left_join(import(here("data", "country_level.csv"))) %>% 
  arrange(kgcid, year)

export(full_df, here("data", "full_dataset.csv"))
%>% 