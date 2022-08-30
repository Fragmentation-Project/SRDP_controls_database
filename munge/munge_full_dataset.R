# This script combines all group- and country-level data.

library(tidyverse)

# Read in datasets --------------------------------------------------------
country_level <- rio::import(here::here("data", "00_country_level.csv"))
group_level <- rio::import(here::here("data", "00_group_level.csv"))

# Construct full dataset --------------------------------------------------

full_df <- sRdpPrivateData::groups |> 
  left_join(group_level, by = c("kgcid", "group", "country", "year")) |> 
  left_join(country_level, by = c("country", "year"))
  
rio::export(full_df, here("data", "00_full_dataset.csv"))
