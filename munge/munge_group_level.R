# This script cleans the group-level data

library(tidyverse)

# Scope -------------------------------------------------------------------

groups <- sRdpPrivateData::groups

# Country-level data ------------------------------------------------------

country_level <- rio::import(here::here("data", "00_country_level.csv"))

# Relative size -----------------------------------------------------------

# Get key between KGCID and EPR groupid
epr_to_kgc <- import(here("data-raw", "EPR to KGC.csv")) |> 
  filter(!is.na(kgcid),
         # Remove duplicate
         cowgroupid != 36532000) |> 
  distinct(kgcid, gwgroupid = cowgroupid)

# Clean and organise EPR relative size data 
epr_relative_size <- rio::import(here::here("data-raw", "relative_size_raw.csv")) |> 
  janitor::clean_names() |> 
  pivot_longer(from:to, values_to = "year") |> 
  group_by(gwgroupid, epr_relative_size = size) |> 
  expand(year = full_seq(year, 1)) |> 
  # Filter for SRDP groups
  right_join(epr_to_kgc, by = "gwgroupid") |> 
  ungroup() |> 
  # Join to full group-level data
  select(-gwgroupid) |> 
  right_join(groups, by = c("year", "kgcid"))
  
# Complete missing data using MAR estimates

# Get key between KGCID and MAR groupid
mar_to_kgc <- import(here("data-raw", "mar_to_kgc.csv")) |> 
  select(kgcid, numcode)

mar_relative_size <- import("http://www.mar.umd.edu/data/marupdate_20042006.csv") |> 
  janitor::clean_names() |> 
  select(numcode, year, gpop, cpop) |> 
  # Filter for SRDP groups
  right_join(mar_to_kgc, by = "numcode") |> 
  select(-numcode) |> 
  # Get relative size
  mutate(mar_relative_size = gpop / cpop)

# Join together and replace missing data with MAR estimates
relative_size <- epr_relative_size |> 
  left_join(mar_relative_size, by = c("year", "kgcid"))



epr_size <- epr_relative_size %>% 
  left_join(import(here("data", "00_country_level.csv"))) %>% 
  mutate(epr_size = relative_size * population) %>% 
  select(kgcid, group, country, year, epr_size)

mar_relative_size <- import("http://www.mar.umd.edu/data/marupdate_20042006.csv") %>% 
  janitor::clean_names() %>% 
  left_join(mar_to_kgc) %>% 
  drop_na(kgcid) %>% 
  select(kgcid, year, gpro, gpop) %>% 
  right_join(group_scope) %>% 
  group_by(kgcid) %>% 
  mutate(mar_average = mean(gpop, na.rm = TRUE)) %>% 
  rowwise() %>% 
  mutate(gpop = replace_na(gpop, mar_average)) %>% 
  left_join(import(here("data", "00_country_level.csv"))) %>% 
  mutate(mar_relative_size = gpop / population) %>% 
  select(kgcid, year, mar_size = gpop, mar_relative_size)

relative_size <- epr_relative_size %>% 
  left_join(mar_relative_size) %>% 
  rowwise() %>% 
  mutate(relative_size = replace_na(mar_relative_size),
         relative_size = round(relative_size, 2)) %>% 
  select(-mar_relative_size, -mar_size)

size <- epr_size %>% 
  left_join(mar_relative_size) %>% 
  rowwise() %>% 
  mutate(size = replace_na(epr_size, mar_size), 
         size = round(size)) %>% 
  select(kgcid:year, size)

# Religion
religion <- religion_raw %>% 
  select(kgcid, religion_1, religion_2)

# Group-level data
df <- group_scope %>% 
  left_join(relative_size) %>% 
  left_join(size) %>% 
  left_join(religion)

export(df, here("data", "group_level.csv"))

