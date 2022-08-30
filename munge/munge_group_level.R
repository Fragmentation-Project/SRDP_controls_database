# This script cleans the group-level data

library(tidyverse)

# Relative size -----------------------------------------------------------

# Get key between KGCID and EPR groupid
epr_to_kgc <- rio::import(here::here("data-raw", "EPR to KGC.csv")) |> 
  filter(!is.na(kgcid),
         # Remove duplicate
         cowgroupid != 36532000) |> 
  distinct(kgcid, gwgroupid = cowgroupid)

# Clean and organise EPR relative size data 
epr_relative_size <- rio::import(here::here("data-raw", "relative_size_raw.csv")) |> 
  janitor::clean_names() |> 
  pivot_longer(from:to, values_to = "year") |>
  distinct(gwgroupid, size, year) |> 
  # Create ID to deal with grouping issues
  mutate(id = row_number()) |> 
  group_by(id, gwgroupid, relative_size = size) |> 
  expand(year = full_seq(year, 1)) |> 
  # Filter for SRDP groups
  right_join(epr_to_kgc, by = "gwgroupid") |> 
  ungroup() |> 
  # Join to full group-level data
  select(-id, -gwgroupid) |> 
  right_join(sRdpPrivateData::groups, by = c("year", "kgcid"))
  
# Complete missing data using MAR estimates
# Get key between KGCID and MAR groupid
mar_to_kgc <- rio::import(here::here("data-raw", "mar_to_kgc.csv")) |> 
  select(kgcid, numcode)

mar_relative_size <- rio::import("http://www.mar.umd.edu/data/marupdate_20042006.csv") |> 
  janitor::clean_names() |> 
  select(numcode, year, gpop, cpop) |> 
  # Filter for SRDP groups
  right_join(mar_to_kgc, by = "numcode") |> 
  # Get relative size
  transmute(kgcid, year, mar_relative_size = gpop / cpop)
  
# Join together and replace missing data with MAR estimates
relative_size <- epr_relative_size |> 
  left_join(mar_relative_size, by = c("year", "kgcid")) |> 
  mutate(relative_size = coalesce(relative_size, mar_relative_size)) |> 
  select(kgcid:country, year, relative_size)

# Religion ----------------------------------------------------------------

religion <- rio::import(here::here("data-raw", "religion_raw.csv")) |> 
  select(kgcid, religion_1, religion_2) |> 
  mutate(religion_2 = na_if(religion_2, ""))

# Group-level data --------------------------------------------------------

group_level <- sRdpPrivateData::groups |>  
  left_join(relative_size, by = c("kgcid", "group", "country", "year")) |> 
  left_join(religion, by = "kgcid")

rio::export(group_level, here::here("data", "00_group_level.csv"))
