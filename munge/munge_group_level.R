# This script munges the group-level data

library(tidyverse)
library(rio)
library(here)
library(RSQLite)
library(DBI)
source("src/src_group_level.R")

# Scope
group_scope <- dbConnect(RSQLite::SQLite(), "/Users/harrietgoers/Documents/github/SRDP/db_migration/data/portal-database-output.sqlite") %>% 
  dbSendQuery("SELECT * FROM groups") %>% 
  dbFetch() %>% 
  mutate(endyear = replace_na(endyear, 2020)) %>% 
  pivot_longer(startyear:endyear, values_to = "year") %>% 
  group_by(kgcid = groupid, groupname, country) %>% 
  expand(year = full_seq(year, 1)) %>% 
  ungroup()

# Country-level data
country_level <- import(here("data", "country_level.rds"))

# (Relative) size
epr_to_kgc <- import(here("data-raw", "EPR to KGC.csv")) %>% 
  distinct(gwgroupid = cowgroupid, kgcid) %>% 
  filter(gwgroupid != 36532000) %>% # remove duplicate
  right_join(group_scope %>% 
               distinct(kgcid, groupname, country))

mar_to_kgc <- import(here("data-raw", "mar_to_kgc.csv"))

epr_relative_size <- relative_size_raw %>% 
  mutate(id = row_number()) %>% 
  pivot_longer(from:to, values_to = "year") %>% 
  group_by(id, gwgroupid, group, country = statename, relative_size = size) %>% 
  expand(year = full_seq(year, 1)) %>% 
  right_join(epr_to_kgc) %>% 
  right_join(group_scope) %>% 
  ungroup() %>% 
  select(kgcid, groupname, country, year, relative_size)

epr_size <- epr_relative_size %>% 
  left_join(import(here("data", "country_level.rds"))) %>% 
  mutate(epr_size = round(relative_size * population)) %>% 
  select(kgcid, year, epr_size)

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
  left_join(import(here("data", "country_level.rds"))) %>% 
  mutate(mar_relative_size = gpop / population) %>% 
  select(kgcid, year, mar_size = gpop, mar_relative_size)

relative_size <- epr_relative_size %>% 
  left_join(mar_relative_size) %>% 
  rowwise() %>% 
  mutate(relative_size = replace_na(mar_relative_size)) %>% 
  select(-mar_relative_size)

size <- epr_size %>% 
  left_join(mar_relative_size) %>% 
  rowwise() %>% 
  mutate(size = replace_na(epr_size, mar_size))
