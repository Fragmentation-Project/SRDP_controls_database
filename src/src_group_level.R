# This script sources group-level control data.

library(tidyverse)
library(rio)
library(RSQLite)
library(DBI)

# Group scope
groups_raw <- dbConnect(RSQLite::SQLite(), "/Users/harrietgoers/Documents/github/SRDP/db_migration/data/portal-database-output.sqlite") %>% # to be migrated to web-based storage
  dbSendQuery("SELECT * FROM groups") %>% 
  dbFetch()

# Size and relative size
relative_size_raw <- import("https://icr.ethz.ch/data/epr/core/EPR-2021.csv")

# Religion
religion_raw <- googlesheets4::range_read("1Q6jeejbdmoNmnQFgma_BgH8BtEBdkFHzSsNBdLPomWY") %>% 
  janitor::clean_names()
