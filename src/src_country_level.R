# This script sources country-level data.

library(tidyverse)
library(rio)
library(here)

# Population
population_raw <- wbstats::wb_data(indicator = "SP.POP.TOTL", 
                                   return_wide = FALSE) %>% 
  select(country, year = date, population = value)

# GDP
gdp_raw <- wbstats::wb_data(indicator = "NY.GDP.MKTP.CD", 
                            return_wide = FALSE) %>% 
  select(country, year = date, gdp_current = value)

# Military expenditure
milex_raw <- import("https://sipri.org/sites/default/files/SIPRI-Milex-data-1949-2020_0.xlsx", 
                    sheet = "Current USD",
                    skip = 4) %>% 
  pivot_longer(!Country:Notes, names_to = "year", values_to = "milex") %>% 
  mutate(across(year:milex, as.numeric),
         milex = milex * 1000000) %>% 
  select(country = Country, year, milex)

# Unified Democracy Scores
uds_raw <- read_csv("http://www.unified-democracy-scores.net/files/20140312/z/uds_summary.csv.gz") %>% 
  select(country, year, uds = mean)

# Checks and balances
checks_raw <- import(here("data-raw", "DPI2020.dta")) %>% 
  select(country = countryname, year, checks) %>% 
  haven::zap_label()

# Civil war 
civil_war_raw <- import("https://ucdp.uu.se/downloads/ucdpprio/ucdp-prio-acd-211-RData.zip") %>% 
  filter(type_of_conflict %in% 3:4) %>% 
  mutate(civil_war = 1) %>% 
  select(conflict_id, country = side_a, year, civil_war)
