# This script sources country-level data.

library(tidyverse)
library(rio)
library(here)
library(countrycode)
library(democracyData)

# Population
population_raw <- import("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx", 
       sheet = "ESTIMATES", 
       skip = 16) %>% 
  pivot_longer(!(Index:`Parent code`), names_to = "year", values_to = "population") %>% 
  janitor::clean_names() %>% 
  mutate(across(year:population, as.numeric),
         country = countrycode::countrycode(country_code, "iso3n", "country.name"), 
         population = population * 1000) %>% 
  select(country, year, population) %>% 
  drop_na(country) %>% 
  # fill missing data with World Bank data
  full_join(wbstats::wb_data(indicator = "SP.POP.TOTL", return_wide = FALSE) %>% 
              mutate(country = countrycode(country, "country.name", "country.name")) %>% 
              select(country, year = date, wb_population = value)) %>% 
  rowwise() %>% 
  mutate(population = replace_na(population, wb_population)) %>% 
  select(-wb_population)

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
  select(conflict_id, country = side_a, year)

# Democracy
fh_raw <- download_fh()

polity_raw <- download_polity_annual()
