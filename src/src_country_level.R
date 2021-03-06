# This script sources country-level data.

library(tidyverse)
library(rio)
library(here)
library(countrycode)
library(democracyData)

# SRDP membership
srdp_country_membership <- import("/Users/harrietgoers/Documents/github/SRDP/db_migration/data-raw/SRDP_Mvmt_2019_release.dta") %>% 
  distinct(country) %>% 
  arrange(country)

# Population
population_raw <- import("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx", 
       sheet = "ESTIMATES", 
       skip = 16) %>% 
  pivot_longer(!(Index:`Parent code`), names_to = "year", values_to = "population") %>% 
  janitor::clean_names() %>% 
  mutate(across(year:population, as.numeric),
         country = countrycode::countrycode(country_code, "iso3n", "country.name"), 
         population = population * 1000) %>% # convert to raw value
  select(country, year, population) %>% 
  drop_na(country) %>% 
  # fill missing data with World Bank data
  full_join(wbstats::wb_data(indicator = "SP.POP.TOTL", return_wide = FALSE) %>% 
              mutate(country = countrycode(country, "country.name", "country.name")) %>% 
              select(country, year = date, wb_population = value)) %>% 
  rowwise() %>% 
  mutate(population = replace_na(population, wb_population)) %>% 
  select(-wb_population)

# GDP
gdp_raw <- wbstats::wb_data(indicator = "NY.GDP.MKTP.CD", return_wide = FALSE)

# Military expenditure
milex_raw <- import("https://sipri.org/sites/default/files/SIPRI-Milex-data-1949-2020_0.xlsx", 
                    sheet = "Current USD",
                    skip = 4) %>% 
  pivot_longer(!Country:Notes, names_to = "year", values_to = "milex") %>% 
  mutate(across(year:milex, as.numeric),
         milex = milex * 1000000) %>% # convert to raw score
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
  filter(type_of_conflict %in% 3:4) %>% # only include civil wars
  select(conflict_id, country = side_a, year)

# Democracy
fh_raw <- download_fh()

polity_raw <- download_polity_annual()

# Elections
elections_raw <- import("/Users/harrietgoers/Documents/github/election_dates_database/data/election_df.csv") %>% # fix this
  filter(country != "European Union") %>% 
  mutate(country = countrycode(country, "country.name", "country.name",
                               custom_match = c("Autonomous Community of Andalusia" = "Spain", 
                                                "Autonomous Community of Galicia" = "Spain", 
                                                "Basque Country" = "Spain",
                                                "Catalunya (Comunitat autònoma d'Espanya)" = "Spain", 
                                                "Cooperative Republic of Guyana" = "Guyana",
                                                "Corsica" = "France", 
                                                "Northern Ireland" = "United Kingdom", 
                                                "Scotland" = "United Kingdom", 
                                                "Wales" = "United Kingdom")))

# Federal - To be completed
federal_countries_raw <- tibble(
  
  country = c("Argentina", "Australia", "Austria", "Belgium", 
              "Bosnia and Herzegovina", "Brazil", "Canada", 
              "Comoros", "Ethiopia", "Germany", "India", "Malaysia", 
              "Mexico", "Micronesia (Federated States of)",
              "Nigeria", "Pakistan", "Russia", "St. Kitts and Nevis",
              "Serbia and Montenegro", "South Africa", "Spain", 
              "Switzerland", "United Arab Emirates", "United States", 
              "Venezuela"),
  source = "Handbook of federal countries, 2005"
  
) %>% 
  bind_rows(
    
    tibble(
      
      country = c("United Arab Emirates", "Argentina", "Canada", "Nepal",
                  "Pakistan", "Russia", "Australia", "Germany", "India",
                  "Malaysia", "Ethiopia", "Nigeria", "United States", 
                  "Venezuela", "Belgium", "Bosnia and Herzegovina", "Iraq", 
                  "South Africa",
                  "Spain"),
      source = "Other"
      
    )
    
  )

federal_countries <- c("Argentina", "Australia", "Austria", "Belgium", "Bosnia and Herzegovina",
                       "Brazil", "Canada", "Comoros", "Ethiopia", "Germany", "India", 
                       "Malaysia", "Mexico", "Micronesia (Federated States of)", 
                       "Nigeria", "Pakistan", "Russia", "St. Kitts and Nevis", 
                       "Serbia and Montenegro", "South Africa", "Spain", "Switzerland", 
                       "United Arab Emirates", "United States", "Venezuela", "Nepal", 
                       "Iraq")