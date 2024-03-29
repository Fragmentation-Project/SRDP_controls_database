# This script sources country-level data.

library(rio)
library(tidyverse)
library(countrycode)

# Population --------------------------------------------------------------
# Population data is collected from UN Population. This is found in their
# standard projections > demographic indicators > compact (most used estimates
# and medium projections).

population_raw <- import(
  "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_General/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx",
  sheet = "Estimates",
  skip = 16
)

export(population_raw, here::here("data-raw", "population_raw.csv"))

# GDP ---------------------------------------------------------------------
# GDP data is collected from the World Bank. 

gdp_raw <- wbstats::wb_data(indicator = "NY.GDP.MKTP.CD", return_wide = FALSE)

export(gdp_raw, here::here("data-raw", "gdp_raw.csv"))

# Military expenditure ----------------------------------------------------

milex_raw <- import("https://sipri.org/sites/default/files/SIPRI-Milex-data-1949-2020_0.xlsx", 
                    sheet = "Current USD",
                    skip = 4)

export(milex_raw, here::here("data-raw", "milex_raw.rds"))

# Unified Democracy Scores ------------------------------------------------

uds_raw <- import("https://clio-infra.eu/data/UnifiedDemocracyScores(UDS)_Compact.xlsx")

export(uds_raw, here::here("data-raw", "uds_raw.csv"))

# Checks and balances -----------------------------------------------------
# Data collected from here: https://datacatalog.worldbank.org/search/dataset/0039819

checks_raw <- import(here::here("data-raw", "DPI2020.dta"))

# Civil war  --------------------------------------------------------------

civil_war_raw <- import("https://ucdp.uu.se/downloads/ucdpprio/ucdp-prio-acd-211-RData.zip") 

export(civil_war_raw, here::here("data-raw", "civil_raw.csv"))

# Freedom House -----------------------------------------------------------

fh_raw <- democracyData::download_fh()

export(fh_raw, here::here("data-raw", "democracy_fh.csv"))

# Polity ------------------------------------------------------------------

polity_raw <- democracyData::download_polity_annual()

export(polity_raw, here::here("data-raw", "democracy_polity.csv"))

# Elections ---------------------------------------------------------------
# TODO: Make accessible to others
elections_raw <- import("https://raw.githubusercontent.com/hgoers/election_dates_database/master/data/election_df.csv")

export(elections_raw, here::here("data-raw", "elections.csv"))

# Federal - To be completed -----------------------------------------------

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