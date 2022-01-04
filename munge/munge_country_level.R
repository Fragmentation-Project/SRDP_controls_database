# This script munges country-level data. 

library(tidyverse)
library(countrycode)
source("src/src_country_level.R")

# SRDP scope
scope <- import("https://correlatesofwar.org/data-sets/state-system-membership/states2016/at_download/file") %>% 
  pivot_longer(c(styear, endyear), values_to = "year") %>% 
  # update dataset to current year (no new states from 2016 to 2020)
  mutate(year = as.double(year), 
         year = case_when(statenme == "Yugoslavia" & year == 2016 ~ 1992,
                          year == 2016 ~ 2020,
                          TRUE ~ year)) %>% 
  group_by(ccode) %>% 
  expand(year = full_seq(year, 1)) %>% 
  # filter to SRDP coverage (from 1960)
  filter(year >= 1960) %>% 
  mutate(country = countrycode(ccode, "cown", "country.name", custom_match = c(`260` = "German Federal Republic"))) %>% 
  ungroup() %>% 
  select(country, year) 

# Population
# Note: UN and World Bank do not provide population values for Czechoslovakia, 
# Yugoslavia, Zanzibar, German Federal Republic, German Democratic Republic, 
# Yemen Arab Republic, Yemen People's Republic, Republic of Vietnam
population <- population_raw %>% 
  right_join(scope)

# Military expenditure
milex <- milex_raw %>% 
  # filter out regions and standardize names
  mutate(country = countrycode(country, "country.name", "country.name", custom_match = c("German DR" = "German Democratic Republic"))) %>% 
  right_join(scope)

skimr::skim(milex)

# Unified Democracy Scores
uds <- uds_raw %>% 
  mutate(country = case_when(country == "Yugoslavia (Serbia)" & year < 1993 ~ "Yugoslavia",
                             country == "Yugoslavia (Serbia)" & year >= 1993 ~ "Serbia", 
                             TRUE ~ country),
         country = countrycode(country, "country.name", "country.name")) %>% 
  right_join(scope)

skimr::skim(uds)

# Checks and balances
checks <- checks_raw %>% 
  mutate(country = countrycode(country, "country.name", "country.name",
                               custom_match = c("PRC" = "China", "S. Africa" = "South Africa",
                                                "Dom. Rep." = "Dominican Republic", 
                                                "GDR" = "German Democratic Republic",
                                                "ROK" = "South Korea",
                                                "PRK" = "North Korea",
                                                "Cent. Af. Rep." = "Central African Republic")),
         checks = na_if(checks, -999)) %>% 
  right_join(scope)

skimr::skim(checks)

# Civil war 
civil_war <- civil_war_raw %>%
  # do not need to account for multiple civil wars in one year
  distinct(country, year) %>% 
  mutate(country = str_remove(country, "Government of "),
         country = case_when(country == "Hyderabad" ~ "India", 
                             country == "Serbia (Yugoslavia)" & year < 1993 ~ "Yugoslavia",
                             country == "Serbia (Yugoslavia)" & year >= 1993 ~ "Serbia", 
                             TRUE ~ country),
         country = countrycode(country, "country.name", "country.name"),
         year = as.numeric(year), 
         civil_war = 1) %>% 
  right_join(scope) %>% 
  mutate(civil_war = replace_na(civil_war, 0))

# Civil war onset
civil_war_onset <- civil_war_raw %>% 
  mutate(country = str_remove(country, "Government of "),
         country = case_when(country == "Hyderabad" ~ "India", 
                             country == "Serbia (Yugoslavia)" & year < 1993 ~ "Yugoslavia",
                             country == "Serbia (Yugoslavia)" & year >= 1993 ~ "Serbia", 
                             TRUE ~ country),
         country = countrycode(country, "country.name", "country.name"),
         year = as.numeric(year)) %>%
  arrange(country, year) %>% 
  group_by(conflict_id, country) %>% 
  mutate(civil_war_onset = case_when(is.na(lag(year)) ~ 1, # no previous year indicates onset
                                     year - lag(year) > 2 ~ 1, 
                                     TRUE ~ 0)) %>% # check whether the previously listed year is within two years
  group_by(country, year) %>% 
  summarise(civil_war_onset = if_else(sum(civil_war_onset) == 0, 0, 1)) %>% 
  ungroup() %>% 
  right_join(scope) %>% 
  mutate(civil_war_onset = replace_na(civil_war_onset, 0))

# Civil war in previous year
civil_war_prev_yr <- civil_war %>%
  arrange(country, year) %>% 
  group_by(country) %>% 
  mutate(civil_war_prev_yr = if_else(lag(civil_war) == 1, 1, 0), 
         civil_war_prev_yr = if_else(year != 1960, replace_na(civil_war_prev_yr, 0), civil_war_prev_yr)) %>% # account for start of dataset, rather than country
  select(-civil_war) %>% 
  ungroup()
