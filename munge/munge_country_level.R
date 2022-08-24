# This script munges country-level data. 

library(tidyverse)
library(countrycode)
library(lubridate)

# Scope of the country-level dataset
scope <- distinct(sRdpPrivateData::groups, country) |> 
  crossing(year = 1960:2020)

# Population
population <- population_raw %>% 
  right_join(scope) %>% 
  ungroup()

# GDP
gdp <- gdp_raw %>% 
  mutate(country = countrycode(country, "country.name", "country.name")) %>% 
  select(country, year = date, gdp = value) %>% 
  right_join(scope) %>% 
  ungroup()

# Military expenditure
milex <- milex_raw %>%
  mutate(country = countrycode(country, "country.name", "country.name", custom_match = c("German DR" = "German Democratic Republic"))) %>% 
  drop_na(milex) %>% 
  right_join(scope) %>% 
  ungroup()

# Unified Democracy Scores
uds <- uds_raw %>% 
  mutate(country = case_when(country == "Yugoslavia (Serbia)" & year < 1993 ~ "Yugoslavia",
                             country == "Yugoslavia (Serbia)" & year >= 1993 ~ "Serbia", 
                             TRUE ~ country),
         country = countrycode(country, "country.name", "country.name")) %>% 
  right_join(scope) %>% 
  ungroup()

# Checks and balances
checks <- checks_raw %>% 
  filter(country != "Turk Cyprus") %>%
  mutate(country = countrycode(country, "country.name", "country.name",
                               custom_match = c("PRC" = "China", "S. Africa" = "South Africa",
                                                "Dom. Rep." = "Dominican Republic", 
                                                "GDR" = "German Democratic Republic",
                                                "ROK" = "South Korea",
                                                "PRK" = "North Korea",
                                                "Cent. Af. Rep." = "Central African Republic",
                                                "P. N. Guinea" = "Papua New Guinea")),
         checks = na_if(checks, -999)) %>% 
  drop_na(checks) %>% 
  right_join(scope) %>% 
  ungroup()

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
  mutate(civil_war = replace_na(civil_war, 0)) %>% 
  ungroup()

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
  mutate(civil_war_onset = replace_na(civil_war_onset, 0)) %>% 
  ungroup()

# Civil war in previous year
civil_war_prev_yr <- civil_war_raw %>%
  distinct(country, year) %>% 
  mutate(country = str_remove(country, "Government of "),
         country = case_when(country == "Hyderabad" ~ "India", 
                             country == "Serbia (Yugoslavia)" & year < 1993 ~ "Yugoslavia",
                             country == "Serbia (Yugoslavia)" & year >= 1993 ~ "Serbia", 
                             TRUE ~ country),
         country = countrycode(country, "country.name", "country.name"),
         year = as.numeric(year), 
         civil_war = 1) %>% 
  arrange(country, year) %>% 
  group_by(country) %>% 
  mutate(civil_war_prev_yr = if_else(lag(civil_war) == 1, 1, 0),
         civil_war_prev_yr = if_else(year != 1946, replace_na(civil_war_prev_yr, 0), civil_war_prev_yr)) %>% # account for start of dataset
  select(-civil_war) %>% 
  ungroup() %>% 
  right_join(scope) %>% 
  mutate(civil_war_prev_yr = replace_na(civil_war_prev_yr, 0)) %>% 
  ungroup()

# Freedom House
fh <- fh_raw %>% 
  mutate(country = countrycode(fh_country, "country.name", "country.name", 
                               custom_match = c("Micronesia" = "Micronesia (Federated States of)"))) %>% 
  select(country, year, fh_political_rights = pr, fh_civil_liberties = cl, fh_status = status) %>% 
  right_join(scope) %>% 
  ungroup()

# PolityV
polity <- polity_raw %>% 
  mutate(country = countrycode(polity_annual_country, "country.name", "country.name"),
         across(democ:polity, ~if_else(.x < -10, NA_real_, .x))) %>% 
  select(country, year, polity_democracy = democ, polity_autocracy = autoc, polity_total = polity) %>% 
  distinct(country, year, polity_democracy, polity_autocracy, polity_total) %>% 
  filter(country != "Sudan" | (country == "Sudan" & year == 2011 & polity_democracy == 1)) %>% 
  right_join(scope) %>% 
  ungroup()

# Elections
elections <- elections_raw %>% 
  mutate(year = year(date), 
         type = str_to_lower(type),
         event = 1) %>% 
  group_by(country, year, type) %>%
  summarise(event = sum(event)) %>% 
  pivot_wider(names_from = type, values_from = event) %>% 
  right_join(scope) %>% 
  mutate(across(election:referendum, ~replace_na(.x, 0))) %>% 
  ungroup()

# Federalism
federal_countries <- federal_countries_raw %>% 
  mutate(country = countrycode(country, "country.name", "country.name"),
         federal = 1) %>% 
  distinct(country, federal) %>% 
  right_join(scope) %>% 
  mutate(federal = replace_na(federal, 0)) %>% 
  ungroup()
  
# Compile country-level data
df <- scope %>% 
  left_join(civil_war) %>% 
  left_join(civil_war_onset) %>% 
  left_join(civil_war_prev_yr) %>% 
  left_join(milex) %>% 
  left_join(population) %>% 
  left_join(gdp) %>% 
  left_join(uds) %>% 
  left_join(checks) %>% 
  left_join(fh) %>% 
  left_join(polity) %>% 
  left_join(elections)

rio::export(df, here::here("data", "country_level.csv"))
