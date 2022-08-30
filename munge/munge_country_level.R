# This script munges country-level data. 

library(rio)
library(tidyverse)
library(countrycode)

# Scope of the country-level dataset --------------------------------------

scope <- distinct(sRdpPrivateData::groups, country) |> 
  crossing(year = 1960:2020)

# Population --------------------------------------------------------------

population <- import(here::here("data-raw", "population_raw.csv")) |> 
  janitor::clean_names() |> 
  filter(type == "Country/Area") |> 
  transmute(country = countrycode::countrycode(region_subregion_country_or_area,
                                               "country.name",
                                               "country.name",
                                               custom_match = c("Türkiye" = "Turkey")), 
            year, 
            population = as.numeric(total_population_as_of_1_january_thousands) * 1000) |> 
  right_join(scope, by = c("country", "year"))

skimr::skim(population)

duplicates <- population |> 
  group_by(country, year) |> 
  summarise(n = n()) |> 
  filter(n > 1)

stopifnot(nrow(duplicates) == 0)

export(population, here::here("data", "population.csv"))

# GDP ---------------------------------------------------------------------

gdp <- import(here::here("data-raw", "gdp_raw.csv")) |> 
  transmute(country = countrycode(country, 
                                  "country.name", 
                                  "country.name",
                                  custom_match = c("Turkiye" = "Turkey")),
            year = date, 
            gdp = value) |>  
  right_join(scope, by = c("country", "year"))

skimr::skim(gdp)

duplicates <- gdp |> 
  group_by(country, year) |> 
  summarise(n = n()) |> 
  filter(n > 1)

stopifnot(nrow(duplicates) == 0)

export(gdp, here::here("data", "gdp.csv"))

# Military expenditure ----------------------------------------------------

milex <- import(here::here("data-raw", "milex_raw.rds")) |> 
  pivot_longer(!Country:Notes, names_to = "year", values_to = "milex") |> 
  # Remove region names, which are confusing countrycode
  filter(!is.na(milex),
         milex != "xxx") |> 
  transmute(country = countrycode(Country, 
                                  "country.name", 
                                  "country.name", 
                                  custom_match = c("German DR" = "German Democratic Republic")),
            year = as.numeric(year),
            # Convert to raw score
            milex = as.numeric(milex) * 1000000) |> 
  right_join(scope, by = c("country", "year"))

skimr::skim(milex)

duplicates <- milex |> 
  group_by(country, year) |> 
  summarise(n = n()) |> 
  filter(n > 1)

stopifnot(nrow(duplicates) == 0)

export(milex, here::here("data", "milex.csv"))

# Unified Democracy Scores ------------------------------------------------

uds <- import(here::here("data-raw", "uds_raw.csv")) |>  
  pivot_longer(!ccode:`country name`, names_to = "year", values_to = "uds") |> 
  transmute(country = case_when(`country name` == "Yugoslavia (Serbia)" & year < 1993 ~ "Yugoslavia",
                                `country name` == "Yugoslavia (Serbia)" & year >= 1993 ~ "Serbia",
                                TRUE ~ `country name`),
            country = countrycode(country, "country.name", "country.name"),
            year = as.numeric(year),
            uds) |> 
  right_join(scope, by = c("country", "year"))

skimr::skim(uds)

duplicates <- uds |> 
  group_by(country, year) |> 
  summarise(n = n()) |> 
  filter(n > 1)

stopifnot(nrow(duplicates) == 0)

export(uds, here::here("data", "uds.csv"))

# Checks and balances -----------------------------------------------------

checks <- import(here::here("data-raw", "DPI2020.dta")) |> 
  haven::zap_label() |> 
  filter(countryname != "Turk Cyprus") |> 
  transmute(country = countrycode(countryname, 
                                  "country.name", 
                                  "country.name",
                                  custom_match = c("PRC" = "China", 
                                                   "S. Africa" = "South Africa",
                                                   "Dom. Rep." = "Dominican Republic",
                                                   "GDR" = "German Democratic Republic",
                                                   "ROK" = "South Korea",
                                                   "PRK" = "North Korea",
                                                   "Cent. Af. Rep." = "Central African Republic",
                                                   "P. N. Guinea" = "Papua New Guinea")),
            year,
            checks = na_if(checks, -999)) |> 
  right_join(scope, by = c("country", "year"))

skimr::skim(checks)

duplicates <- checks |> 
  group_by(country, year) |> 
  summarise(n = n()) |> 
  filter(n > 1)

stopifnot(nrow(duplicates) == 0)

export(checks, here::here("data", "checks.csv"))

# Civil war ---------------------------------------------------------------

civil_war <- import(here::here("data-raw", "civil_raw.csv")) |> 
  # Only include civil wars
  filter(type_of_conflict %in% 3:4) |>  
  separate_rows(side_a, sep = ", ") |> 
  select(conflict_id, country = side_a, year) |> 
  # Do not need to account for multiple civil wars in one year
  distinct(country, year) |>  
  mutate(country = str_remove(country, "Government of "),
         country = case_when(country == "Hyderabad" ~ "India", 
                             country == "Serbia (Yugoslavia)" & year < 1993 ~ "Yugoslavia",
                             country == "Serbia (Yugoslavia)" & year >= 1993 ~ "Serbia", 
                             TRUE ~ country),
         country = countrycode(country, "country.name", "country.name"),
         year = as.numeric(year), 
         civil_war = 1) |> 
  right_join(scope, by = c("country", "year")) |> 
  mutate(civil_war = replace_na(civil_war, 0))

duplicates <- civil_war |> 
  group_by(country, year) |> 
  summarise(n = n()) |> 
  filter(n > 1)

stopifnot(nrow(duplicates) == 0)

export(civil_war, here::here("data", "civil_war.csv"))

# Civil war onset ---------------------------------------------------------

civil_war_onset <- import(here::here("data-raw", "civil_raw.csv")) |>  
  separate_rows(side_a, sep = ", ") |> 
  mutate(country = str_remove(side_a, "Government of "),
         country = case_when(country == "Hyderabad" ~ "India", 
                             country == "Serbia (Yugoslavia)" & year < 1993 ~ "Yugoslavia",
                             country == "Serbia (Yugoslavia)" & year >= 1993 ~ "Serbia", 
                             TRUE ~ country),
         country = countrycode(country, "country.name", "country.name"),
         year = as.numeric(year)) |> 
  arrange(country, year) |> 
  group_by(conflict_id, country)|> 
  # No previous year indicates onset
  mutate(civil_war_onset = case_when(is.na(lag(year)) ~ 1, 
                                     year - lag(year) > 2 ~ 1, 
                                     TRUE ~ 0)) |> 
  group_by(country, year) |> 
  summarise(civil_war_onset = if_else(sum(civil_war_onset) == 0, 0, 1)) |> 
  ungroup() |> 
  right_join(scope) |> 
  mutate(civil_war_onset = replace_na(civil_war_onset, 0))

duplicates <- civil_war_onset |> 
  group_by(country, year) |> 
  summarise(n = n()) |> 
  filter(n > 1)

stopifnot(nrow(duplicates) == 0)

export(civil_war_onset, here::here("data", "civil_war_onset.csv"))

# Civil war in previous year ----------------------------------------------

civil_war_prev_yr <- import(here::here("data-raw", "civil_raw.csv")) |> 
  separate_rows(side_a, sep = ", ") |> 
  distinct(country = side_a, year)  |>  
  mutate(country = str_remove(country, "Government of "),
         country = case_when(country == "Hyderabad" ~ "India", 
                             country == "Serbia (Yugoslavia)" & year < 1993 ~ "Yugoslavia",
                             country == "Serbia (Yugoslavia)" & year >= 1993 ~ "Serbia", 
                             TRUE ~ country),
         country = countrycode(country, "country.name", "country.name"),
         year = as.numeric(year), 
         civil_war = 1) |> 
  arrange(country, year) |> 
  group_by(country) |> 
  mutate(civil_war_prev_yr = if_else(lag(civil_war) == 1, 1, 0),
         # Account for start of dataset
         civil_war_prev_yr = if_else(year != 1946, 
                                     replace_na(civil_war_prev_yr, 0), 
                                     civil_war_prev_yr)) |> 
  select(-civil_war)|> 
  ungroup() |> 
  right_join(scope, by = c("year", "country")) |> 
  mutate(civil_war_prev_yr = replace_na(civil_war_prev_yr, 0))

duplicates <- civil_war_prev_yr |> 
  group_by(country, year) |> 
  summarise(n = n()) |> 
  filter(n > 1)

stopifnot(nrow(duplicates) == 0)

# Freedom House -----------------------------------------------------------

fh <- import(here::here("data-raw", "democracy_fh.csv")) |>  
  transmute(country = countrycode(fh_country, 
                                  "country.name", 
                                  "country.name",
                                  custom_match = c("Micronesia" = "Micronesia (Federated States of)")),
            year,
            fh_political_rights = pr, 
            fh_civil_liberties = cl, 
            fh_status = na_if(status, "")) |> 
  right_join(scope)

skimr::skim(fh)

duplicates <- fh |> 
  group_by(country, year) |> 
  summarise(n = n()) |> 
  filter(n > 1)

stopifnot(nrow(duplicates) == 0)

export(fh, here::here("data", "democracy_fh.csv"))

# Polity ------------------------------------------------------------------

polity <- import(here::here("data-raw", "democracy_polity.csv")) |> 
  transmute(country = countrycode(polity_annual_country, 
                               "country.name", 
                               "country.name"),
            year,
            across(democ:polity, ~ replace(.x, .x < -10, NA))) |> 
  distinct(country, 
           year, 
           polity_democracy = democ, 
           polity_autocracy = autoc, 
           polity_total = polity) |> 
  filter(country != "Sudan" | (country == "Sudan" & year == 2011 & polity_democracy == 1)) |> 
  right_join(scope, by = c("country", "year"))

skimr::skim(polity)

duplicates <- polity |> 
  group_by(country, year) |> 
  summarise(n = n()) |> 
  filter(n > 1)

stopifnot(nrow(duplicates) == 0)

export(polity, here::here("data", "democracy_polity.csv"))

# Elections
elections <- import(here::here("data-raw", "elections.csv")) |> 
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
                                                "Wales" = "United Kingdom")),
         year = lubridate::year(date), 
         type = str_to_lower(type),
         event = 1) |> 
  group_by(country, year, type) |> 
  summarise(event = sum(event)) |>  
  ungroup() |> 
  pivot_wider(names_from = type, values_from = event) |> 
  right_join(scope, by = c("country", "year")) |>  
  mutate(across(election:referendum, ~replace_na(.x, 0)))

duplicates <- elections |> 
  group_by(country, year) |> 
  summarise(n = n()) |> 
  filter(n > 1)

stopifnot(nrow(duplicates) == 0)

export(elections, here::here("data", "elections.csv"))

# Compile country-level data ----------------------------------------------

df <- scope |> 
  left_join(civil_war, by = c("country", "year")) |> 
  left_join(civil_war_onset, by = c("country", "year")) |>  
  left_join(civil_war_prev_yr, by = c("country", "year")) |>  
  left_join(milex, by = c("country", "year")) |>  
  left_join(population, by = c("country", "year")) |>  
  left_join(gdp, by = c("country", "year")) |>  
  left_join(uds, by = c("country", "year")) |>  
  left_join(checks, by = c("country", "year")) |>  
  left_join(fh, by = c("country", "year")) |> 
  left_join(polity, by = c("country", "year")) |>  
  left_join(elections, by = c("country", "year"))

export(df, here::here("data", "00_country_level.csv"))
