# This script sources group-level control data.

library(tidyverse)
library(rio)

# Group scope
groups_raw <- sRdpPrivateData::groups

rio::export(groups_raw, here::here("data-raw", "groups_raw.csv"))

# Size and relative size
relative_size_raw <- rio::import("https://icr.ethz.ch/data/epr/core/EPR-2021.csv")

rio::export(relative_size_raw, here::here("data-raw", "relative_size_raw.csv"))

# Religion
religion_raw <- googlesheets4::range_read("1Q6jeejbdmoNmnQFgma_BgH8BtEBdkFHzSsNBdLPomWY")

rio::export(religion_raw, here::here("data-raw", "religion_raw.csv"))
