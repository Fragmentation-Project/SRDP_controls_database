# This script sources group-level control data.

library(tidyverse)
library(rio)

# Group scope
groups_raw <- sRdpPrivateData::groups

# Size and relative size
relative_size_raw <- import("https://icr.ethz.ch/data/epr/core/EPR-2021.csv")

# Religion
religion_raw <- googlesheets4::range_read("1Q6jeejbdmoNmnQFgma_BgH8BtEBdkFHzSsNBdLPomWY") %>% 
  janitor::clean_names()
