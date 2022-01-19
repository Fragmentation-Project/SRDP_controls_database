# This script sources group-level control data.

library(tidyverse)
library(rio)

# Relative size
relative_size_raw <- import("https://icr.ethz.ch/data/epr/core/EPR-2021.csv")
