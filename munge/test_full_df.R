# This script makes sure that the data are behaving as expected.

# Read in full dataset ----------------------------------------------------

full_df <- rio::import("full_dataset.csv")

# Check scope -------------------------------------------------------------

# No duplicate group years
full_df |> 
  group_by(kgcid, group, year) |> 
  summarise(n = n()) |> 
  filter(n > 1)

# Check group-level data --------------------------------------------------

# Expect relative size to be between 0 and 1
full_df |> 
  summarise(min = min(relative_size, na.rm = T),
            max = max(relative_size, na.rm = T))

