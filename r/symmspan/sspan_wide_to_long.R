################################################################################
# Author:
#
################################################################################
# Description:
# converts wide to long

################################################################################
# load libraries and functions
library(dplyr)
R.utils::sourceDirectory(here::here("r", "functions"))

################################################################################
# read the raw data
# subset the data, this performs the previous steps (reads raw, subsets to wide,
# writes sspan_long.Rds)
source(here::here("r", "symmspan", "champ_all_waves_create_sspan_subset.R"))

################################################################################
# read wide data
WIDE <- readRDS(here::here("data", "symmspan", "sspan_subset_wide.Rds"))

################################################################################
# convert to long
LONG <-
  WIDE |>
  tidyr::pivot_longer(cols = starts_with("symm_"),
                      names_to = c(".value", "trial"),
                      names_pattern = "([A-Za-z]+)(\\d+)"
  )

# LONG |> view_html()

################################################################################
# write long data
saveRDS(LONG, here::here("data", "symmspan", "sspan_long.Rds"))

message("Saved: sspan_long.Rds")
