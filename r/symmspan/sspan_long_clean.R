################################################################################
# Author:
#
################################################################################
# Description:
# reads and cleans sspan_long.Rds
# filters rows
# saves sspan_long_clean.Rds for data aggregation/summary


################################################################################
# load libraries and functions
library(dplyr)
R.utils::sourceDirectory(here::here("r", "functions"))

################################################################################
# source the previous steps (read raw, subsets to wide, converts to long, then
# writes sspan_long.Rds)
source(here::here("r", "symmspan", "sspan_wide_to_long.R"))

################################################################################
# read long data
SSPAN <- readRDS(here::here("data", "symmspan", "sspan_long.Rds"))

################################################################################
# clean variables and rows
CLEANED <-
  SSPAN |>
  # create wave from id_wave and move
  mutate(wave = id_wave) |>
  relocate(wave, .after = id_wave) |>

  # Filter the data so that you only have rows on trial 12
  filter(trial == "12") |>

  # remove missing
  filter(!is.na(correct)) |>

  select(-c(id_wave, trial)) |>

  # remove duplicate rows, if there are any
  distinct()


CLEANED |> view_html()
################################################################################
# trim data maybe

################################################################################
# write

saveRDS(CLEANED, here::here("data", "symmspan", "sspan_long_clean.Rds"))

message("Saved: sspan_long_clean.Rds")
