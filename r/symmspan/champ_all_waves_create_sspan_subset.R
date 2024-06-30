################################################################################
# Author:
#
# Description:
#
#
################################################################################
# load libraries and functions
library(tidyverse)
R.utils::sourceDirectory(here::here("r", "functions"))

################################################################################
# source relevant script
source(here::here("r", "champ_all_waves_initial_clean.R"))


################################################################################
# read data
CHAMP <- readRDS(here::here("data", "champ_all_waves_initial_clean.Rds"))

#CHAMP  |> view_html()
################################################################################
# subset the variables
CHAMP <-
  CHAMP |>
  select("id_subject", "id_school", "id_wave", contains("symm_"))

################################################################################
# write out data?
saveRDS(CHAMP, here::here("data", "symmspan", "sspan_subset_wide.Rds"))

message("Saved: sspan_subset_wide.Rds")
################################################################################

