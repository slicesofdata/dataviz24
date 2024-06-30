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
get_current_file()

################################################################################
# source relevant script
source(here::here("r", "champ_all_waves_initial_clean.R"))


################################################################################
# read data
CHAMP <- readRDS(here::here("data", "champ_all_waves_initial_clean.Rds"))

################################################################################
# subset the variables
CHAMP <-
  CHAMP |>
  select("id_subject", "id_school", "id_wave", contains("gng_"))

################################################################################
# write out data?
saveRDS(CHAMP, here::here("data", "gng", "gng_subset_wide.Rds"))

message("Saved: gng_subset_wide.Rds")
################################################################################

