################################################################################
# Author: Gabriel Cook
#
# Description:
# - Reading data
# - Subsetting data
# - Writing out the subset
################################################################################

################################################################################
# Load libraries and functions
library(dplyr)
R.utils::sourceDirectory(here::here("r", "functions"))

################################################################################
# Source the champ all waves script and read the RDS file
source(here::here("r", "champ_all_waves_initial_clean.R"))
CHAMP <- readRDS(file = here::here("data", "champ_all_waves_initial_clean.Rds"))

################################################################################
# Subsetting the Data Frame

useful <- c("id_subject", "id", "id_school", "id_wave", "dob", "age")
CHAMP <- select(CHAMP,  any_of(useful) | contains("aaq") | contains("abq") | contains("bmq") | contains("bnq"))

################################################################################
# Writing out the subset

saveRDS(object = CHAMP,
        file = here::here("data", "demog", "demog_subset_wide.rds")
)
message("Saved: demog_subset_wide.rds")
