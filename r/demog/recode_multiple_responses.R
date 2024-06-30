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

# read the subset file
source(here::here("r", "demog", "champ_all_waves_demog_subset.R"))


################################################################################
# read data
# needs to be created
#
DEMOG <- readRDS(here::here("data", "champ_all_waves_initial_clean.Rds"))
# DEMOG <- readRDS(here::here("data", "demog", "demog_subset_wide.Rds"))


################################################################################
# subset the variables
names(DEMOG)
DEMOG$biq5

# This works to reduce response
DEMOG |>
  #select(c("id_subject",  "id_wave", starts_with("blq1"))) |>
  group_by(id_subject, id_wave) |>
  # the blq1option responses
  recode_wide_options_to_single_var(base_varname = "blq1option", to = "blq1") |>
  # the asq1option responses
  recode_wide_options_to_single_var(base_varname = "asq1option", to = "asq1") |>
  #select(contains("blq1option|asq1option"))
  select(any_of(c("blq1", "asq1")))




# but the fails (is it a multiple checkmark?)
#DEMOG <- ''
DEMOG |>
  select(c("id_subject", "id_wave", starts_with("acqsnack"))) |>
  group_by(id_subject, id_wave) |>
  recode_wide_options_to_single_var(base_varname = "acqsnack_1_acq",
                                    to = "acqsnack_1_acq"
  )

################################################################################
# write out data?
#saveRDS(DEMOG, here::here("data", "gng", "demog_subset_wide_cleaned.Rds"))


################################################################################

