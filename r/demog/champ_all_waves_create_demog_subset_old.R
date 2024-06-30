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


################################################################################
# subset the variables
names(CHAMP)
CHAMP$biq5

CHAMP |>
  select(c("id_subject",  "id_wave", starts_with("blq1"))) |>
  group_by(id_subject, id_wave) |>
  recode_wide_options_to_single_var(base_varname = "blq1option",
                                    to = "blq1"
  )


CHAMP |>
  select(c("id_subject", "id_wave", starts_with("acqsnack"))) |>
  group_by(id_subject, id_wave) |>
  recode_wide_options_to_single_var(base_varname = "acqsnack_1_acq",
                                    to = "x9"
  )







recode_wide_options_to_single_var
demog_vars <- c("id_subject", "id_school", "id_wave", "w1age",)
CHAMP <-
  CHAMP |>
  select("id_subject", "id_school", "id_wave", contains("symm_"))

################################################################################
# write out data?
saveRDS(CHAMP, here::here("data", "gng", "sspan_subset_wide.Rds"))


################################################################################

