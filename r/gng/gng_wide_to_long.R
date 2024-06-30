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
get_current_file()

################################################################################
# read the raw data
#source(here::here("r", "champ_all_waves_initial_clean.R"))
# subset the data, this performs the previous steps (reads raw, subsets to wide,
# writes gng_subset_wide.Rds)
source(here::here("r", "gng", "champ_all_waves_create_gng_subset.R"))

################################################################################
# read wide data
WIDE <- readRDS(here::here("data", "gng", "gng_subset_wide.Rds"))

#view_html(WIDE)

#WIDE |>
#  mutate(task = case_when(
#    is.na(gng_ntarget1) ~ "Symmetry Span",
#    !is.na(gng_ntarget1) ~ "Go/No-Go",
#  )) |> view_html()

################################################################################
# convert to long
LONG <-
  WIDE |>
  # tidyr::pivot_longer(cols = starts_with(c(task_vars_for_long)),
  tidyr::pivot_longer(cols = starts_with("gng_"),
                      names_to = c(".value", "trial"),
                      names_pattern = "([A-Za-z]+)(\\d+)"
  )


#LONG <-
#  LONG |>
#  mutate(task = case_when(
#    is.na(ntarget) ~ "Symmetry Span",
#    !is.na(ntarget) ~ "Go/No-Go",
#  )) |>
#  relocate(task, .after = id_wave)

#view_html(LONG)

################################################################################
# write long data
saveRDS(LONG, here::here("data", "gng", "gng_long.Rds"))

message("Saved: gng_long.Rds")
