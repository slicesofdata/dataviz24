################################################################################
# Author:
#
# Description:
# read the main waves data, cleans up some key variables, and arranges them,
# prepares for all other tasks
# creates champ_all_waves_initial_clean.Rds
#
#
################################################################################
################################################################################
suppressMessages(library(dplyr))
R.utils::sourceDirectory(here::here("r", "functions"))
#get_current_file()

# read the original all waves file
CHAMP <- readRDS(here::here("data", "raw", "champ_all_waves.Rds"))
#CHAMP <- readRDS(here::here("data", "raw", "champ_all_waves_practice_subset.Rds"))

# remember you will later adjust this to the full data file rather that n = 3

#CHAMP |>
#  mutate(new = gsub(" ", "", ID_Subject)) |>
#  filter(new %in% all_ids[1:20]) |>
#  select(!new) |>
#  saveRDS(here::here("data", "champ_all_waves_practice_subset.Rds"))

#CHAMP |>
#  mutate(new = gsub(" ", "", ID_Subject)) |>
#  filter(new %in% all_ids[1:20]) |>
#  select(!new) |>
#  readr::write_csv(here::here("data", "champ_all_waves_practice_subset.csv"))

CHAMP <-
  CHAMP |>
  # rename the variable to be lower
  rename_with(.fn = tolower, .cols = everything()) |>
  # remove the spaces from id_subject and id_school
  mutate(id_subject = gsub(" ", "", id_subject)) |>

  mutate(id_school = gsub(" ", "", id_school)) |>
  # relocate to first col
  relocate(id_subject, id_school, id_wave, .before = 1) |>
  #group_by(id_subject) |>
  mutate(id = paste0("P", dense_rank(id_subject))) |> ungroup() |>
  relocate(id, .after = id_subject)

# consider/address other issues that you might desire for cleaning, or
# just address later in subsets

#unique(CHAMP$id_school)

# then filter for sample
CHAMP |>
  #filter(id_school %in% unique(id_school)[1:20])
  saveRDS(here::here("data", "champ_all_waves_initial_clean.Rds"))

message("Saved: champ_all_waves_initial_clean.Rds")
# how many variables
#CHAMP |>  names() |>  length()

#CHAMP |> dim()

#view_html(CHAMP[1:5,])

# After meeting with your liaison, review any tasks and scales that will
# require subsetting, and later pivoting from wide to long, and potential cleaning
# Make a list of the surveys/measures/tasks for delegating and working on later
