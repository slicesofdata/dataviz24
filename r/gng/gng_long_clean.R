################################################################################
# Author:
#
################################################################################
# Description:
# reads and cleans gng_long.Rds
# create necessary variables
# modifies RT
# saves gng_long_clean.Rds for data aggregation/summary

################################################################################
# load libraries and functions
library(dplyr)
R.utils::sourceDirectory(here::here("r", "functions"))
get_current_file()

################################################################################
# source the previous steps (read raw, subsets to wide, converts to long, then
# writes gng_long.Rds)
source(here::here("r", "gng", "gng_wide_to_long.R"))

################################################################################
# read long data
LONG <- readRDS(here::here("data", "gng", "gng_long.Rds"))

################################################################################
#LONG |> view_html()

#LONG |> select(correct, ptarget) |> table()

#LONG |>  select(correct, ptarget) |> table()

# Clean
GNG_CLEAN <-
  LONG |>
    #filter(id_subject == "FAQAAW") |>
  # create and relocate wave
  mutate(wave = id_wave) |>
  relocate(wave, .after = id_wave) |>

  # then pipe to:
  mutate(count = 1) |>

  # then pipe to:
  mutate(cue = case_when(
    ptarget %in% c("VGO.bmp", "VNOGO.bmp") ~ "Vertical",
    ptarget %in% c("HGO.bmp", "HNOGO.bmp") ~ "Horizontal",
    is.na(ptarget) ~ NA
  )) |>
  # ifelse(ptarget %in% c("VGO.bmp", "VNOGO.bmp"), "Vertical", "Horizontal")) |>
  # could also use gsub to clean up.

  # then pipe to:
  mutate(target = case_when(
    stringr::str_detect(ptarget, "NOGO") ~ "No-Go",
    !stringr::str_detect(ptarget, "NOGO") ~ "Go",
    is.na(ptarget) ~ NA
  )) |>
  # accuracy
  mutate(accuracy = case_when(
    correct %in% c("Correct1", "Correct2") ~ 1,
    correct == "Incorrect" ~ 0,
    is.na(ptarget) ~ NA
  )) |>
  #
  # make rt missing
  #  mutate(rt = ifelse(rt == 0, NA, rt)) |>

  # get the RTs for go and no-go
  mutate(rt = case_when(
    target == "Go" & accuracy == 1 ~ rt,
    target == "No-Go" & accuracy == 0 ~ rt
  )) |>
  mutate(commission_rt = case_when(
    target == "No-Go" & accuracy == 0 ~ rt
  ))|>
  group_by(id_subject, wave, target) |>
  mutate(total_trials = sum(count)) |>
  ungroup() |>
  mutate(missing_trials = ifelse(total_trials < 80, "yes", "no"))


GNG_CLEAN |> head() |> view_html()

################################################################################
# trim data maybe

################################################################################
# write
saveRDS(GNG_CLEAN, here::here("data", "gng", "gng_long_clean.Rds"))

message("Saved: gng_long_clean.Rds")

#GNG_CLEAN <-
#    LONG |>
#    # get only the real rows
#    filter(ntarget %in% c(1, 2, 3, 4)) |>
#    # compute counter for trial count (use count() option too)
#    mutate(count = 1) |>
#    # better var name
#    mutate(wave = id_wave) |>
#    relocate(wave, .after = id_wave) |>
#    # create the target type
#    mutate(target = case_when(
#      ptarget %in% c("VGO.bmp", "HGO.bmp") ~ "Go",
#      ptarget %in% c("VNOGO.bmp", "HNOGO.bmp") ~ "No-Go",
#      TRUE ~ NA
#    )) |>
#    # compute accuracy
#    mutate(accuracy = case_when(
#      target == "Go" & correct == "Correct1" ~ 1,
#      target == "No-Go" & correct == "Correct2" ~ 1,
#      correct == "Incorrect" ~ 0,
#      TRUE ~ NA
#    )) |>
#    # remove effort
#    mutate(rt = ifelse(rt == 0, NA_real_, rt)) |>
#    #mutate(rt = ifelse(accuracy == 0, NA, rt)) |>




