################################################################################
# Go/No-Go Task
################################################################################

# Inhibitory control was measured by a Go/No-Go task in which an empty
# rectangle was filled with the color green (Go) or the color blue (No-Go).
# Participants were instructed to press a response key as rapidly as
# possible when presented with Go signals and to withhold a response
# when presented with No-Go signals.

# The orientation of the rectangle (i.e. horizontal or vertical) preceding
# the target color stimulus signaled the probability that a go or no-go
# target will be displayed. Cues presented horizontally preceded the go
# target on 70% of the trials and preceded the no-go target on 30% of
# the trials. Cues presented vertically preceded the no-go target on 70%
# of the trials and preceded the go target on 30% of the trials.
# Half the cues were vertical rectangles and half the cues were
# horizontal rectangles. The task consisted of 160 trials presented in
# four blocks. Individual trials with mean reaction times greater
# than 1000ms or less than 100ms were excluded from the statistical
# analysis (Fillmore et al., 2006; 2009).

# Previous research has demonstrated that this level of cue validity
# produces prepotent responding (Fillmore, Rush, & Hays, 2006;
# Fillmore, Ostling, Martin, & Kelly, 2009; Finn, Justus, Mazas, & Steinmetz,
# 1999; Mostofsky & Simmonds, 2008; Simmonds, Pekar, & Mostofsky, 2008).
# Failing to withhold a response to a No-Go stimulus is designated as a
# false alarm and serves as a measure of response inhibition failure
# indicative of impulsive behavior (Fillmore et al., 2006; 2009;
# Weafer, Fillmore, & Milich, 2009).

# For predictive models, three parcel score indicators were used
# for the latent variables for the generic-cued false alarm.
# Individual trials were randomly assigned to one of the three parcels,
# and then standardized scores were computed within each parcel.
# Parceling creates an accurate representation of the data if the
# measure is uni-dimensional in theory and measurement
# (for a detailed discussion of the advantages of parceling,
# please see Little, Cunningham, Shahar, & Widaman, 2002).
#
################################################################################
library(tidyverse)

# read the raw data
source(here::here("r", "champ_all_waves_initial_clean.R"))
CHAMP <- readRDS(here::here("data", "raw", "champ_all_waves_practice_subset.Rds"))
CHAMP %>%
#  select(order(names(.))) |>
  select(contains("w2")) |>
  names()
CHAMP$w1aa

# subset the data
source(here::here("r", "gng", "champ_all_waves_create_gng_subset.R"))

#source(here::here("data", "champs_read.R"))

# Load data
#CHAMP <- readRDS(here::here("data", "raw", "champ_all_waves_n3.Rds"))

#
# table(CHAMP$ntarget, CHAMP$correct)



########################################################################################################
# GNG
########################################################################################################
# read data
DAT <- readRDS(here::here("data", "gng", "gng_long.Rds"))

table(DAT$ntarget, DAT$ptarget)

DAT |>
  select(ntarget, ptarget) |>
  table()


rownames_to_var <- function(data,
                            var
                            ) {
  tibble::rownames_to_column(.data = {{data}},
                             var = {{var}})
}

rownames_to_var()

rownames_to_var(data = DAT, var = "soa")


DAT |>
  filter(id_subject == "FAQAAW") |>
  select(soa, ptarget) |>
  table() |>
  as.data.frame.matrix() |>
  tibble::rownames_to_column(var = "soa") |>
  #mutate(all = 1) |>
  #group_by(all) |>
  mutate(v_pre_go   = sum(VGO.bmp)/sum(VGO.bmp, HGO.bmp),
         h_pre_nogo = sum(HNOGO.bmp)/sum(HNOGO.bmp, VNOGO.bmp)
          ) |>
  group_by(soa) |>
  mutate(v_pre_go_soa   = sum(VGO.bmp)/sum(VGO.bmp, HGO.bmp),
         h_pre_nogo_soa = sum(HNOGO.bmp)/sum(HNOGO.bmp, VNOGO.bmp)
  ) |>
  ungroup()

  mutate(H_go_to_nogo = HGO.bmp/HNOGO.bmp,
         V_go_to_nogo = VNOGO.bmp/VGO.bmp
         ) |>
  mutate(ratio = ratio_of_integers(HGO.bmp, HNOGO.bmp)) |>
  ungroup()


DAT |> view_html()

ratio_of_integers <- function(x, y) {
  gcd = gmp::gcd(gmp::as.bigz(x), gmp::as.bigz(y))
  return(paste((c(x, y) / gcd)[1], ":", (c(x, y) / gcd)[2], sep = ""))
}

ratio_of_integers(1, 10)

########################################################################################################
# Create trialtype variable
# or change go and no go  and

#Investigate the ntarget and ptarget variables. You can use `table() for cross tabulation of the two variable
# to understand how those variables related. To do so, select these two variables from your data frame and pipe
# them to table(). The contingency table will display counts at each combination of events.

# Your variable creation can be based on either variable, so there is no one way to accomplish the goal.




# create mutate(cnt = 1)

# Create trialtype

# Create orientation

# unique() get_url_for_module(file_name = "vectors_and_data_frame_basics")
# distinct() get_url_for_module(file_name = "manipulating_data_selecting_filtering_mutating")
# ifelse() or case_when() at get_url_for_module(file_name = "manipulating_data_selecting_filtering_mutating")

  DAT |>
  # then pipe to:
  # create a variable that assign
  mutate(cnt = 1) |>

  # then pipe to:
  # either (a) rename id_wave into wave or
  rename(wave = id_wave) |>
  # (b) mutate wave based on id_wave, relocate it to its original location and, select out id_wave
  # mutate(wave = id_wave) |>

  # create a new variable named orientation as either vertical or horizontal
  # the ptarget variable will provide the information needed for creating the variable
  # because the first letter will indicate whether the orientation is either v = vertical
  # or h = horizontal
  # in the new variable, trials that are vertical should be named "Vertical" and the
  # horizontal trials "Horizontal", specifically with the upper case first character because
  # that naming convention may appear better on plots or outputs that might require them.
  #

  # remember also that unique() will provide you with the unique instances of a vector

  # there are several ways that you can accomplish this goal using different functions presented
  # in readings thus far; so there are is no single correct approach

  # using ifelse()
  mutate(orientation_ie = ifelse(ptarget %in% c("VGO.bmp", "VNOGO.bmp"), "Vertical", "Horizontal")) |>

  # you can use startsWith() from base R

  mutate(orientation_iesw = ifelse(startsWith(DAT$ptarget, "V"), "Vertical", "Horizontal")) |>
  # you can use str_detect from {stringr}, with was used in the context of filter() in manipulating_data_selecting_filtering_mutating
  mutate(orientation_cw = case_when(
    stringr::str_detect(ptarget, "^V") ~ "Vertical", # starts ^
    stringr::str_detect(ptarget, "^H") ~ "Horizontal"
    )) |>


  # then pipe to:
  # create a new variable named trialtype to represent trials as either go or no-go
  # the ptarget variable will provide the information needed for creating the variable
  # there are several ways that you can accomplish this goal, so there are is no single
  # correct approach

  # Because there are only two conditions/levels of this variable
  mutate(trialtype = case_when(
    stringr::str_detect(ptarget, "NOGO") ~ "No-Go",
    !stringr::str_detect(ptarget, "NOGO") ~ "Go"
  ))

  #


  # then pipe to:

DAT |> view_html()

DAT |>
  filter(stringr::str_detect(ptarget, "NOGO")) |>
  filter(stringr::str_detect(ptarget, "NOGO"))

########################################################################################################





# verify that :
#
# 1. The task consisted of 160 trials presented in
# four blocks.
# 2. Half the cues were vertical rectangles and half the cues were horizontal rectangles.

# 3. Cues presented horizontally preceded the go
# target on 70% of the trials and preceded the no-go target on 30% of
# the trials.
# 4. Cues presented vertically preceded the no-go target on 70%
# of the trials and preceded the go target on 30% of the trials.
# 5. Examine the number of trials for each soa

# 1: 160 trials?trials
task_DAT_long |>
  group_by(id_subject) |>
  count()

# 2. Half the cues were vertical rectangles and half the cues were horizontal rectangles.
DAT |>
  select(id_subject, id_wave, trialtype, orientation, cnt) |>
  group_by(id_subject, orientation) |>
  summarize(trials = n()) |>
  ungroup()

# or
task_DAT_long |>
  select(subid, trialtype, orientation, cnt) |>
  group_by(subid, orientation) |>
  summarize(trials = n()) |>
  group_by(subid) |>
  mutate(trialtype_cnt = sum(trials)) |>
  mutate(ratio = trials/trialtype_cnt) |>
  ungroup()

# 3 and 4: Cues presented horizontally preceded the go target on 70% of the trials and preceded the no-go target on 30% of
task_DAT_long |>
  select(subid, trialtype, orientation, cnt) |>
  group_by(subid, trialtype, orientation) |>
  summarize(trials = n()) |>
  group_by(subid, trialtype) |>
  mutate(trialtype_cnt = sum(trials)) |>
  mutate(ratio = trials/trialtype_cnt) |>
  ungroup()

# Accuracy
# new var
task_DAT_long <- task_DAT_long |>
  select(subid, w1age, trialtype, correct, rt, soa) |>
  mutate(accuracy = ifelse(stringr::str_detect(correct , "^Corr"), 1, 0))

# or overwrite correct
#task_DAT_long <- task_DAT_long |>
#  mutate(correct = ifelse(stringr::str_detect(correct , "^Corr"), 1, 0))

view_html(task_DAT_long)

library(magrittr)

task_DAT_long |>
  group_by(subid, trialtype, soa) |>
  summarize(corr = mean(accuracy, na.rm = T),
            rt = mean(rt, na.rm = T),
            .groups = "drop") %>%
  saveRDS(.,
          file = here::here("data/Rds/tasks/", paste0("CHAMPS_",gsub("_", "", toupper(task_prefix)), "_agg.Rds"))
  )


# then save
#saveRDS(task_DAT,
#        file = here::here("data", paste0("CHAMPS_",gsub("_", "", toupper(task_prefix)), ".Rds")))

###############################################################################
###############################################################################
###############################################################################







# Get the relevant variable names
task_prefix <- "gng_"

task_vars <-
  CHAMP |>
  select(matches(paste0("^", task_prefix))) |>
  names() |>
  tolower()

task_vars

CHAMP |>
  select(c("ID_Subject", "ID_Wave", matches(paste0("^", task_prefix))))

rename_with(toupper) |>
  select(all_of(c(task_vars)))


# get the unique task vars
task_vars_for_long <- unique(gsub("[^A-Za-z]", "", gsub(task_prefix, "", unique(task_vars)))) |>
  tolower()

# get key vars to keep
key_vars <- c("subid", "w1age")

key_vars <- c("id_subject", "id_wave", "w1age")

# Create the data set
task_DAT <-
  CHAMP |>
  # rename all vars to lower
  rename_with(tolower, everything()) |>
  # select
  mutate(id_subject = gsub(" ", "", id_subject)) |>
  # select the key variables, including the cognitive task
  select(all_of(c(key_vars, task_vars))) |>
  # remove any missing data
  filter(!is.na(gng_ntarget1)) |>
  # then remove the prefix of names
  rename_with(~gsub(task_prefix, "", .x, fixed = TRUE)) |>
  # then filter rows with missing data across all task trials
  filter(if_all(starts_with(task_prefix), ~ !is.na(.)) )

######################################################################
######################################################################

# write the wide
task_DAT |>
  filter(id_subject == "FAQAAW") |>
  saveRDS(here::here("data", "gng", "gng_wide.Rds"))



######################################################################
# pivot to long
task_DAT_long <-
  task_DAT |>
  tidyr::pivot_longer(cols = starts_with(c(task_vars_for_long)),
                      names_to = c(".value", "trial"),
                      names_pattern = "([A-Za-z]+)(\\d+)"
  )

task_DAT_long |> filter(id_subject == "FAQAAW") |>
  saveRDS(here::here("data", "gng", "gng_long.Rds"))
task_DAT_long |> filter(id_subject == "FAQAAW") |>
  readr::write_csv(here::here("data", "gng", "gng_long.csv"))

#task_DAT_long |> filter(subid == 13) |> view_html()

#view_html(task_DAT_long)


# Then Cleqn

task_DAT_long |> select(ntarget) |> distinct()
task_DAT_long |> select(ptarget) |> distinct()
task_DAT_long |> select(soa) |> distinct()
task_DAT_long |> select(correct) |> distinct()

gng_table <- table(task_DAT_long$ntarget, task_DAT_long$correct)
row.names(gng_table) <- task_DAT_long |> select(ptarget) |> distinct() |> pull()


#task_DAT_long <- ''
task_DAT_long |>
  mutate(trialtype = case_when(
    stringr::str_detect(ptarget, "NOGO") ~ "No-Go",
    !stringr::str_detect(ptarget, "NOGO") ~ "Go"
  ))
  mutate(orientation = case_when(
    stringr::str_detect(ptarget, "^V") ~ "Vertical", # starts ^
    stringr::str_detect(ptarget, "^H") ~ "Horizontal"
  )) |>
  mutate(cnt = 1)

# then save
saveRDS(task_DAT_long,
        file = here::here("data/Rds/tasks",
                          paste0("CHAMPS_", gsub("_", "", toupper(task_prefix)), "_long.Rds")))

writexl::write_xlsx(x = task_DAT_long,
                    path = here::here("data/Rds/tasks", paste0("CHAMPS_",gsub("_", "", toupper(task_prefix)), "_long.xlsx"))
)

#############################################################################
#############################################################################
# read back in
#task_DAT_long <- readRDS(here::here("data/Rds/tasks", paste0("CHAMPS_", gsub("_", "", toupper(task_prefix)), "_long.Rds")))




# The orientation of the rectangle (i.e. horizontal or vertical) preceding
# the target color stimulus signaled the probability that a go or no-go
# target will be displayed. Cues presented horizontally preceded the go
# target on 70% of the trials and preceded the no-go target on 30% of
# the trials. Cues presented vertically preceded the no-go target on 70%
# of the trials and preceded the go target on 30% of the trials.
# Half the cues were vertical rectangles and half the cues were
# horizontal rectangles. The task consisted of 160 trials presented in
# four blocks. Individual trials with mean reaction times greater
# than 1000ms or less than 100ms were excluded from the statistical
# analysis (Fillmore et al., 2006; 2009).
#
#

