################################################################################
# Author: Andrei Sarapov
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
source(here::here("r", "demog", "champ_all_waves_create_demog_subset.R"))

CHAMP <- readRDS(file = here::here("data", "demog", "demog_subset_wide.Rds"))

###############################################################################
# cleaning the "aaq" "abq"
################################################################################
CHAMP <-
  CHAMP |>
  mutate(eth = w1aaq6,
         sex = w1aaq2,
         his_or_lat = w1aaq3,
         his_lat_origin = w1aaq4,
         white = w1aaq5option1,
         black = w1aaq5option2,
         asian = w1aaq5option3,
         pacific = w1aaq5option4,
         native = w1aaq5option5,
         other_eth = w1aaq5option6,
         pob = w1aaq7,
         years_in_us = w1aaq8,
         pob_father = w1aaq9,
         pob_mother = w1aaq10,
         edu_father = w1aaq11,
         edu_mother = w1aaq12,
         emp_father = w1aaq13,
         emp_mother = w1aaq14,
         emp_type_father = aaq15,
         emp_type_mother = aaq16,
         welfare = aaq17,
         food_stamps = aaq18,
         free_lunches = aaq19,
         emp_type = aaq20,
         por = abq1,
         living_arr = abq2,
         father = abq3option1,
         mother = abq3option2,
         stepfather = abq3option3,
         stepmother = abq3option4,
         foster = abq3option5,
         brother = abq3option6,
         sister = abq3option7,
         grandfather = abq3option8,
         grandmother = abq3option9,
         relative = abq3option10,
         spouse = abq3option11,
         partner = abq3option12,
         son = abq3option14,
         daughter = abq3option13,
         roommate = abq3option15,
         other_cohab = abq3option16,
         res_under_18 = abq4,
         res_over_18 = abq5) |>
  select(-contains("aaq") & -contains("abq"))



################################################################################
# Clean the demographic variables
################################################################################
CHAMP <-
  CHAMP |>
  mutate(white_yn = white,
         black_yn = black,
         asian_yn = asian,
         pacific_yn = pacific,
         native_yn = native,
         other_eth_yn = other_eth,
         sex = case_when(
           sex == 1 ~ "Male",
           sex == 2 ~ "Female"
         ),
         his_or_lat = case_when(
           his_or_lat == 0 ~ "No",
           his_or_lat == 1 ~ "Yes"
         ),
         his_lat_origin = case_when(
           his_lat_origin == 1 ~ "Mexican American",
           his_lat_origin == 2 ~ "Central American",
           his_lat_origin == 3 ~ "South American",
           his_lat_origin == 4 ~ "Cuban",
           his_lat_origin == 5 ~ "Puerto Rican",
           his_lat_origin == 6 ~ "Mixed",
           his_lat_origin == 7 ~ "Other"
         ),
         eth = case_when(
           eth == 1 ~ "White",
           eth == 2 ~ "Black",
           eth == 3 ~ "Asian",
           eth == 4 ~ "Pacific Islander",
           eth == 5 ~ "Native",
           eth == 6 ~ "Other"
         ),
         pob = case_when(
           pob == 1 ~ "US",
           pob == 2 ~ "Mexico",
           pob == 3 ~ "Central America",
           pob == 4 ~ "Latin America",
           pob == 5 ~ "Middle East",
           pob == 6 ~ "Africa",
           pob == 7 ~ "Asia",
           pob == 8 ~ "Pacific",
           pob == 9 ~ "Europe",
           pob == 10 ~ "Other"
         ),
         pob_father = case_when(
           pob_father == 1 ~ "US",
           pob_father == 2 ~ "Mexico",
           pob_father == 3 ~ "Central America",
           pob_father == 4 ~ "Latin America",
           pob_father == 5 ~ "Middle East",
           pob_father == 6 ~ "Africa",
           pob_father == 7 ~ "Asia",
           pob_father == 8 ~ "Pacific",
           pob_father == 9 ~ "Europe",
           pob_father == 10 ~ "Other"
         ),
         pob_mother = case_when(
           pob_mother == 1 ~ "US",
           pob_mother == 2 ~ "Mexico",
           pob_mother == 3 ~ "Central America",
           pob_mother == 4 ~ "Latin America",
           pob_mother == 5 ~ "Middle East",
           pob_mother == 6 ~ "Africa",
           pob_mother == 7 ~ "Asia",
           pob_mother == 8 ~ "Pacific",
           pob_mother == 9 ~ "Europe",
           pob_mother == 10 ~ "Other"
         ),
         edu_father = case_when(
           edu_father == 1 ~ "High school",
           edu_father == 2 ~ "Finished high school",
           edu_father == 3 ~ "College",
           edu_father == 4 ~ "Finished high school",
           edu_father == 5 ~ "Finished graduate school",
           edu_father == -7 ~ "Unknown"
         ),
         edu_mother = case_when(
           edu_mother == 1 ~ "High school",
           edu_mother == 2 ~ "Finished high school",
           edu_mother == 3 ~ "College",
           edu_mother == 4 ~ "Finished high school",
           edu_mother == 5 ~ "Finished graduate school",
           edu_mother == -7 ~ "Unknown"
         ),
         emp_father = case_when(
           emp_father == 9 ~ "Major professional",
           emp_father == 8 ~ "Minor professional",
           emp_father == 7 ~ "Small business",
           emp_father == 6 ~ "Clerk",
           emp_father == 5 ~ "Skilled laborer",
           emp_father == 4 ~ "Semi-skilled laborer",
           emp_father == 3 ~ "Manual labor",
           emp_father == 2 ~ "Unemployed",
           emp_father == 1 ~ "Stay at home",
           emp_father == -7 ~ "Unknown"
         ),
         emp_mother = case_when(
           emp_mother == 9 ~ "Major professional",
           emp_mother == 8 ~ "Minor professional",
           emp_mother == 7 ~ "Small business",
           emp_mother == 6 ~ "Clerk",
           emp_mother == 5 ~ "Skilled laborer",
           emp_mother == 4 ~ "Semi-skilled laborer",
           emp_mother == 3 ~ "Manual labor",
           emp_mother == 2 ~ "Unemployed",
           emp_mother == 1 ~ "Stay at home",
           emp_mother == -7 ~ "Unknown"
         ),
         emp_type_father = case_when(
           emp_type_father == 2 ~ "Full-time",
           emp_type_father == 1 ~ "Part-time",
           emp_type_father == 0 ~ "Not working",
           emp_type_father == -7 ~ "Unknown"
         ),
         emp_type_mother = case_when(
           emp_type_mother == 2 ~ "Full-time",
           emp_type_mother == 1 ~ "Part-time",
           emp_type_mother == 0 ~ "Not working",
           emp_type_mother == -7 ~ "Unknown"
         ),
         welfare = case_when(
           welfare == 0 ~ "No",
           welfare == 1 ~ "Yes"
         ),
         food_stamps = case_when(
           food_stamps == 0 ~ "No",
           food_stamps == 1 ~ "Yes"
         ),
         free_lunches = case_when(
           free_lunches == 0 ~ "No",
           free_lunches == 1 ~ "Yes"
         ),
         emp_type = case_when(
           emp_type == 2 ~ "Full-time",
           emp_type == 1 ~ "Part-time",
           emp_type == 0 ~ "Not working",
           emp_type == -7 ~ "Unknown"
         ),
         white = case_when(
           white == 1 ~ "White"
         ),
         black = case_when(
           black == 1 ~ "Black"
         ),
         asian = case_when(
           asian == 1 ~ "Asian"
         ),
         pacific = case_when(
           pacific == 1 ~ "Pacific"
         ),
         native = case_when(
           native == 1 ~ "Native"
         ),
         other_eth = case_when(
           other_eth == 1 ~ "Other"
         ),
         por = case_when(
           por == 1 ~ "Parent's home",
           por == 2 ~ "Another's home",
           por == 3 ~ "Own home",
           por == 4 ~ "Group home"
         ),
         living_arr = case_when(
           living_arr == 1 ~ "Alone",
           living_arr == 2 ~ "With others"
         ),
         father = case_when(
           father == 1 ~ "Father"
         ),
         mother = case_when(
           mother == 1 ~ "Mother"
         ),
         stepfather = case_when(
           stepfather == 1 ~ "Stepfather"
         ),
         stepmother = case_when(
           stepmother == 1 ~ "Stepmother"
         ),
         foster = case_when(
           foster == 1 ~ "Foster"
         ),
         brother = case_when(
           brother == 1 ~ "Brother"
         ),
         sister = case_when(
           sister == 1 ~ "Sister"
         ),
         grandfather = case_when(
           grandfather == 1 ~ "Grandfather"
         ),
         grandmother = case_when(
           grandmother == 1 ~ "Grandmother"
         ),
         relative = case_when(
           relative == 1 ~ "Relative"
         ),
         spouse = case_when(
           spouse == 1 ~ "Spouse"
         ),
         partner = case_when(
           partner == 1 ~ "Partner"
         ),
         son = case_when(
           son == 1 ~ "Son"
         ),
         daughter = case_when(
           daughter == 1 ~ "Daughter"
         ),
         roommate = case_when(
           roommate == 1 ~ "Roommate"
         ),
         other_cohab = case_when(
           other_cohab == 1 ~ "Other"
         ),
         prim_cohab = coalesce(father, mother, stepfather, stepmother, foster,
                               brother, sister, grandfather, grandmother,
                               relative, spouse, partner, son, daughter,
                               roommate, other_cohab),
         self_eth = coalesce(white, black, asian, pacific, native, other_eth)
         ) |>
  relocate(self_eth, .after = eth) |>
  relocate(prim_cohab, .before = living_arr) |>
  relocate(c(white_yn, black_yn, asian_yn, pacific_yn, native_yn, other_eth_yn),
           .after = self_eth) |>
  select(-white, -black, -asian, -pacific, -native, -other_eth, -father, -mother,
         -stepfather, -stepmother, -foster, -brother, -sister, -grandfather,
         -grandmother, -relative, -spouse, -partner, -son, -daughter, -roommate,
         -other_cohab) |>
  mutate(across(everything(), ~replace(., is.na(.), NA))) |>
  mutate(
    white_yn = case_when(
      white_yn == 1 ~ 1,
      is.na(white_yn) ~ 0),
    black_yn = case_when(
      black_yn == 1 ~ 1,
      is.na(black_yn) ~ 0),
    asian_yn = case_when(
      asian_yn == 1 ~ 1,
      is.na(asian_yn) ~ 0),
    pacific_yn = case_when(
      pacific_yn == 1 ~ 1,
      is.na(pacific_yn) ~ 0),
    native_yn = case_when(
      native_yn == 1 ~ 1,
      is.na(native_yn) ~ 0),
    other_eth_yn = case_when(
      other_eth_yn == 1 ~ 1,
      is.na(other_eth_yn) ~ 0)
    )

################################################################################
# clean up the drug use frequency scale
################################################################################
CHAMP <- CHAMP |> drug_use_freq()

#CHAMP |> head() |> view_html()

################################################################################
# # Dictionary:
################################################################################

# For columns with the suffix _yn, 1 == Yes, 0 == No
# eth == ethnicity/race
# self_eth == self-identified ethnicity/race
# pob == place of birth
# edu == education level
# emp == employment
# por == place of recidence
# prim_cohab == primary cohabitant
# living_arr == living arrangements
# res_XXX_18 == number of residents under/over 18 in the household
# NA = Not Applicable (for y/n questions), or No Answer in other cases

################################################################################
# Writing out the subset

saveRDS(object = CHAMP,
        file = here::here("data", "demog", "demog_clean.rds")
)

message("Saved: demog_clean.rds")

################################################################################
