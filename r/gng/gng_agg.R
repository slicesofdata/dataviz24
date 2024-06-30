################################################################################
# Author:
#
################################################################################
# Description:
# reads and cleans gng_long_cleaned.Rds
# filters rows
# aggregation/summary


################################################################################
# garbage clean
#gc(verbose = FALSE, full = TRUE, reset = TRUE)

# load libraries and functions
library(dplyr)
R.utils::sourceDirectory(here::here("r", "functions"))

################################################################################
# source the previous steps (read raw, subsets to wide, converts to long, then
# writes sspan_long.Rds)
source(here::here("r", "gng", "gng_long_clean.R"))

################################################################################
# read long cleaned data
GNG_CLEAN <- readRDS(here::here("data", "gng", "gng_long_clean.Rds"))

#glimpse(GNG_CLEAN)

################################################################################
# Aggregate
GNG_AGG_by_id <-
  GNG_CLEAN |>
  select(c(id_subject, wave, target, accuracy, rt)) |>
  group_by(id_subject, wave, target) |>
  summarise(across(.cols = c("rt", "accuracy"),
                   .fns = summarize_across[c(1, 2, 5)],
                   .names = "{.col}_{.fn}"
  )
  ) |>
  ungroup()


################################################################################
# save aggregated
saveRDS(GNG_AGG_by_id, here::here("data", "gng", "gng_agg_by_id.Rds"))

message("Saved: gng_agg_by_id.Rds")



