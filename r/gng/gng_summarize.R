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
# source the previous steps
# aggregate the data by id
source(here::here("r", "gng", "gng_agg.R"))

################################################################################
# read aggregated data
GNG_AGG_by_id <- readRDS(here::here("data", "gng", "gng_agg_by_id.Rds"))

################################################################################
# Summarize
GNG_AGG_by_group <-
  GNG_AGG_by_id |>
  group_by(wave, target) |>
  summarise(across(.cols = c("rt_mean", "accuracy_mean"),
                   .fns = summarize_across[c(1, 2, 5)],
                   .names = "{.col}_{.fn}"
  )) |>
  ungroup()

GNG_AGG_by_group |> view_html()
