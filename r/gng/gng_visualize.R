################################################################################
# Author:
#
################################################################################
# Description:
# reads and cleans gng_long_cleaned.Rds
# filters rows
# aggregation/summary
# plots

################################################################################
# garbage clean
#gc(verbose = FALSE, full = TRUE, reset = TRUE)

# load libraries and functions
library(dplyr)
library(ggplot2)
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

GNG_AGG_by_group

################################################################################
# a simple bar plot
GNG_AGG_by_group |>
  mutate(rt = rt_mean_mean) |>
  ggplot(mapping = aes(
    x = target,
    y = rt
    )) +
  geom_col()

# but by wave?
GNG_AGG_by_group |>
  mutate(rt = rt_mean_mean) |>
  ggplot(mapping = aes(
    x = target,
    y = rt
  )) +
  geom_col(mapping = aes(fill = wave))

# dodge
GNG_AGG_by_group |>
  mutate(rt = rt_mean_mean) |>
  ggplot(mapping = aes(
    x = target,
    y = rt
  )) +
  geom_col(mapping = aes(fill = wave),
           position = "dodge2"
  )


# make color categorical
GNG_AGG_by_group |>
  mutate(rt = rt_mean_mean) |>
  mutate(wave = as.factor(wave)) |>
  ggplot(mapping = aes(
    x = target,
    y = rt
  )) +
  geom_col(mapping = aes(fill = wave),
           position = "dodge2"
  )

# create the subgroup for the plot below
data2 <- GNG_AGG_by_group |>
  filter(wave == 1) |>
  mutate(rt = rt_mean_mean) |>
  mutate(wave = as.factor(wave))

# rt
GNG_AGG_by_id |>
  filter(wave == 1) |>
  mutate(rt = rt_mean) |>
  mutate(wave = as.factor(wave)) |>
  ggplot(mapping = aes(
    x = target,
    y = rt
  )) +
  geom_point(position = position_jitter(),
             alpha = .5
             ) +
  geom_point(data = data2,
             mapping =  aes(y = rt,
                            x = target
                            ),
             col = "red",
             size = 4,
             alpha = .5
  )

# see also position_jitter_dodge

MERGE <- readRDS(here::here("data", "merged", "gng_sspan_merged.Rds"))


MERGE |>
  filter(wave == 1) |>
  ggplot(mapping = aes(x = rt_mean)) +
  geom_histogram(
    mapping = aes(fill = wave),
    binwidth = 10,
                 fill = "skyblue",
                 color = "darkgrey"
                 ) +
  theme_minimal()

MERGE |>
  #filter(wave == 1) |>
  ggplot(mapping = aes(x = rt_mean,
                       fill = as.factor(wave)
                       )
         ) +
  geom_histogram(
  #  mapping = aes(),
    position = "identity",
    alpha = 0.4,
    #bins = 100,
    binwidth = 10,
    color = "black"
    ) +
  facet_wrap(facets = vars(wave),
             ncol = 1
  ) +
  scale_fill_manual(values = c("black", "red", "blue")) +
  theme_minimal()

MERGE |>
  #filter(wave == 1) |>
  ggplot(mapping = aes(x = rt_mean,
                       fill = as.factor(wave)
  )
  ) +
  geom_density(alpha = 0.5) +
  facet_wrap(facets = vars(wave),
             ncol = 1
  ) +
  scale_fill_manual(values = c("black", "red", "blue"))

MERGE |>
  #filter(wave == 1) |>
  ggplot(mapping = aes(x = rt_mean,
                       fill = as.factor(wave)
  )
  ) +
  geom_density(alpha = 0.5) +
  facet_grid(wave ~ target) +
  scale_fill_manual(values = c("black", "red", "blue")) +
  theme_minimal()


MERGE |>
  #filter(wave == 1) |>
  ggplot(mapping = aes(x = accuracy_mean,
                       fill = as.factor(wave)
  )
  ) +
  geom_density(alpha = 0.5) +
  facet_grid(wave ~ target) +
  scale_fill_manual(values = c("black", "red", "blue")) +
  theme_minimal()

MERGE |>
  filter(wave == 1) |>
  ggplot(mapping = aes(x = rt_mean,
                       fill = as.factor(wave)
  )
  ) +
  geom_density(alpha = 0.5) +
  facet_wrap(facets = vars(target), ncol = 1) +
  scale_fill_manual(values = c("black", "red", "blue")) +
  theme_minimal()



MERGE |>
  filter(wave == 1) |>
  ggplot(mapping = aes(x = rt_mean,
                       fill = as.factor(wave)
  )
  ) +
  geom_density(alpha = 0.5) +
  facet_wrap(facets = vars(target),
             ncol = 1
  ) +
  scale_fill_manual(values = c("black", "red", "blue"))




# Anyone participate in both tasks?
MERGE |>
  ggplot(mapping = aes(x = rt_mean,
                       y = sspan
                       )) +
  geom_point(position = position_jitter())



