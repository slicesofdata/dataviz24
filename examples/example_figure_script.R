################################################################################
# Author: my name, email
#
# Description:
# A clear description or what does the script does
################################################################################
# Load libraries and functions
library(dplyr)
library(ggplot2)
library(ragg)     # the r agg device library https://ragg.r-lib.org/
R.utils::sourceDirectory(here::here("src", "functions"))

################################################################################
# load data for ...
dat <- readRDS(file = here::here("data", "raw", "mtcars.Rds"))

################################################################################
# ggplot2 code for creating visualization about ...

mtcars_plot <-
  dat |>
  ggplot() +
  geom_point(mapping = aes(x = wt, y = mpg))


################################################################################
# code to save plot

ggsave(filename = here::here("report", "figs", "my_awesome_plot.png"),
       plot = mtcars_plot,  # last_plot(),  the default is the last plot
       device = ragg::agg_png,
       dpi = 320            # 320 retina, 300 is fine
)

