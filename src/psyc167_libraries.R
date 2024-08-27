install_if_not_installed <- function(list) {
  new_pkg <- list[
    !(list %in% installed.packages()[,"Package"])]
  if (length(new_pkg)) install.packages(new_pkg)
}

dataviz167_packages <- c(

  # DATA FRAME MANIPULATION
  "tidyverse", "dplyr", "dtplyr", "data.table",
  "car",  "magrittr", "readr",

  # DATES AND TIMES
  "lubridate", "zoo", "xts",

  # DEVELOPMENT
  "devtools", "remotes",

  # COLORS
  #"colorblindr",
  "monochromeR", "viridis", "viridisLite",

  # FONTS and TEXT
  "extrafont", "glue", "showtext", "utf8", "systemfonts",

  # GEOSPATIAL
  "zipcodeR",

  # IMAGES AND GRAPHICS
  "ragg", "Cairo", "magick", "gifski",
  "colourpicker", "RColorBrewer", "colorspace",

  # MODELS AND DATA
  "easystats", "see",
  "boot",    # bootstrapping
#  "broom",   # cleaning up model outputs
#  "lavaan",  # latent variable
#  "ranger",  # random forest
#  "party",   # decision tree
#  "caret",   #
  # http://www.sthda.com/english/articles/35-statistical-machine-learning-essentials/141-cart-model-decision-tree-essentials/
#  "rpart",   #

  # PLOTTING
  "dendextend", "dygraphs",
  "ggplot2", "GGally", "gganimate", "ggiraph", "ggpubr", "ggvis",
  "ggtext", "geomtextpath", "ggthemes", "ggforce",
  "gghighlight", "ggformula", "ggThemeAssist", #"ggAssist",
  "ggrepel", "ggridges", "ggsci", "rcartocolor", "ggdendro", "ggdist",
  "gridExtra", "grid", "ggdark",
  "prismatic", "patchwork", "scico", "highcharter", "corrr",
  "gt", "gtsummary", "ggstatsplot",
  "plotly", "cowplot", "colorspace",

  # PROJECTS AND MANAGEMENT
  "usethis", "here", "gitr", "gh",  "gitcreds", "gert", "renv", # "gitcreds",

  # READING AND WRITING DATA FILES
  "foreign", "haven", "vroom", "xlsx", "readr", "openxlsx", "rio",

  # REFERENCES AND REPORTING
  "bibtex", "knitr", "pandoc", "rstudioapi", "rmarkdown", "quarto", "shiny",
  #"shinydashboard", "shinydashboardPlus",

  # STRING MANIPULATION
  "stringr",

  # TABLES
  "DT", "htmlwidgets", "htmlTable", "kableExtra", "htmltools",

  # OTHER
  "coin", "fs", "zoom", "webshot", "sjPlot", "sqldf", "remotes"
)

install_if_not_installed(dataviz167_packages)

remotes::install_github("clauswilke/colorblindr", force = TRUE)
