dataviz166_packages <- c(

  # DATA FRAME MANIPULATION
  "tidyverse", "dplyr", "dtplyr", "data.table",
  "car",  "magrittr",

  # DATES AND TIMES
  "lubridate", "zoo", "xts",

  # DEVELOPMENT
  "devtools", "remotes",

  # FONTS and TEXT
  "glue", "utf8",

  # IMAGES AND GRAPHICS
  "ragg", "Cairo", "magick",

  # MODELS AND DATA
  "easystats", "see",
  "boot",    # bootstrapping
  "broom",   # cleaning up model outputs
  "lavaan",  # latent variable
  "ranger",  # random forest
  "party",   # decision tree
  "caret",   #
  # http://www.sthda.com/english/articles/35-statistical-machine-learning-essentials/141-cart-model-decision-tree-essentials/
  "rpart",   # decision tree

  # PLOTTING
  "dendextend", "dygraphs",
  "ggplot2", "GGally", "ggiraph", "ggpubr",
  "gt", "gtsummary",
  "plotly", "cowplot",
  "rpart.plot", # partial R and plotting

  # PROJECTS AND MANAGEMENT
  "gert", "gitr", "gh", "gitcreds", "usethis", "here", "renv",

  # READING AND WRITING DATA FILES
  "foreign", "haven", "vroom", "xlsx", "readr", "openxlsx", "rio",

  # REFERENCES AND REPORTING
  "bibtex", "knitr", "pandoc", "rstudioapi", "rmarkdown", "quarto",
  "shiny", "shinydashboard", "shinydashboardPlus",

  # STRING MANIPULATION
  "stringr",

  # TABLES
  "DT", "htmlwidgets", "htmlTable", "kableExtra",

  # confidence intervals
  "coin",

  # OTHER
  "fs", "zoom",
  "webshot", "sjPlot", "sqldf"
)

# install.packages(dataviz166_packages, dep = T)

install_if_not_installed <- function(list) {
  new_pkg <- list[
    !(list %in% installed.packages()[,"Package"])]
  if(length(new_pkg)) install.packages(new_pkg)
}

install_if_not_installed(dataviz166_packages)
