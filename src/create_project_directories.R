################################################################################
# Author: https://github.com/slicesofdata
#
# Description:
# function to create project directories and files
################################################################################



#############################################################################
# specify the directories
directories <- c(
  "data/raw",
  "data/interim",
  "data/processed",
  "docs",
  "report/figs",
  "report/images",
  "refs",
  "src/data",
  "src/figs",
  "src/functions"
)

# Define the files to be created
files <- list(
  "README.md" = "# Project Title\n\nProject Description",
  "requirements.txt" = "# List of libraries and dependencies\n"
)

# Function to create directories
create_directories <- function(dirs) {
  for (dir in dirs) {
    dir_path <- here::here(dir)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
      cat(paste("Created directory:", dir_path, "\n"))
    } else {
      cat(paste("Directory already exists:", dir_path, "\n"))
    }
  }
}

# Function to create files
create_files <- function(files) {
  for (file in names(files)) {
    file_path <- here::here(file)
    if (!file.exists(file_path)) {
      writeLines(files[[file]], file_path)
      cat(paste("Created file:", file_path, "\n"))
    } else {
      cat(paste("File already exists:", file_path, "\n"))
    }
  }
}

# Create the directories
create_directories(directories)

# Create the files
create_files(files)
