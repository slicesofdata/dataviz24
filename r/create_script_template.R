create_script_template <- function(file_path = here::here("temp_script_file.R")) {
# this function creates a script file along with author,
# description, and sections for adding code in blocks
# filepath is needed

  lines_to_write =
    "################################################################################
# Author:
#
# Description:
#
#
################################################################################
# load libraries and functions\n\n\n
################################################################################
# source relevant script\n\n\n
################################################################################
# read data\n\n\n
################################################################################
# ...\n\n\
################################################################################
# write out data?\n\n\n
################################################################################
"
  # create/open file and write lines
  if (file.exists(file_path)) {
    message(
      paste0("Filename already exists: ", file_path, "\nChoose new name or new directory"))
  } else {
    file.create(file_path)
    fileConn <- file(file_path)
    writeLines(lines_to_write, fileConn)
    message(paste0("File created: ", file_path))
    close(fileConn)
    rm(fileConn)
  }
}

create_script_template() #here::here("r", "new_script.R"))
