create_script_template <- function(file) {
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
  # open file and write lines
  if (file.exists(file)) {
    fileConn <- file(file)
    writeLines(lines_to_write, fileConn)
    close(fileConn)
    rm(fileConn)
  } else {
    file.create(file)
    fileConn <- file(file)
    writeLines(lines_to_write, fileConn)
    close(fileConn)
    rm(fileConn)
    message("File does not exist.")
  }
}

create_script_template(here::here("r", "new_script.R"))
