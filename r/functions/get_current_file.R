# read the current file
#
get_current_file <- function(message = TRUE) {
  # get the current file.
  file = rstudioapi::getSourceEditorContext()$path %>% sub(".*/", "", .)

  # print message
  #if(message) message("Reading: ", file)
}

