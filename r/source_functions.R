# save your function scripts /r/functions
# source the directory
# setting modifiedOnly = F will source functions whether or not you modified them
message("Defining functions in r/functions")
R.utils::sourceDirectory(here::here("r", "functions"))
#R.utils::sourceDirectory(here::here("R", "functions"), modifiedOnly = F)
