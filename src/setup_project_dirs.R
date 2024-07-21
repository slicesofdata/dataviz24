# simply makes subdirs in project dir

message("Loading functions...")
source(here::here("r", "functions", "make_proj_dirs.R"))
message("Building project directories...")
make_proj_dirs()
message("Writing to '.gitignore' file...")
write_git_ignore_project()
# write_git_ignore_course()
