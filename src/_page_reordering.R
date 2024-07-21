# function for listing files without dirs

list.files_only <- function(dir, full.names = T) {
  # use set diff to determine difference in lists
  setdiff(list.files(dir, full.names = full.names),
          list.dirs(dir, recursive = F)
  )
}

# function for reordering the pages for display

page_reorder <- function(ord_name_ext = NULL,
                         dir = NULL,
                         prefix = "_",
                         ...
) {
  here_dir = here::here()
  if (!is.null(dir)) {
    dir = paste(here_dir, dir, sep = "/")
  }
  # backup dir
  #zip(paste0(basename(dir), "_", Sys.Date(), ".zip"), list.files(dir, include.dirs = F))

  if (!is.null(ord_name_ext)) {

    # rename string based on order; pad single digits
    ord_name_ext = trimws(unique(ord_name_ext), which = "both") # remove repetitions
    names_prefix = paste(gsub("\\b(\\d)\\b", "0\\1",
                              paste(1 : length(ord_name_ext)))
                         )

    #print(substring(names_prefix, 6))
    # add the spacing
    names_prefix = paste(names_prefix, prefix, sep = "")
    #print(names_prefix)


    # build new name ordered list
    new_names_ordered = paste0(names_prefix, ord_name_ext)

    new_names_ordered = gsub(" ", "", new_names_ordered, fixed = TRUE)
    #new_names_ordered = gsub("-", "_", new_names_ordered)

    #print(new_names_ordered)

    # retrieve names from dir
    old_names = basename(list.files(dir, full.names = T))

    # keep only names matching ordered_names pattern
    old_names = grep(pattern = paste(ord_name_ext, collapse = "|"),
                     x = old_names, value = TRUE
    )
    #print(old_names)

    # reorder the old names to match new order
    old_names_cleaned = trimws(gsub(".*\\b(\\w+\\.\\w+)$", "\\1",
                                     old_names), which = "both")
    old_names_reordered = old_names[
      (match(ord_name_ext, old_names_cleaned))]

    # copy/rename files
#    file.copy(
#      from = paste(dir, old_names_reordered, sep = "/"),
#      to = paste(dir, paste0("new_",ord_name_ext), sep = "/")
#    )
    file.rename(from = paste(dir, old_names_reordered, sep = "/"),
                to   = paste(dir, new_names_ordered, sep = "/")
              )

  } else message("ord_name_ext is NULL")
}

###############################################################
### Module Page
###############################################################
ordered_names <- c("installing_r_and_rstudio.qmd",
                   #"creating_a_posit_account.qmd",
                   #"installing_git_and_github_desktop.qmd",
                   #"project_management_with_here.qmd",
                   #
                   # week 1
                   # 1
                   # "introduction_to_r_rstudio_and_rmarkdown.qmd",         # Full Day
                   #
                   #
                   # week 2
                   # 2
                   # "installing_git_and_github.qmd",                       # do before class
                   # 3
                   # "using_git_and_github_with_rstudio.qmd",                   # class walk through and activity - Full Day?
                   #
                   #
                   # week 3
                   # 4
                   # "functions_and_scripts.qmd",          # optional
                   # 5
                   # "vectors_and_data_frame_basics.qmd",
                   #
                   #
                   # week 4
                   # 6
                   # "importing_and_exporting_data.qmd",
                   # 7
                   # "variables_and_measures_of_cognition.qmd",                 # hands on
                   #
                   #
                   # week 5
                   # 8
                   # "transforming_data.qmd",
                   # 9
                   # "working_with_cognitive_task_data.qmd",                    # hands on
                   #
                   #
                   # week 6
                   # 10
                   # "summarizing_data.qmd",
                   # 11
                   # "summarizing_cognitive_task_data.qmd"                      # hands on
                   #
                   #
                   # week 7
                   # 12
                   # "visualizing_data.qmd",
                   # "ggplot_and_the_grammar_of_graphics.qmd",
                   # 13
                   # "examining_relationships_in_variables_of_cognition.qmd"    # hands on
                   #
                   #
                   # week 8
                   # 14
                   # "strings_and_factors.qmd",
                   # other?
                   #
                   # week 9
                   # # <span style=""color:#1EDAFB"">Spring Break (no class)</span>"
                   #
                   #
                   # week 10
                   # "<span style=""color:#8DFF0A"">Mid-Term Presentation</span>"
                   #
                   #
                   # week 11
                   # 15
                   # "joining_relational_data.qmd",
                   # 16
                   # "joining_project_data.qmd"                                 # hands on
                   #
                   #
                   # week 12
                   # Discuss Appropriate Models Related to Cognition Readings
                   #
                   #
                   # week 13
                   # 17
                   # "linear_models.qmd"
                   #
                   #
                   # week 14
                   # 18 "exploratory_data_analysis.qmd"
                   #
                   #
                   # week 15
                   # Project week
                   #
                   #
                   #"data_subsets_and_summaries.qmd",


#
# THESE NEED TO be titled correctly with the files in the directory. Some out of order.
# Some missing. Some names
                   )

page_reorder(ord_name_ext = ordered_names,
             dir = "modules")
#
#
###############################################################

#"introduction"

###############################################################
### xxx Page
###############################################################
