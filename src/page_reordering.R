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

  if (!is.null(dir)) {
    #dir = paste(here_dir, dir, sep = "/")
    dir = here::here(dir)
  } else {
    dir = here::here()
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
                     x = old_names,
                     value = TRUE
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
ordered_names <- c(
  #"installing_r_and_rstudio.qmd",
  #"creating_a_posit_account.qmd",
  #"installing_git_and_github_desktop.qmd",
  #"project_management_with_here.qmd",  #                   "module_starter_page.qmd"#,
  "project_management.qmd",
  #"installing_and_setting_up_git_and_github",
  #"using_git_and_github",
  "graphical_perception.qmd",
  #"reading_data_files.qmd",
  "data_frame_manipulation_and_wrangling.qmd",
  "data_subsets_and_summaries.qmd",
  "considerations_in_data_visualization.qmd",
  "designing_perceptually_efficient_visualizations.qmd",
  "ggplot_and_the_grammar_of_graphics.qmd",
  "visualizing_amounts.qmd",
  #"geom_bar.qmd",
  "visualizing_associations.qmd",
  "spatial_position_and_adjustment.qmd",
  "color_scales_and_palettes.qmd",
  "histograms_and_density_plots.qmd",
  "coordinates_axes_and_position_scales.qmd",
  "statistical_transformations.qmd",
  "more_data_wrangling",
  "visualizing_more_distributions.qmd",
  "visualizing_uncertainty.qmd",
  "visualizing_trends.qmd",
  "legends_and_arrangement.qmd",
  "annotation_and_text.qmd",
  "attentional_control.qmd",
  "multi_panel_plots_faceting.qmd"
  # add
  #"titles_captions_and_tables",
  #"themes"


  # "animation.qmd",
  #                   "hello.qmd"
  #
  # THESE NEED TO be titled correctly with the files in the directory. Some out of order.
  # Some missing. Some names
)


page_reorder(ord_name_ext = ordered_names,
             dir = "modules"
             )
#
#
###############################################################

#"introduction"

###############################################################
### xxx Page
###############################################################
