##############################################################
# Load libraries
library(here)

#############################################################
# Directory containing the qmd files
directories <- c("modules", "module_setup")
directories <- here::here(directories)

#############################################################

#############################################################
# Function to update order in YAML
update_yaml_order <- function(filepath, order_value) {
  if (file.exists(filepath)) {
    lines <- readLines(filepath)
    yaml_end <- which(lines == "---")[2] # Find the end of the YAML front matter

    # Check if order line exists
    order_line <- grep("^order:", lines)

    if (length(order_line) > 0 && order_line < yaml_end) {
      # Update existing order line
      lines[order_line] <- paste("order:", order_value)
    } else {
      # Insert new order line before the end of YAML
      lines <- append(lines, paste("order:", order_value), after = yaml_end - 1)
    }

    writeLines(lines, filepath)
  } else {
    message("File does not exist: ", filepath)
  }
}

add_order_for_files <- function(directory,
                         file_order = file_order
                         ) {
  # Loop through the files and update the order

  for (i in seq_along(file_order)) {

  # iterate through all directories
  #for (directory in directories) {
    # Get the list of .qmd files in the directory
    files_in_directory <- list.files(directory,
                                     pattern = "\\.qmd$",
                                     full.names = TRUE,
                                     recursive = TRUE
                                     )
    # get file path
    filepath <- file.path(directory, file_order[i])

    # Check if file exists in the list of files in the directory
    matching_files <- files_in_directory[basename(files_in_directory) == basename(filepath)]

    if (length(matching_files) > 0) {
      update_yaml_order(matching_files[1], i)
    } else {
      message("File not in directory: ", filepath)
    }
    #if (basename(filepath) %in% basename(files_in_directory)) {
 #     update_yaml_order(filepath, i)
 #   } else {
 #     message("File not in directory: ", filepath)
 #   }
  }
}

add_order_for_files2 <- function(
    directory,
    file_order = file_order
    ) {

  # Get the list of .qmd files in the directory recursively
  files_in_directory <- list.files(directory,
                                   pattern = "\\.qmd$",
                                   full.names = TRUE,
                                   recursive = TRUE
                                   )

  # Process each file in the directory
  for (filepath in files_in_directory) {
    file_name <- basename(filepath)

    # Check if file is in the specified order vector
    if (file_name %in% file_order) {

    # Get the order index from file_order
    order_value <- which(file_order == file_name)
    } else {
      # Set order to 0 if file is not in file_order
      order_value <- 0
    }

    # update the yaml code
    update_yaml_order(filepath, order_value)
}
}

# end functions
#############################################################

##############################################################
### Order Setup Module files
##############################################################
file_order <- c(
  "installing_r_and_rstudio.qmd",
  "introduction_to_r_rstudio_and_rmarkdown.qmd",
  "functions_and_scripts.qmd",
  "vectors_and_data_frame_basics.qmd",
  "reading_data_files.qmd",
  "reading_excel_files.qmd"
)

add_order_for_files2(
  directory = here::here("modules_setup"),
  file_order = file_order
  )

##############################################################
### Order Base Module files
##############################################################
file_order <- c(
  #"module_starter_page.qmd"#,
  #"project_management.qmd",
  "installing_and_setting_up_git_and_github",
  "using_git_and_github",
  "graphical_perception.qmd",
  #"reading_data_files.qmd",
  "data_frame_manipulation_and_wrangling.qmd",
  "data_subsets_and_summaries.qmd",
  "considerations_in_data_visualization.qmd",
  "designing_perceptually_efficient_visualizations.qmd",
  "ggplot_and_the_grammar_of_graphics.qmd",
  "visualizing_amounts.qmd",
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
  "multi_panel_plots_faceting.qmd",
  "attentional_control.qmd",
  "titles_captions_and_tables.qmd",
  "themes.qmd"
)
add_order_for_files2(directory = here::here("modules"),
                    file_order = file_order
)
