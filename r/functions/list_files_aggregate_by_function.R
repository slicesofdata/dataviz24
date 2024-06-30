
do = FALSE
if (do) {
list_files_aggregate_by_function <- function(
    path,                        # the directory
    pattern = ".csv",            # the file extension
    col_names,            # a column vector of names
    min_size = 500              # the minimum file size to include
) {
  # load libraries
  pacman::p_load(tidyverse)

  #####################################################################
  # check for file names and gather vector of matched file names
  #####################################################################
  if (missing(col_names)) {
    print("missing")
  } else {
    print("not missing")
  }
}

DAT <- list_files_aggregate_by_function(path = "C:/Users/gcook/Sync/git/pm/axcpt-pm-shape/pmaxcpt-lindsey/data/raw",
                                        col_names = starts_with("axcpt_")
                                        #                                       col_names = c("axcpt_subid", "axcpt_date",  "axcpt_exp",  "axcpt_colorCond",  "axcpt_cond", "axcpt_phaseFile", "axcpt_probe_rt")
)


list_files_aggregate_by_function <- function(
    path,                        # the directory
    pattern = ".csv",            # the file extension
    col_names,            # a column vector of names
    min_size = 500              # the minimum file size to include
) {
  # load libraries
  pacman::p_load(tidyverse)

  #####################################################################
  # check for file names and gather vector of matched file names
  #####################################################################
  if (!missing(col_names)) {
    files = list.files(path, pattern = pattern, full.names = TRUE)

    # filter files by size
    files = files[file.size(files) > min_size]

    #####################################################################
    # iterate over each file, keep file path if names are present (or missing)
    #####################################################################
    matched_files = c()
    mismatched_files = c()

    for (file in files) {

      #####################################################################
      # check the names
      #####################################################################
      col_check = sapply(col_names, function(x) grepl(x, readLines(file, n = 1)))
      missing_cols = names(col_check[col_check == FALSE])

      #####################################################################
      # message if name is missing, otherwise read
      #####################################################################
      if (length(missing_cols)) {
        #  # report the missing
        message("Columns missing in ", basename(file), ":")
        message(paste("  ", missing_cols), "\n")
      }

      # read the top row for the file and check the presence of names, if all
      if (all(col_check)) {
        # names are present, append file name
        matched_files = append(matched_files, file)
      } else {
        # append the files missing the cols
        mismatched_files = append(mismatched_files, basename(file))
      }
    } # getting match and mismatched

    #####################################################################
    # now read files and combine
    #####################################################################
    df_list = lapply(matched_files, function(file) {
      read.csv(file, stringsAsFactors = FALSE) |> # read data frame
        select({{col_names}})                   # select only col_names
    })

    # return the data frame
    return(list(data = do.call(rbind, df_list),
                missing = mismatched_files
    )
    )
  } else {
    message("Message: No arguemnt passed to col_names.")
  }
}
#DAT <-
#  list_files_aggregate_by_colname(
#    path = here::here("data"),
#    col_names = c("ogt_subid", "ogt_rt")
#  )

#DAT$data
#DAT$missing




xxx <- function(path = "C:/Users/gcook/Sync/git/pm/axcpt-pm-shape/pmaxcpt-lindsey/data/raw",
                pattern = ".csv",            # the file extension
                col_names,
                min_size = 500,              # the minimum file size to include
                keep_col_selection = TRUE, ...    # to keep only the selected columns
) {

  dfs_list <- list()

  files = list.files(path, pattern = pattern, full.names = TRUE)

  # filter files by size
  files = files[file.size(files) > min_size]

  #####################################################################
  # iterate over each file, keep file path if names are present (or missing)
  #####################################################################
  matched_files = c()
  mismatched_files = c()

  df_list = list()
  for (file in 1:length(files)) {

    tryCatch(
      {
        # Attempt to read the CSV file
        df <- read.csv(files[file])

        # Check if the column names exist
        if (missing(colnames({{col_names}}))) {
          warning("Column names are missing. Skipping file.")
          return(NULL)
        } else {
          return(df)
        }
      },
      warning = function(w) {
        # Handle warning (e.g., missing column names)
        cat("Warning:", conditionMessage(w), "\n")
        return(NULL)
      },
      error = function(e) {
        # Handle error (e.g., file not found)
        cat("Error:", conditionMessage(e), "\n")
        return(NULL)
      }
    )


    #print(file)
    # Read the file
    #     df <- read.csv(files[file])

    #      df_selected <- df |> select({{col_names}})

    # Store the selected data frame in the list
    #                       dfs_list[[file]] <- df_selected
    #       }

    # Combine all data frames into a single data frame
    #return(dplyr::bind_rows(dfs_list))
    df_list[[file]] <- read.csv(files[file]) |> select(all_of({{col_names}}))
  }
  return(bind_rows(df_list))
}

D <- xxx(col_names = starts_with("axcpt_"))
D <- xxx(col_names = c("axcpt_", "axcpt_date"))
D |> filter(!is.na(axcpt_subid)) |> view_html()


vvv <- function(path = "C:/Users/gcook/Sync/git/pm/axcpt-pm-shape/pmaxcpt-lindsey/data/raw",
                pattern = ".csv",            # the file extension
                col_names,
                min_size = 500,              # the minimum file size to include
                keep_col_selection = TRUE, ...    # to keep only the selected columns
) {

  dfs_list <- list()

  files = list.files(path, pattern = pattern, full.names = TRUE)

  # filter files by size
  files = files[file.size(files) > min_size]

  #####################################################################
  # iterate over each file, keep file path if names are present (or missing)
  #####################################################################
  matched_files = c()
  mismatched_files = c()

  for (file in files) {

    #####################################################################
    # check the names
    #####################################################################
    col_check = sapply(col_names, function(x) grepl(x, readLines(file, n = 1)))
    missing_cols = names(col_check[col_check == FALSE])

    #####################################################################
    # message if name is missing, otherwise read
    #####################################################################
    if (length(missing_cols)) {
      #  # report the missing
      message("Columns missing in ", basename(file), ":")
      message(paste("  ", missing_cols), "\n")
    }

    # read the top row for the file and check the presence of names, if all
    if (all(col_check)) {
      # names are present, append file name
      matched_files = append(matched_files, file)
    } else {
      # append the files missing the cols
      mismatched_files = append(mismatched_files, basename(file))
    }
  } # getting match and mismatched

  #print(matched_files)

  for (file in matched_files) {
    # Read the file
    # df <- read.csv(file)

    #df_selected <- df |> select(where({{col_names}}))
    #df_selected <- df |> select(({{col_names}}))

    # Store the selected data frame in the list
    #    dfs_list[[file]] <- df_selected
  }

  # Combine all data frames into a single data frame
  #  return(dplyr::bind_rows(dfs_list))



  return(read.csv(Z[1]) |> select(all_of({{col_names}})))
}


D <- vvv(col_names = starts_with("axcpt_"))
D <- vvv(col_names = c("axcpt_subid", "axcpt_date"))
D |> filter(!is.na(axcpt_subid)) |> head() |> view_html()

#####################################################################
# iterate over each file, keep file path if names are present (or missing)
#####################################################################
matched_files = c()
mismatched_files = c()

for (file in files) {

  #####################################################################
  # check the names
  #####################################################################
  col_check = sapply(col_names, function(x) grepl(x, readLines(file, n = 1)))

  col_check = sapply(col_names, function(x) grepl(x, readLines(file, n = 1)))
  missing_cols = names(col_check[col_check == FALSE])

  #####################################################################
  # message if name is missing, otherwise read
  #####################################################################
  if (length(missing_cols)) {
    #  # report the missing
    message("Columns missing in ", basename(file), ":")
    message(paste("  ", missing_cols), "\n")
  }

  # read the top row for the file and check the presence of names, if all
  if (all(col_check)) {
    # names are present, append file name
    matched_files = append(matched_files, file)
  } else {
    # append the files missing the cols
    mismatched_files = append(mismatched_files, basename(file))
  }
} # getting match and mismatched




vvv(col_names = ends_with("rt"))


vvv <- function(path, col_names) {
  # Get list of files in the specified directory
  files <- list.files(path = path, full.names = TRUE)

  # Initialize an empty list to store data frames
  dfs_list <- list()

  # Read each file and aggregate by column names
  for (file in files) {
    # Read the file
    df <- read.csv(file)

    # Aggregate by column names
    if (is.character(col_names)) {
      # If col_names is a character vector, select columns based on their names
      df_selected <- df %>% select(all_of(col_names))
    } else if (is.function(col_names)) {
      # If col_names is a function, apply it to select columns
      df_selected <- df %>% select(where(col_names))
    } else {
      stop("col_names must be a quoted vector or a function")
    }

    # Store the selected data frame in the list
    dfs_list[[file]] <- df_selected
  }

  # Combine all data frames into a single data frame
  combined_df <- bind_rows(dfs_list, .id = "file_name")

  return(combined_df)
}
V <- vvv(path = "C:/Users/gcook/Sync/git/pm/axcpt-pm-shape/pmaxcpt-lindsey/data/raw",
         col_names = starts_with("axcpt_")
)

}
