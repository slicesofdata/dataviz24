# vrooms/opens and aggregates data files specifed by column name vector

library(tidyverse)
library(vroom)

list_files_aggregate_by_colname <- function(
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
            select(all_of({{col_names}}))                   # select only col_names
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
