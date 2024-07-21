# function for listing files without dirs

list.files_only <- function(dir, full.names = T) {
  # use set diff to determine difference in lists
  setdiff(list.files(dir, full.names = full.names),
          list.dirs(dir, recursive = F)
  )
}

# function for reordering the pages for display


list.files_only(dir, full.names = F)

zippy <- function(dir) {
  zip(zipfile = paste0(basename(dir), "_", Sys.Date(), ".zip"),

    files = list.files_only(dir, full.names = F)
    )
}
#zippy(dir = dir)

page_reorder <- function(ord_name_ext = NULL,
                         dir = NULL,
                         prefix = " - ",
                         ...
) {
  here_dir = here::here()
  if (!is.null(dir)) {
    dir = paste(here_dir, dir, sep = "/")
  }
  # backup dir
  zip(paste0(basename(dir), "_", Sys.Date(), ".zip"), list.files(dir, include.dirs = F))

  if (!is.null(ord_name_ext)) {

    # rename string based on order; pad single digits
    ord_name_ext = trimws(unique(ord_name_ext), which = "both") # remove repetitions
    names_prefix = paste(gsub("\\b(\\d)\\b", "0\\1",
                              paste(1 : length(ord_name_ext)))
                         )
    # add the spacing
    names_prefix = paste(names_prefix, prefix)

    # build new name ordered list
    new_names_ordered = paste0(names_prefix, ord_name_ext)

    # retrieve names from dir
    old_names = basename(list.files(dir, full.names = T))

    # keep only names matching ordered_names pattern
    old_names = grep(pattern = paste(ord_name_ext, collapse = "|"),
                     x = old_names, value = TRUE
    )

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


trimws(" c j ", which = "both")


###############################################################
### Module Page
###############################################################
ordered_names <- c("installing_r_and_rtudio.qmd",
                   "creating_a_posit_account.qmd",
                   "installing_git_and_github_desktop.qmd",
                   "geom_bar.qmd",
                   "geom_col.qmd",
                   "geom_point.qmd",
                   "geom_line.qmd",
                   "module_starter_page.qmd",
                   "animation.qmd",
                   "hello.qmd"
                   )

page_reorder(ord_name_ext = ordered_names,
             dir = "modules")
#
#
###############################################################

###############################################################
### xxx Page
###############################################################
