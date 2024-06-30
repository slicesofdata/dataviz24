# simply makes subdirs in project dir

make_proj_dirs <- function(dirs = c("api",
                                    "data",
                                    "docs",
                                    "figs",
                                    "images",
                                    "output",
                                    "r",
                                    "r/src",
                                    "refs",
                                    "report"
                                    )
                           ) {
  suppressWarnings(
    lapply(dirs, FUN = function(x) dir.create(here::here(x)) ))
}

writeLines_to_File <- function(
    file,
    lines
    ) {
  # open file and write lines
  if (file.exists(file)) {
    fileConn <- file(file)
    writeLines(lines, fileConn)
    close(fileConn)
    rm(fileConn)
  } else {
    message("File does not exist.")
  }
}


write_git_ignore_project <- function(
    file = ".gitignore",
    lines =   c(".Rproj.user", ".Rproj.user/", ".Rhistory", ".RData", ".Ruserdata", ".Rprofile",
                # data files
                " ", "*.RData", "*.rdb", "*.rdx"
                )
    ) {
  # open file and write lines
  if (file.exists(file)) {
    fileConn <- file(file)
    writeLines(lines, fileConn)
    close(fileConn)
    rm(fileConn)
  } else {
    #
    message("File does not exist.")
  }
}



write_git_ignore_course <- function(
    file = ".gitignore",
    lines =   c(".Rproj.user", ".Rproj.user/", ".Rhistory", ".RData", ".Ruserdata", ".Rprofile",
                # data files
                " ", "*.RData", "*.rdb", "*.rdx",
                " ", "/.quarto/", "_freeze/",
                " ", "_omit/", "_notes/", "notes/", "homework/key", "exercises/key"
                )
    ) {
  # open file and write lines
  if (file.exists(file)) {
    fileConn <- file(file)
    writeLines(lines, fileConn)
    close(fileConn)
    rm(fileConn)
  } else {
    #
    message("File does not exist.")
  }
}

make_course_dirs <- function(dirs = c(
  "_notes",
  "_omit",
  "_omit/data", "_omit/exercises", "_omit/fonts",  "_omit/git_tips",
  "_omit/homework", "_omit/notes", "_omit/exercises",
  "_omit/projects", "_omit/r", "_omit/readings", "_omit/exercises",
  "_omit/reflection", "_omit/refs", "_omit/slides",

  "cheatsheets","exercises", "figs", "homework", "images", "modules",
  "project", "refs", "report", "resources", "slides", "syllabus",
  "r", "r/src", "r/functions"
)
) {
  suppressWarnings(
    lapply(dirs, FUN = function(x) dir.create(here::here(x)) ))
}


