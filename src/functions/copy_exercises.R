# Exercise files are created in /exercises but
# will render for the website at /docs/exercies

# Rather than rebuild the entire website, copy files
# from /exercises to /docs/exercises

copy_exercises <- function(from = here::here("exercises"),
                           to = here::here("docs")
                           ) {
  file.copy(from = from,
            to = to,
            overwrite = TRUE,
            recursive = TRUE,
            copy.mode = TRUE
            )
}

