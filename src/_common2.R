# General Course Details
the_email        <- "gcook@CMC.edu"
the_semester     <- "Fall 2024"
the_course       <- "PSYC 167"
the_section      <- paste(the_course, "Sect-01", sep = ", ")
the_section      <- paste(the_course)
the_day_time     <- "T/R 1:15-2:30 PM or 2:45-4:00PM (Pacific)"
the_location     <- "Location: Roberts North, 102"
the_office_hours <- "T/R: 12-1pm"
the_instructor   <- "Gabriel I. Cook"
the_contact      <- paste0("Discord (preferred) or Email: ", the_email, " (*please put '", the_course," in subject line)*")
the_credit       <- "3 hours; 1 credits"
# End General Course Details

the_website <- "https://gabrielcook.xyz"
the_website_sub_course <- "dataviz24"
the_course_site <- paste(the_website, the_website_sub_course, sep = "/")
the_course_upload_link <- "https://claremontmckenna.app.box.com/f/140969a32dea44e9ab6c0c00146d66e3"

github_user <- "slicesofdata"
github_raw_url <- "https://raw.githubusercontent.com"

github_raw_link <- function(subdir = "modules",
                            file = ""
                            ) {
 return(
   paste(github_raw_url,
         github_user,
         "main",
         subdir,
         file,
         sep = "/"
         )
   )
}

course_links_grep <- function(path = "modules",
                              pattern = "git",
                              replace = the_course_site
                              ) {
  library(magrittr)
  return(
    list.files(path = here::here(path),
             full.names = T
  ) |>
    stringr::str_subset(pattern) %>%
    gsub(here::here(), replace, .) |>
    print(quote = FALSE)
  )
}

# huber text
huber_pdf <- "https://www.markhuberdatascience.org/_files/ugd/c2b9b6_543ea42a1ea64e32b4440b34ffd71635.pdf"
front_matter <- 10
huber_intro_ch <- glue::glue(huber_pdf,"#page=",front_matter + 3)
huber_rmarkdown_ch <- glue::glue(huber_pdf,"#page=",front_matter + 10)
huber_graphing_ch <- glue::glue(huber_pdf,"#page=",front_matter + 17)
huber_transformation_ch <- glue::glue(huber_pdf,"#page=",front_matter + 59) # 5
huber_summaries_ch <- glue::glue(huber_pdf,"#page=",front_matter + 71)
huber_eda_var_ch <- glue::glue(huber_pdf,"#page=",front_matter + 81)
huber_eda_covar_ch <- glue::glue(huber_pdf,"#page=",front_matter + 93)
huber_import_ch <- glue::glue(huber_pdf,"#page=",front_matter + 103)
huber_tidy_data_ch <- glue::glue(huber_pdf,"#page=",front_matter + 123)
huber_relational_ch <- glue::glue(huber_pdf,"#page=",front_matter + 141)
huber_filtering_joins_ch <- glue::glue(huber_pdf,"#page=",front_matter + 150)
huber_strings_ch <- glue::glue(huber_pdf,"#page=",front_matter + 155)
huber_regex_ch <- glue::glue(huber_pdf,"#page=",front_matter + 162)
huber_using_regex_ch <- glue::glue(huber_pdf,"#page=",front_matter + 170)
huber_func_patterns_ch <- glue::glue(huber_pdf,"#page=",front_matter + 182)
huber_factors_ch <- glue::glue(huber_pdf,"#page=",front_matter + 186)
huber_sql_ch <- glue::glue(huber_pdf,"#page=",front_matter + 199)
huber_writing_functions_ch <- glue::glue(huber_pdf,"#page=",front_matter + 232)
huber_modeling_ch <- glue::glue(huber_pdf,"#page=",front_matter + 241)

#message("done")
