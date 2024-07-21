# function takes survey items with response options in wide format
# and transforms into a single variable and relocates back to original
# location in data frame

# assumes the option variables are named option<n>

recode_wide_options_to_single_var <- function(
    data,
    base_varname,
    to
    ) {

  return(
    tidyr::pivot_longer(data = {{data}},
                        cols = starts_with(base_varname),
                        names_to = "option",
                        values_to = "the_value"
    ) |>
      # remove NA rows to retain only the rows with a response
      filter(!is.na(the_value)) |>

      # replace the values_to variable with the response option ('option' object created by names_to in pivot)
      mutate({{to}} := (gsub(paste0(to, "option"), "", option))) |>
      # mutate({{to}} := as.numeric(gsub(paste0(to, "option"), "", option))) |>

      # remove 'option' string from variable
      #      mutate(option = gsub("option", "", option)) |> # paste0(to, "option"), "", option))) |>

      #    mutate(x = option) |>
      mutate({{to}} := gsub(base_varname, "", option)) |> # paste0(to, "option"), "", option))) |>

      # remove the temporary names_to and values_to variables
      select(-option) |>
      select(-the_value) |>
      # position the {to} variable at original location
      relocate(!!as.name(to),
               .after = which(colnames({{data}}) ==
                                paste0(to, "option", "1")) - 1
      )
  )
}


# needs a corresponding function for the response options verbal responses
# to replace the number with a label as a new variable.
#car::Recode(var, "1 = 'this'; 2 = 'that'; 3 = 'other'")
