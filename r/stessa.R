library(dplyr)
library(readxl)

# set the path
the_year <- 2024
the_dir <- "C:/Users/gcook/My Drive/Alumni Partners/Properties/339 S Poplar/Accounting, Insurance, and Reports/statements"
the_dir <- paste0(the_dir, "/", the_year, "-Statements")


#C:\Users\gcook\My Drive\Alumni Partners\Properties\339 S Poplar\Accounting, Insurance, and Reports\statements\2024-Statements

# get the file list
files <- list.files(path = the_dir, full.names = TRUE)
xls_files <- files[grep("\\.xls$", files)]
xls_files <- xls_files[2]

# define columns for stessa
stessa_cols <- c("Date", "Amount", "Payee", "Description")

# clean up monthly files
for (f in xls_files) {
  # read
  dat <- readxl::read_xls(path = f, skip = 1)

  # clean up to make stessa ready
  dat <-
    dat |>
    rename_with(~gsub(" ", "_", .)) |>
    mutate(Payee = "",
           Amount = Debit_Amount
    ) |>
    # change debits to negative for stessa
    mutate(Amount = -1 * as.double(gsub("\\$|,", "", Amount))) |>
    # keep positive for deposits
    mutate(Credit_Amount = as.numeric(gsub("\\$|,", "", Credit_Amount))) |>
    # merge the columns
    mutate(Amount = ifelse(is.na(Amount), Credit_Amount, Amount)) |>
    # reduce to relevant cols
    select(any_of(stessa_cols))
    # remove any duplicates
    # distinct()
    #
    # Add payee details
    # mutate(Payee = case_when(Description contains("))
    # mutate(Payee = case_when(
    # stringr::str_detect(Description, "PENNSYLVANIA POW/BILL PAYMT") ~ "PP&L",
    #  stringr::str_detect(Description, "aqua") ~ "Aqua",
    #  #stringr::str_detect(Description, "FULTON BANK/BILL PAYMT") ~ "Mortgage Payment"
    #  TRUE ~ NA
    # )) |>
    # mutate(Category = case_when(
    #  stringr::str_detect(Payee, "PP&L") ~ "Electric",
    #  stringr::str_detect(Payee, "Aqua") ~ "Water & Sewer",
    #  stringr::str_detect(Description, "FULTON BANK/BILL PAYMT") ~ "Mortgage Payment",
    #  TRUE ~ NA
    # ))

  # rename and write csv for upload
  readr::write_csv(x = dat, file = gsub(".xls", "-stessa.csv", x = f))
}
# end clean/write

#dat |>
#  mutate(Payee = case_when(
#    stringr::str_detect(Description, "PENNSYLVANIA POW/BILL PAYMT") ~ "PP&L",
#    stringr::str_detect(Description, "aqua") ~ "Aqua",
#    #stringr::str_detect(Description, "FULTON BANK/BILL PAYMT") ~ "Mortgage Payment"
#    TRUE ~ NA
#  )) |>
#  mutate(Category = case_when(
#    stringr::str_detect(Payee, "PP&L") ~ "Electric",
#    stringr::str_detect(Payee, "Aqua") ~ "Water & Sewer",
#    stringr::str_detect(Description, "FULTON BANK/BILL PAYMT") ~ "Mortgage Payment",
#    TRUE ~ NA
#  )) |> view_html()


