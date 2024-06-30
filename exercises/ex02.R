R.utils::sourceDirectory(here::here("r", "functions"))

# adding dates
birthdates <- c("2000-01-09", "1946-10-28",  "1992-05-03",  "2021-08-23",  "1957-03-05")


# adding names
names <- c("Bill", "Jill", "Mill", "Till", "Lill")


# create ages
ages <- get_years_since_birth(birthdates)

ages

mean_ages <- mean(ages)

mean_ages
