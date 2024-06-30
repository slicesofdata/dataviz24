# takes a data vector and calculates the years passed until now
#
get_years_since_birth <- function(dob) {

  if (!hasArg(dob)) {
    message("Error: dob missing/no argument provided")

  }
  else {
    # make string a data
    dob = lubridate::as_date(dob)

    # obtain the difference in time in days
    diff = difftime(time1 = Sys.Date(), time2 = dob, units = "days")

    # create age base on days
    age = as.numeric(diff / 365.25)

    # return the age in years, truncated
    return(trunc(age))
  }
}

# message("get_years_since_birth defined.")
