summarize_mean <- function(data, vars) {
  data %>% 
    summarise(n = n(), across({{ vars }}, mean))
}


summarize_mean_by <- function(data, group, vars) {
  data %>% 
    group_by({{group}}) %>%
    summarise(n = n(), across({{ vars }}, mean))
}

f <- function(data, group) {
  data %>% 
    group_by({{group}}) %>%
    count() 
}

#f(SWIM, all_of("Time")) 
