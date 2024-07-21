# cleans the data file


readr::read_csv(here::here("data", "swim", "cms-top-all-time-2023-swim.csv")) %>%
  mutate(FullName = name) %>%
  mutate(time = ifelse(stringr::str_detect(time, ":"), time, paste0("0:", time))) %>%
  mutate(MinSec = lubridate::ms(time)) %>%
  mutate(Seconds = lubridate::period_to_seconds(MinSec)) %>%
  relocate(time, .after = team) %>% 
  tidyr::separate_wider_delim(., 
                                cols = name,
                                delim = " ",
                                names = c("First", "Last"),
                                too_many = "drop"
    ) %>%
  mutate(Last_FI = paste(Last, substring(First, first = 1, last = 1), sep = ", ")) %>%
  rename_all(~stringr::str_to_title(.)) %>%
  saveRDS(., here::here("data", "swim", "cleaned_cms_top_all_time_2023.Rds"))

  


       
