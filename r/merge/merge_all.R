################################################################################

R.utils::sourceDirectory(here::here("r", "functions"))
################################################################################

################################################################################
# source script for cleaning subsets
################################################################################

source_all = FALSE

if (source_all) {
  source(here::here("r", "gng", "gng_long_clean.R"))
  source(here::here("r", "symmspan", "sspan_long_clean.R"))
  source(here::here("r", "demog", "demog_clean.R"))
}
################################################################################
# read cleaned subsets
################################################################################

# go/no-go task
# Span task
GNG_cleaned <- readRDS(here::here("data", "gng", "gng_long_clean_cayman.Rds")) |>
  mutate(wave = id_wave) |>
  group_by(id_subject, wave, target) |>
  summarize(rt = mean(rt, na.rm = T),
            accuracy = mean(accuracy, na.rm = T),
            .groups = "drop"
  )
GNG_cleaned_by_id <- GNG_cleaned |>
  tidyr::pivot_wider(
    names_from  = c(target),
    values_from = c(rt, accuracy)
  )

# Span task
SPAN_cleaned <- readRDS(here::here("data", "symmspan", "sspan_long_clean.Rds")) |>
  select(id_subject, id_school, wave, totalerrors,
         totalcorrectsquares, totalrecalledsets, sspan) |>
  mutate(WMC = totalcorrectsquares,
         WMCe = totalerrors
  )


# demographic data
DEMOG <- readRDS(here::here("data", "demog", "demog_clean.rds")) |>
  mutate(wave = as.numeric(id_wave),
         age = as.numeric(age),
         ethnicity = ifelse(self_eth == "N/A" | is.na(self_eth),
                            "NA", self_eth
         ),
         #sex = ifelse(sex == "N/A" | is.na(sex), "Not Answered", sex)
  ) |>
  relocate(c(id_subject, id_school, wave, age, ethnicity, sex), .before = 1)

################################################################################
# join
################################################################################
JOINED_LONG <-
  dplyr::left_join(x = DEMOG,
                   y = SPAN_cleaned,
                   by = c("id_subject", "wave"),
                   suffix = c("", ""),
                   relationship = "many-to-many"
  )

# then merge gng
JOINED_LONG <-
  dplyr::left_join(x = JOINED_LONG,
                   y = GNG_cleaned_by_id,
                   by = c("id_subject", "wave"),
                   suffix = c("", ""),
                   relationship = "many-to-many"
                   )
dplyr::left_join(x = JOINED_LONG,
                 y = GNG_cleaned,
                 by = c("id_subject", "wave"),
                 suffix = c("", ""),
                 relationship = "many-to-many"
) |> view_html()
JOINED_LONG |> saveRDS(here::here("data", "merged", "champ_demog_span_gng_long_merged.Rds"))


# Wide data

# pivot wave for go/no-go

# pivot wave for span
SPAN_cleaned_wide <-
  SPAN_cleaned |>
  tidyr::pivot_wider(
    id_cols = id_subject,
    names_from  = wave,
    values_from = c(sspan, totalcorrectsquares, totalerrors, WMC, WMCe)
  )

#SPAN_cleaned_wide |> view_html()

JOINED_WIDE <- full_join(
  x = DEMOG,
  y = SPAN_cleaned_wide,
  by = "id_subject"
)

#JOINED_WIDE |> view_html()

JOINED_WIDE <-
  JOINED_WIDE |>
  mutate(across(.cols = contains("_yn"),
                .fns = ~as.numeric(ifelse(.x == "N/A", NA, .x))
  ))
JOINED_WIDE |> saveRDS(here::here("data", "merged", "champ_demog_span_wide_merged.Rds"))
