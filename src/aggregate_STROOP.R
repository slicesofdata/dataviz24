# get all Stroop
#library(magrittr)
#library(dplyr)

STROOP <- read.csv(here::here("data", "stroop", "gcds_stroop_project_data.csv"))

STROOP <- STROOP %>%
  filter(., !is.na(id))

# compute vars
STROOP_error_agg <- STROOP %>%
  filter(., block == "stroop") %>%
  mutate(., trialtype = case_when(
    word == "blue"   & font == "blue" ~ "congruent",
    word == "blue"   & font != "blue" ~ "incongruent",
    word == "green"  & font == "green" ~ "congruent",
    word == "green"  & font != "green" ~ "incongruent",
    word == "orange" & font == "orange" ~ "congruent",
    word == "orange" & font != "orange" ~ "incongruent",
    word == "yellow" & font == "yellow" ~ "congruent",
    word == "yellow" & font != "yellow" ~ "incongruent")
  ) %>%
  mutate(., congruent = # for models
           ifelse(trialtype == "congruent", 0, 1)) %>%
  mutate(.,
         error = case_when(
           font == "blue"   & resp == "d" ~ 0,
           font == "blue"   & resp != "d" ~ 1,
           font == "green"  & resp == "f" ~ 0,
           font == "green"  & resp != "f" ~ 1,
           font == "orange" & resp == "j" ~ 0,
           font == "orange" & resp != "j" ~ 1,
           font == "yellow" & resp == "k" ~ 0,
           font == "yellow" & resp != "k" ~ 1)
  ) %>%
  select(., id, trialtype, error) %>%
  group_by(., id, trialtype) %>%
  summarize(., error = mean(error))

# Clean up the RT data
#names(STROOP)

# Clean up other vars and compute new
STROOP <- STROOP %>%
  select(., id, block, word, font, resp, rt) %>%
  filter(., block == "stroop") %>%
  mutate(.,
         resp = case_when(
           resp == "d" ~ "blue",
           resp == "f" ~ "green",
           resp == "j" ~ "orange",
           resp == "k" ~ "yellow",
         )
  ) %>%
  mutate(.,
         error = ifelse(font == resp, 0, 1),
         odd   = ifelse(id %% 2, 1, 0),
         trialtype = case_when(
           word == font ~ "congruent",
           word != font ~ "incongruent"),
  ) %>%
  mutate(.,
         incongruent = ifelse(trialtype == "congruent", 0, 1),
         incongruent_fact = factor(incongruent),
         congruent_rt     = ifelse(trialtype == "congruent", rt, NA),
         incongruent_rt   = ifelse(trialtype == "incongruent", rt, NA)
  ) #%>%
  #select(., -c(block, word, font))

# clean up Rts
STROOP <- STROOP %>%
  #mutate(., rt = ifelse(rt > 8000, NA, rt)) %>% # remove too long
  #select(., c(id, block, trial, word, font, resp, rt, cnt)) %>%
  #mutate(., trialtype = trial) %>%
  group_by(., id, trialtype) %>%
  mutate(., rtz = scale(rt)) %>%               # standardize for trimming
  mutate(., rt = ifelse(abs(rtz) < 3, rt, NA)) # trim extremes

#view(STROOP)

STROOP_agg <- STROOP %>%
  group_by(id, odd, trialtype) %>%
  summarize(., rt = mean(rt, na.rm = T)) %>%
  mutate(., incongruent = ifelse(trialtype == "congruent", 0, 1))

STROOP_agg <- merge(STROOP_agg, STROOP_error_agg)

# filter by class
#STROOP_agg
STROOP %>%
  select(., c(id, trialtype, rt, error, incongruent)) %>%
  write.csv(., here::here("data", "stroop", "STROOP.csv"), row.names = F)

write.csv(STROOP_agg, here::here("data", "stroop", "STROOP_agg.csv"), row.names = F)


STROOP_agg %>%
  ggplot() +
  geom_boxplot(aes(trialtype, rt)) +
  geom_jitter(aes(trialtype, rt, color = trialtype), width = .1) +
  see::theme_modern() +
  #ggtitle("Stroop Interference") +
  ylab("Response Time (ms)") + xlab("Trial Type")

# create a nested data frame
id_data <- STROOP %>%
  select(., id, trialtype, rt, error) %>%
  group_by(id) %>%
  nest()

#fit <- lm(rt ~ trialtype + error, data = m[['data']][[1]])

# obtain t value from model
lm_t <- function(df) {
  fit = lm(rt ~ trialtype, data = df)
  t = summary(fit)$coefficients[2,"t value"]
  return(t)
}
# obtain standardized beta for each id
b1_beta <- function(df) {
  fit = lm(rt ~ trialtype, data = df)
  fit_beta = lm.beta::lm.beta(fit)
  slope = as.numeric(coef(fit_beta)[2])
  return(slope)
}
#
b1_beta_mult <- function(df) {
  fit = lm(rt ~ trialtype + error, data = df)
  fit_beta = lm.beta::lm.beta(fit)
  slope = as.numeric(coef(fit_beta)[2]) # second term
  return(slope)
}
# obtain adj r2 for each id
r2_beta <- function(df) {
  fit = lm(rt ~ trialtype, data = df)
  fit_beta = lm.beta::lm.beta(fit)
  r2 = as.numeric(performance::r2(fit_beta)$R2)
  return(r2)
}
# obtain adj r2 for each id
r2_beta_mult <- function(df) {
  fit = lm(rt ~ trialtype + error, data = df)
  fit_beta = lm.beta::lm.beta(fit)
  r2 = as.numeric(performance::r2(fit_beta)$R2)
  return(r2)
}
# obtain adj r2 for each id
r2adj_beta <- function(df) {
  fit = lm(rt ~ trialtype, data = df)
  fit_beta = lm.beta::lm.beta(fit)
  r2_adj = as.numeric(performance::r2(fit_beta)$R2_adjusted)
  return(r2_adj)
}
# mult
r2adj_beta_mult <- function(df) {
  fit = lm(rt ~ trialtype + error, data = df)
  fit_beta = lm.beta::lm.beta(fit)
  r2_adj = as.numeric(performance::r2(fit_beta)$R2_adjusted)
  return(r2_adj)
}

id_data <- id_data %>%
  mutate(., b1 = unlist(purrr::map(data, b1_beta_mult))) %>%
  mutate(., t  = unlist(purrr::map(data, lm_t))) %>%
  mutate(., r2 = unlist(purrr::map(data, r2_beta_mult))) %>%
  mutate(., r2_adj = unlist(purrr::map(data, r2adj_beta_mult))) %>%
  select(., id, b1, t, r2, r2_adj) %>%
  arrange(., b1, t, r2_adj)

#view_html(id_data)


id_data %>%
  filter(., id > 9999) %>%
  mutate(, id = as.character(id)) %>%
  ggplot(data = ., aes(x = reorder(id, t), y = t)) +
  geom_col() +
  see::theme_modern() +
  ylab("Size") + xlab("ID") + coord_flip()

#stat = "identity") +


ggplot(id_data) +
  geom_bar(aes(x = id, y = t),
           stat = "identity") +
  coord_flip()

  scale_fill_manual(
    name = "Mileage",
    labels = c("Above Average", "Below Average"),
    values = c("above" = "#00ba38", "below" = "#f8766d")
  ) +
  ylab("Standardized mpg") +
  xlab("Make and Model") +
  theme_classic()

id_data %>%
  filter(., id > 9999) %>%
  ggplot(data = .) +
  geom_bar(aes(x = t)) #+
  coord_flip() #+
geom_pointrange(stat = "summary") +
  geom_linerange(stat = "summary") +

STROOP %>%
  filter(., id > 9999) %>%
  ggplot(data = ., aes(trialtype, rt)) +

  geom_bar(stat = "identity") +
  coord_flip() #+
  geom_pointrange(stat = "summary") +
  geom_linerange(stat = "summary") +
 # geom_pointrange() +
  see::theme_modern() + facet_wrap(~id)


