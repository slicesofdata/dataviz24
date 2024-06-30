# axcpt measures
#

R.utils::sourceDirectory(here::here("r", "functions"))
library(readxl)

#gonthier_raw <- read_xlsx("C:/Users/gcook/Sync/git/pm/axcpt-pm-shape/pmaxcpt-lindsey/data/raw/Gonthier_Experiment_1_Raw_Data.xlsx")
gonthier_cleaned <- read_xls("C:/Users/gcook/Sync/git/pm/axcpt-pm-shape/pmaxcpt-lindsey/data/raw/Gonthier_Experiment_1_Clean_Data.xls")

AX <- readRDS(here::here("data", "axcpt", "cleaned_axcpt_pm_shape.Rds"))
AX <- AX |> select(subid, block, trialNum, trial, task_acc, probe_rt)
AX |> view_html()

################################################################################
# Functions
#################################################################################
axcpt_set_vars <- function(
    # need to make flexible for missing arguments
  data,
  id,
  block,
  trialnum,
  trialtype,
  accuracy,
  rt
  ) {
    # set grouping structure
    if (missingArg(block)) {
      groups_ = c("id", "trialtype")          # omit block
      data$block = "none"
    } else {
      groups_ = c("id", "block", "trialtype") # include block
    }
    # add trialnum if not present
    if (missingArg(trialnum)) {
      # create it for grouping modify
      #data$trialnum = NA
      data = data |>
        group_by(across(any_of(groups_))) |>
        mutate(trialnum = 1:n()) |> ungroup()
    } else {
      data = data |> mutate(trialnum  = {{trialnum}} )
    }
    # create variables for remaining functions
    data =
      data |>
      mutate(id        = {{id}},
             block     = {{block}},
             trialtype = {{trialtype}},
             accuracy  = {{accuracy}},
             rt        = {{rt}}
      )
    data =
      data |>
      group_by(across(all_of(groups_))) |>
      mutate(#trialcount =
      # rt NA is incorrect
      rt    = ifelse(accuracy == 1, rt, NA),
      # correct = 1
      corr  = ifelse(accuracy == 1, 1, 0),
      # if is error
      error = ifelse(accuracy == 0, 1, 0),
      #
      corr_total  = sum(accuracy, na.rm = TRUE),
      trial_total = n()
      ) |>
      ungroup() |>
      select(any_of(c("id", "block", "trialnum", "trialtype", "accuracy",
                      "rt", "corr", "error", "corr_total", "trial_total"))
      ) |>
      arrange(across(any_of(c("id", "block", "trialtype"))))

    return(data)
}


axcpt_pivot <- function(
    data

    # add for z scores and trimming?
    ) {
  # set grouping structure
#  if(missingArg(block)) {
#    groups_ = c("id", "trialtype")          # omit block
    #    id_cols_ = c("id", "trialnum")
#  } else {
#    groups_ = c("id", "block", "trialtype") # include block
    #   id_cols_ = c("id", "trialnum", "block")
#  }
  # arrange data frame and pivot by trialtype and block
#  message("hi")
  data =
    data |>
    #group_by()
    mutate(trials = 1) |>
    # group and standardize
    #group_by(across(any_of(c("id", "block", "trialtype")))) |>
    #mutate(rt_z = as.numeric( scale(accuracy)) ) |>
    #ungroup() |>
    pivot_wider(
      id_cols = any_of(c("id", "trialnum", "block")),
      names_from = trialtype,
      # old      values_from = c(task_acc, rt, corr, errors, trials)
      values_from = any_of(c("accuracy", "rt", "corr", "error", "rt_z")) #trials was the counter
    ) |>
    group_by(across(any_of(c("id", "block")))) |>
    summarise(across(.cols = c(matches("rt_|accuracy_")),
                     .fns = ~mean(.x, na.rm = TRUE)
                     ),
              across(.cols = c(matches("corr_|error_")),
                     .fns = ~sum(.x, na.rm = TRUE) #.names = "{.col}"
                     ),

              #across(.cols = c(matches("trials_")),
              #.fns = ~sum(.x, na.rm = TRUE)
              #),
              .groups = "drop"
              ) |>
    rename_with(.fn = ~gsub("task_acc", "hr", .x))
    #arrange(across(any_of(c("id", "trialnum", "block", "trialtype"))))

  return(data)
}

axcpt_add_measures <- function(
    data

    # add for z scores and trimming?
    ) {
  data =
    data |>
    #arrange(across(any_of(c("id", "block", "trialtype")))) |>
    mutate(
      # corrected hit rates
      hr_corr_AX = (corr_AX + .5) / (corr_AX + error_AX + 1), #used
      hr_corr_AY = (corr_AY + .5) / (corr_AY + error_AY + 1),
      hr_corr_BX = (corr_BX + .5) / (corr_BX + error_BX + 1),
      hr_corr_BY = (corr_BY + .5) / (corr_BY + error_BY + 1),

      # uncorrected error rates
      err_AX = error_AX / (corr_AX + error_AX),
      err_AY = error_AY / (corr_AY + error_AY),
      err_BX = error_BX / (corr_BX + error_BX),
      err_BY = error_BY / (corr_BY + error_BY),

      #err_corr_AX = (error_AX + .5) / (trials_AX + 1),
      #err_corr_AY = (error_AY + .5) / (trials_AY + 1),
      #err_corr_BX = (error_BX + .5) / (trials_BX + 1),
      #err_corr_BY = (error_BY + .5) / (trials_BY + 1),

      # uncorrected error rates
      err_corr_AX = (error_AX + .5) / (corr_AX + error_AX + 1),
      err_corr_AY = (error_AY + .5) / (corr_AY + error_AY + 1), # used
      err_corr_BX = (error_BX + .5) / (corr_BX + error_BX + 1), # used
      err_corr_BY = (error_BY + .5) / (corr_BY + error_BY + 1),

      #fa_AX = errors_AX / (corr_AX + errors_AX),
      #fa_AY = errors_AY / (corr_AY + errors_AY),
      #fa_BX = errors_BX / (corr_BX + errors_BX),
      #fa_BY = errors_BY / (corr_BY + errors_BY),

      # the rts check out with Gonthier
      # mean_err_?? errors check out
      ############################################################################
      # D-Prime Context
      # this checks out with Gonthier
      dp_context  = (qnorm(hr_corr_AX) - qnorm(err_corr_BX)),
      ############################################################################

      ############################################################################
      # A-Cue Bias: Signal detection bias for AX and AY trial types. The A-cue bias
      # measure indicates the general tendency to make a target response following
      # an A-cue (which would lead to a high AX target hit rate, but also a high AY
      # false alarm rate).  Positive values indicate a positive bias, and are
      # predicted to increase in proactive conditions in the DMC framework.
      # An  A-cue  bias  measure  was  also  calculated (Richmond et al., 2015) by
      # computing a c criterion from hits on AX trials and false alarms on AY trials
      # as 1/2*(Z[H]+Z[F]), with H representing hits on AX trials and F representing
      # false alarms on AY trials.

      # If the A-cue bias is close to 0: It suggests that the participant's responses to
      # A cues and non-A cues are relatively balanced, indicating a neutral or unbiased
      # response pattern. If the A-cue bias is positive: It suggests a bias towards
      # responding to A cues more frequently than non-A cues. This could indicate a
      # tendency to be more sensitive or more likely to respond when presented with
      # A cues. If the A-cue bias is negative: It suggests a bias against responding to
      # A cues compared to non-A cues. This could indicate a tendency to be less
      # sensitive or less likely to respond when presented with A cues.

      #  this does NOT check out with Gonthier
      a_cue = (0.5 * (qnorm(hr_corr_AX) + qnorm(err_corr_AY))),
      ############################################################################

      ############################################################################
      # Proactive Behavioral Index
      pbi_err = (err_corr_AY - err_corr_BX) / (err_corr_AY + err_corr_BX),
      pbi_rt  = (rt_AY - rt_BX) / (rt_AY + rt_BX),
      #pbi_comp = ?
      ############################################################################

      ############################################################################
      # BX Probe Interference
      # this does not check out with Gonthier
      bxpi_rt  = (rt_BX - rt_BY),
      # this does not check out with Gonthier
      bxpi_acc = (corr_BX - corr_BY),
      #bxpi_acc2 = (hr_BX - hr_BY),
      #bxpi_acc3 = (hr_corr_BX - hr_corr_BY),
  ############################################################################
  ) |>
    mutate(across(rt_AX:rt_BY, round, digits = 2))

  return(data)
}

axcpt_pivot_measures <- function(data) {
  if ("block" %in% names(data)) {
    data =
      data |>
      pivot_wider(names_from = block,
                  values_from = !c(id, block))
  }
  return(data)
}



################################################################################
# End Functions
################################################################################

################################################################################
# Testing
################################################################################
AX <- readRDS(here::here("data", "axcpt", "cleaned_axcpt_pm_shape.Rds")) |>
  select(subid, block, trialNum, trial, task_acc, probe_rt)

AX |> view_html()

AX_set_vars <-
  AX |>
  axcpt_set_vars(id = subid, block = block, trialnum = trialNum,
                 trialtype = trial, accuracy = task_acc, rt = probe_rt
                 )

AX_set_vars |> view_html()

AX_set_vars |>
  axcpt_pivot() |>
  view_html() #id = subid, block = trialtype = trial, accuracy = task_acc, rt = probe_rt)

AX_set_vars |>
  axcpt_pivot() |>
  axcpt_add_measures()

AX |>
  axcpt_set_vars(id = subid, block = block, trialnum = trialNum,
                 trialtype = trial, accuracy = task_acc, rt = probe_rt
  ) |>
  axcpt_pivot() |>
  axcpt_add_measures() |>
  filter(id != 2) |>
  axcpt_pivot_measures() |>
  view_html()



gonthier_raw <- read_xlsx("C:/Users/gcook/Sync/git/pm/axcpt-pm-shape/pmaxcpt-lindsey/data/raw/Gonthier_Experiment_1_Raw_Data.xlsx")
names(gonthier_raw)

gonthier_raw |>
  axcpt_set_vars(id = Subject, block = ExperimentName, #trialnum = trialNum,
                 trialtype = TrialType, accuracy = ProbeComb.Acc, rt = ProbeComb.RT
  ) |>
  axcpt_pivot() |>
  axcpt_add_measures() |>
  filter(id != 2) |>
  view_html()

gonthier_cleaned <- read_xls("C:/Users/gcook/Sync/git/pm/axcpt-pm-shape/pmaxcpt-lindsey/data/raw/Gonthier_Experiment_1_Clean_Data.xls")
gonthier_cleaned |> view_html()

names(gonthier_cleaned)

################################################################################
# End Testing
################################################################################
################################################################################
################################################################################
axcpt_prep <- function(
       data,
       id,
       block,
       trialnum,
       trialtype,
       accuracy,
       rt
       ) {
  # set grouping structure
  if (missing(block)) {
    groups_ = c("id", "trialtype")          # omit block
  } else {
    groups_ = c("id", "block", "trialtype") # include block
  }
  # add trialnum if not present
  if (missing(trialnum)) {
    # create it for grouping modify
    #data$trialnum = NA
    data =
      data |>
      group_by(across(any_of(groups_))) |>
      mutate(trialnum = 1:n()) |> ungroup()
  } else {
    data = data |> mutate(trialnum  = {{trialnum}} )
  }
   # create variables for remaining functions
  data =
       data |>
       mutate(id        = {{id}},
              block     = {{block}},
              trialtype = {{trialtype}},
              accuracy  = {{accuracy}},
              rt        = {{rt}}
              ) |>
      # group_by(across(all_of(c("subid", "block", "trial")))) |>
    group_by(across(all_of(groups_))) |>
      mutate(#trialcount =
             # rt NA is incorrect
             rt    = ifelse(accuracy == 1, rt, NA),
             # correct = 1
             corr  = ifelse(accuracy == 1, 1, NA),
             # if is error
             error = ifelse(accuracy == 0, 1, NA),
             #
             corr_total  = sum(accuracy, na.rm = TRUE ),
             trial_total = n()
             ) |>
       ungroup() |>
       # keep any of these variables (will keep block if included)
       select(any_of(c("id", "block", "trialnum", "trialtype", "accuracy",
                       "rt", "corr", "error", "corr_total", "trial_total"))
              )
  return(data)
}

AX_prepped <-
  AX |>
   axcpt_prep(
       id = subid,
       block = block,
       trialtype = trial,
       trialnum = trialNum,
       accuracy = task_acc,
       rt = probe_rt
       )

AX_prepped |> view_html()
############

ax_pivot <- function(
    data,
    id,
    block,
    trialtype,
    accuracy,
    rt
    ) {
  # set grouping structure
  if(missing(block)) {
    groups_ = c("id", "trialtype")          # omit block
#    id_cols_ = c("id", "trialnum")
  } else {
    groups_ = c("id", "block", "trialtype") # include block
 #   id_cols_ = c("id", "trialnum", "block")
  }
  data =
    data |>
    #group_by()
    mutate(trials = 1) |>
    arrange(across(any_of(c("id", "block", "trialtype")))) |>
    pivot_wider(
      id_cols = any_of(c("id", "trialnum", "block")),
      names_from = trialtype,
      # old      values_from = c(task_acc, rt, corr, errors, trials)
      values_from = c(accuracy, rt, corr, error) #trials was the counter
      ) |>
    group_by(across(any_of(c("id", "block")))) |>
    summarise(across(.cols = c(matches("rt_|accuracy_")),
                     .fns = ~mean(.x, na.rm = TRUE)
    ),
    across(.cols = c(matches("corr_|error_")),
           .fns = ~sum(.x, na.rm = TRUE) #.names = "{.col}"
    ),
    #trials_AX = sum(tr)
    across(.cols = c(matches("trials_")),
           .fns = ~sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
    ) |>
    rename_with(.fn = ~gsub("task_acc", "hr", .x))
  return(data)
}

#AX_prepped |> view_html()
AX_prepped |>
  ax_pivot() |>
  view_html() #id = subid, block = trialtype = trial, accuracy = task_acc, rt = probe_rt)


############################################################################################################################
############################################################################################################################
############################################################################################################################

# This computes the signal detection bias measure selectively for AX and AY trial types.
# The A-cue bias measure indicates the general tendency to make a target response following an A-cue
# (which would lead to a high AX target hit rate, but also a high AY false alarm rate).
# Positive values indicate a positive bias, and are predicted to increase in proactive conditions in the DMC framework.
#

# BX Probe Interference
# This computes the magnitude of behavioral interference measured as BX-BY performance. It is computed on both
# RT and accuracy. The DMC framework predicts a reduction in the accuracy interference effect in both Proactive
# and Reactive conditions relative to Baseline. In contract, the RT interference effect is predicted to selectively
# increase in the Reactive condition.
#

# A-Cue Bias
# This computes the signal detection bias measure selectively for AX and AY trial types.
# The A-cue bias measure indicates the general tendency to make a target response following an A-cue
# (which would lead to a high AX target hit rate, but also a high AY false alarm rate).
# Positive values indicate a positive bias, and are predicted to increase in proactive conditions in the DMC framework.

# BX Probe Interference
# This computes the magnitude of behavioral interference measured as BX-BY performance. It is computed on both
# RT and accuracy. The DMC framework predicts a reduction in the accuracy interference effect in both
# Proactive and Reactive conditions relative to Baseline. In contract, the RT interference effect is predicted to
# selectively increase in the Reactive condition.

# D-Prime Context
# This computes the signal detection dprime measure selectively for AX and BX trial types (i.e., AX hits vs. BX false alarms).
# The d-prime context measure indicates the ability to discriminate target and nontargets based on the cue.  It is a measure
# frequently computed in the AX-CPT and so is included for comparison purposes, but there is not a strong prediction from the
# DMC framework.

# Proactive Behavioral Index
# This computes a normalized difference score of performance distinctions between AY and BX trial types (i.e., [AY-BX]/[AY+BX]).
# It can be computed on both RT and accuracy measures as well as their sum.  This measure has often been computed in the AX-CPT
# (Braver et al, 2009).  The index measure is predicted to increase in the Proactive condition.  However, it is not clear yet
# whether there is a prediction to be made for the Reactive condition.

############################################################################################################################
############################################################################################################################
############################################################################################################################

gonthier_raw_data <-
  read_xlsx("C:/Users/gcook/Sync/git/pm/axcpt-pm-shape/pmaxcpt-lindsey/data/raw/Gonthier_Experiment_1_Raw_Data.xlsx") |>
  mutate(id = Subject,
         block = substring(ExperimentName, 1, 1),
         task_acc = ProbeComb.Acc,
         probert = ProbeComb.RT,
         trial = TrialType,
         rt = ifelse(task_acc == 1, ProbeComb.RT, NA),
         corr = ifelse(task_acc == 1, 1, NA),
         errors = ifelse(task_acc == 0, 1, NA)
  ) # select(Subject, ExperimentName, rt)

gonthier_raw_data |>
  select(id, ExperimentName, trial, task_acc, rt)


gonthier_raw_data |> view_html()

gonthier_raw  <-
  gonthier_raw_data |>
  mutate(id = Subject,
         #block = ExperimentName,
         block = substring(ExperimentName, 1, 1),
         task_acc = ProbeComb.Acc,
         probert = ProbeComb.RT,
         trial = TrialType,
         rt = ifelse(task_acc == 1, ProbeComb.RT, NA),
         corr = ifelse(task_acc == 1, 1, NA),
         errors = ifelse(task_acc == 0, 1, NA)
  ) |>
  group_by(id, block) |>
  mutate(trialnum = 1:n()) |>
  group_by(id, block, trial) |>
  mutate(trials = 1) |> ungroup() |>
  relocate(trialnum, .after = block) |>
  arrange(id, block, trialnum, trial) |>
  select(id, block, trialnum, trial, task_acc, rt, corr, errors, trials)

function1 <- function(
    data,
    accuracy,
    rt,
    trial
) {

}

gonthier_raw |> view_html()



# Create variables
gonthier_wide <-
  gonthier_raw |>
  pivot_wider(id_cols = c(id, trialnum, block),
              names_from = trial,
              values_from = c(task_acc, rt, corr, errors, trials)
  ) |> #view_html()
  group_by(id, block) |>
  summarise(across(.cols = c(matches("rt_|acc_")),
                   .fns = ~mean(.x, na.rm = TRUE)
  ),
  across(.cols = c(matches("corr_|errors_")),
         .fns = ~sum(.x, na.rm = TRUE) #.names = "{.col}"
  ),
  #trials_AX = sum(tr)
  across(.cols = c(matches("trials_")),
         .fns = ~sum(.x, na.rm = TRUE)
  ),
  .groups = "drop"
  ) |>
  rename_with(.fn = ~gsub("task_acc", "hr", .x))

gonthier_wide |> view_html()


gonthier_wide_measures <-
  gonthier_wide |>
  mutate(
    # corrected hit rates
    hr_corr_AX = (corr_AX + .5) / (corr_AX + errors_AX + 1), #used
    hr_corr_AY = (corr_AY + .5) / (corr_AY + errors_AY + 1),
    hr_corr_BX = (corr_BX + .5) / (corr_BX + errors_BX + 1),
    hr_corr_BY = (corr_BY + .5) / (corr_BY + errors_BY + 1),

    # uncorrected error rates
    err_AX = errors_AX / (corr_AX + errors_AX),
    err_AY = errors_AY / (corr_AY + errors_AY),
    err_BX = errors_BX / (corr_BX + errors_BX),
    err_BY = errors_BY / (corr_BY + errors_BY),


    #err_corr_AX = (errors_AX + .5) / (trials_AX + 1),
    #err_corr_AY = (errors_AY + .5) / (trials_AY + 1),
    #err_corr_BX = (errors_BX + .5) / (trials_BX + 1),
    #err_corr_BY = (errors_BY + .5) / (trials_BY + 1),

    # uncorrected error rates
    err_corr_AX = (errors_AX + .5) / (corr_AX + errors_AX + 1),
    err_corr_AY = (errors_AY + .5) / (corr_AY + errors_AY + 1), # used
    err_corr_BX = (errors_BX + .5) / (corr_BX + errors_BX + 1), # used
    err_corr_BY = (errors_BY + .5) / (corr_BY + errors_BY + 1),

    #fa_AX = errors_AX / (corr_AX + errors_AX),
    #fa_AY = errors_AY / (corr_AY + errors_AY),
    #fa_BX = errors_BX / (corr_BX + errors_BX),
    #fa_BY = errors_BY / (corr_BY + errors_BY),

    # the rts check out with Gonthier
    # mean_err_?? errors check out
    ############################################################################
    # D-Prime Context
    # this checks out with Gonthier
    dp_context  = (qnorm(hr_corr_AX) - qnorm(err_corr_BX)),
    ############################################################################

    ############################################################################
    # A-Cue Bias: Signal detection bias for AX and AY trial types. The A-cue bias
    # measure indicates the general tendency to make a target response following
    # an A-cue (which would lead to a high AX target hit rate, but also a high AY
    # false alarm rate).  Positive values indicate a positive bias, and are
    # predicted to increase in proactive conditions in the DMC framework.
    # An  A-cue  bias  measure  was  also  calculated (Richmond et al., 2015) by
    # computing a c criterion from hits on AX trials and false alarms on AY trials
    # as 1/2*(Z[H]+Z[F]), with H representing hits on AX trials and F representing
    # false alarms on AY trials.

    #If the A-cue bias is close to 0: It suggests that the participant's responses to A cues and non-A cues are relatively balanced, indicating a neutral or unbiased response pattern.
    #If the A-cue bias is positive: It suggests a bias towards responding to A cues more frequently than non-A cues. This could indicate a tendency to be more sensitive or more likely to respond when presented with A cues.
    #If the A-cue bias is negative: It suggests a bias against responding to A cues compared to non-A cues. This could indicate a tendency to be less sensitive or less likely to respond when presented with A cues.

    #  this does NOT check out with Gonthier
    a_cue = (0.5 * (qnorm(hr_corr_AX) + qnorm(err_corr_AY))),
    ############################################################################

    ############################################################################
    # Proactive Behavioral Index
    pbi_err = (err_corr_AY - err_corr_BX) / (err_corr_AY + err_corr_BX),
    pbi_rt  = (rt_AY - rt_BX) / (rt_AY + rt_BX),
    #pbi_comp = ?
    ############################################################################

    ############################################################################
    # BX Probe Interference
    # this does not check out with Gonthier
    bxpi_rt  = (rt_BX - rt_BY),
    # this does not check out with Gonthier
    bxpi_acc = (corr_BX - corr_BY),
    bxpi_acc2 = (hr_BX - hr_BY),
    bxpi_acc3 = (hr_corr_BX - hr_corr_BY),
    ############################################################################
  ) |>
  mutate(across(rt_AX:rt_BY, round, digits = 2))
#summarise(., across( c(), ~ mean(.x, na.rm = TRUE) ))

gonthier_wide_measures |> #select(id, dp_context:bxpi_acc) |>
  select(c(block, id,
           hr_corr_AX:hr_corr_BY
           err_AX:err_BY,
           rt_AX:rt_BY, #errors_AX:errors_BY,
           dp_context, a_cue, pbi_err, pbi_rt
  )
  ) |>
  filter(id != 2) |>
  view_html()
