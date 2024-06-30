
R.utils::sourceDirectory(here::here("r", "functions"))
library(readxl)

#gonthier_raw <- read_xlsx("C:/Users/gcook/Sync/git/pm/axcpt-pm-shape/pmaxcpt-lindsey/data/raw/Gonthier_Experiment_1_Raw_Data.xlsx")
gonthier_cleaned <- read_xls("C:/Users/gcook/Sync/git/pm/axcpt-pm-shape/pmaxcpt-lindsey/data/raw/Gonthier_Experiment_1_Clean_Data.xls")



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


gonthier_raw_data
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


#gonthier_raw |> view_html()

# Create variables
gonthier_wide <-
  gonthier_raw |>
  pivot_wider(id_cols = c(id, trialnum, block),
              names_from = trial,
              values_from = c(task_acc, rt, corr, errors, trials)
  ) |>
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

gonthier_cleaned |> view_html()
  select(SubjectID,  `BA-dcontext`, `BA-Acuebias`,
       `BA-PBIcomp`, `BA-PBIerr`, `BA-PBIrt`) |> view_html()


hr = G$corr_AX/(G$corr_AX + G$errors_AX)
yhit1p = sqrt((-2)*log(hr))
zhr = (-yhit1p) + ((((0.0000453642210148*yhit1p +
                           0.0204231210245)*yhit1p + 0.342242088547)*yhit1p + 1)*yhit1p +
                        0.322232431088)/((((0.0038560700634*yhit1p +
                                              0.10353775285)*yhit1p + 0.531103462366)*yhit1p +
                                            0.58858170495)*yhit1p + 0.099348462606)

fa  = (G$errors_BX + 5)/(G$corr_BX + G$errors_BX + 1)
yfa1p = sqrt((-2) * log(fa))
zfa = (-yfa1p) +
  ((((0.0000453642210148*yfa1p + 0.0204231210245)*yfa1p + 0.342242088547)*yfa1p + 1)*yfa1p +
0.322232431088)/((((0.0038560700634*yfa1p +0.10353775285)*yfa1p + 0.531103462366)*yfa1p +
0.58858170495)*yfa1p + 0.099348462606)

dp = zhr - zfa
dp
gonthier_cleaned$`BA-dcontext`

gonthier_cleaned |> view_html()
  select(SubjectID, `BA-dcontext`, `BA-Acuebias`,
         `BA-PBIcomp`, `BA-PBIerr`, `BA-PBIrt`,
         ) |> view_html()

gonthier_raw |>
  #filter(block == "B") |>
  pivot_wider(id_cols = c(id, block),
              names_from = trial,
              values_from = c(task_acc, rt)
              ) |>
  mutate(
    task_acc_AX = unlist(task_acc_AX),
    task_acc_BY = unlist(task_acc_BY),
    task_acc_BX = unlist(task_acc_BX),
    task_acc_AY = unlist(task_acc_AY),
    rt_AX = unlist(rt_AX),
    rt_BY = unlist(rt_BY),
    rt_BX = unlist(rt_BX),
    rt_AY = unlist(rt_AY)
  )



gonthier_summary <-
  gonthier_raw |>
  #filter(id == 1) |>
  mutate(
    # trial counts and error counts
    trial_cnt = 1,
    trial_cnt_acc = ifelse(task_acc == 1, 1, 0),  # if correct count trial, else don't
    trial_cnt_err = ifelse(task_acc == 0, 1, 0),  # if erroroneous, count trial, else don't
    #    trial_cnt_w_errors = sum(trial_cnt),         # total count of trials with errors (unsure if you need correct trials only)

    # response times
    rt_acc = ifelse(task_acc == 1, probert, NA),  # correct rt
    rt_err = ifelse(task_acc == 0, probert, NA),  # error rt
   ) |>
  # then by grouping
  group_by(id, block, trial) |>          # for each id, block, trialtype, and accuracy
  summarize(
    # trial counts and error counts
    trial_total = sum(trial_cnt),                  # all trials
    trial_total_acc = sum(trial_cnt_acc),          # all correct trials
    trial_total_w_errors = sum(trial_cnt),         # all trials (same as trial_total)

    # log linear correction
    acc_total = sum(ifelse(task_acc == 1, 1, 0)),  # sum of accurate trials
    err_total = sum(ifelse(task_acc == 0, 1, 0)),  # sum of error trials

    #z = case_when(
    #qnorm = qnor
    #)
    #    hits = sum(ifelse(task_acc == 1, 1, 0)),            # sum of hits
    #    #    fa   = sum(ifelse(task_acc == 0, 1, 0)),       # sum of fa
    #    #    miss =             # sum of miss
    #    #    cr   =             # sum of cr
    #log_linear_corr = (err_total + .5) / (trial_total + 1),    # error correction?
    #log_linear_corr2 = (err_total + .5) / (trial_cnt_acc + 1),

    # response times
    mean_rt_acc  = mean(rt_acc, na.rm = TRUE),          # mean correct rt
    mean_rt_err  = mean(rt_err, na.rm = TRUE),          # mean incorrect rt

    # accuracy
    mean_acc = mean(task_acc, na.rm = TRUE),            # mean accuracy
    mean_err = err_total/sum(err_total, acc_total),     # mean error
    mean_err_ll = (err_total + .5) / (trial_total + 1),    # error correction?
    #log_linear_corr2 = (err_total + .5) / (trial_cnt_acc + 1),

    .groups = "drop"
    ) |>
  #select(-c(trial_total_acc, trial_total_w_errors))
  arrange(id, block, trial)

gonthier_summary_wide <-
  gonthier_summary |>
    pivot_wider(
      id_cols = c(id, block),
      names_from = trial,
      values_from = c(trial_total, trial_total_acc, trial_total_w_errors, acc_total, err_total, mean_rt_acc, mean_rt_err, mean_acc, mean_err)
      )

gonthier_summary_wide |> view_html()

################################################################################
## Add d-prime
gonthier_summary_wide |> view_html()

final <-
  gonthier_summary_wide |>
    mutate(
      # the rts check out with Gonthier
      # mean_err_?? errors check out

      # Proactive Behavioral Index
      # this checks out with Gonthier
      pbi_rt  = (mean_rt_acc_AY - mean_rt_acc_BX) / (mean_rt_acc_AY + mean_rt_acc_BX),

      # this does not check out with Gonthier
      pbi_acc = (mean_acc_AY - mean_acc_BX) / (mean_acc_AY + mean_acc_BX),

      # this does not check out with Gonthier
      pbi_err = (mean_err_AY - mean_err_BX) / (mean_err_AY + mean_err_BX),
      # this does not check out with Gonthier
      pbi_acc_sums = (acc_total_AY - acc_total_BX) / (acc_total_AY + acc_total_BX),  # this right?

      # D-Prime Context
      # this does not check out with Gonthier
      dp_context = (qnorm(mean_acc_AX) - qnorm(mean_err_BX)),
      # dprime(HR = mean_acc_AX, FAR = mean_err_BX),


      #A-Cue Bias
      # this does not check out with Gonthier
      a_cue = (qnorm(mean_acc_AX) - qnorm(mean_err_AY)), #dprime(HR = mean_acc_AX, FAR = mean_err_AY),

      # BX Probe Interference
      # this does not check out with Gonthier
      bxpi_rt  = (mean_rt_acc_BX - mean_rt_acc_BY),
      # this does not check out with Gonthier
      bxpi_acc = (mean_acc_BX - mean_acc_BY)
    ) #|>
    #filter(block == "Proactive") |>
    #select(c(id, block, mean_acc_AX:mean_rt_acc_BY, mean pbi_rt:bxpi_acc)) |>
  #filter(id == 1) |> view_html()

final
long <-
  final |>
  pivot_wider(#data = df,
              names_from = c("mean_rt_acc", "mean_rt_err", "pbi_rt", "bxpi_rt"),
              names_sep = "_",
              values_from = c(mean_rt_acc_AX, mean_rt_acc_AY, mean_rt_acc_BX, mean_rt_acc_BY,
                              mean_rt_err_AX, mean_rt_err_AY, mean_rt_err_BX, mean_rt_err_BY,
                              pbi_rt, bxpi_rt
                              )
              )

final2 <-
  final |> select(contains("rt")) |>
  rename_with( ~gsub("mean_", "", .x)) |>
  select(contains("acc"))

col_names <- sub(".*_(..)$", "\\1", names(final2))

col_names

# Pivot wider based on the last two characters of each column name
pivot_wider(data = final2,
                          names_from = col_names,
                          values_from = everything())


gonthier_cleaned |> view_html()
#gonthier_cleaned |> select(id, block, trial, pbi_rt:bxpi_acc) |> view_html()


# https://neuropsychology.github.io/psycho.R/2018/03/29/SDT.html

final
hr = .87; fa = .05
(qnorm(hr) - qnorm(fa) )
qnorm(mean = .7, sd = .05)
?qnorm

library(psycho)

# Let's simulate three participants with different results at a perceptual detection task
df <- data.frame(Participant = c("A", "B", "C"),
                 n_hit = c(1, 2, 5),
                 n_fa = c(1, 3, 5),
                 n_miss = c(6, 8, 1),
                 n_cr = c(4, 8, 9)
                 )
indices <- psycho::dprime(df$n_hit, df$n_fa, df$n_miss, df$n_cr)
psycho::dprime

hit_rate_adjusted <- n_hit/n_targets
fa_rate_adjusted <- n_fa/n_distractors
}
else {
  hit_rate_adjusted <- (n_hit + 0.5)/(n_hit + n_miss +
                                        1)
  fa_rate_adjusted <- (n_fa + 0.5)/(n_fa + n_cr + 1)
}

dprime <- (qnorm(hr) - qnorm(fa))
zhr <- qnorm(hr)
zfar <- qnorm(fa)
beta <- exp(-zhr * zhr/2 + zfar * zfar/2)
c <- -(qnorm(hr) + qnorm(fa))/2
beta

#  hit_rate <- n_hit/n_targets
#  fa_rate <- n_fa/n_distractors
#  dprime <- qnorm(hit_rate) - qnorm(fa_rate)
#  zhr <- qnorm(hit_rate)
#  zfar <- qnorm(fa_rate)
#  beta <- exp(-zhr * zhr/2 + zfar * zfar/2)
#  c <- -(qnorm(hit_rate) + qnorm(fa_rate))/2


indices
df <- cbind(df, indices)


NEW FILE.
TITLE  Contextual variables and their effect on R-K Judgments.
Set width =80.
Data List File='C:\spssdat\rkrep1.raw' FREE/
  subid * counter * Wordid * Oldnew * Trialtyp * Loc *  Resp *  rt.
execute.

/* Oldnew Old '1' New '2'.
/* TrialTyp 	1X1L = '1' 	3X1L = '2'  	3X3L = '3'  	New = '4'.
/* Resp 		Remember '1'		Know '2'		New '3'.

IF any(resp,1,2) and (oldnew eq 1) hit=1.
IF any(resp,1,2) and (oldnew eq 2) fa=1.
execute.

AGGREGATE OutFile=*
  /Break= SubID
/hit=Sum(hit)
/fa=Sum(fa).


compute hitp = hit/108.
compute fap = fa/108.
execute.

Compute totold = 108.
Compute totnew = 108.

/*The following are operations for computing Signal Detection measures
/*of a Yes/No task (e.g., recognition memory) for statistical analysis.

/*Needed are:  the number of hits, the number of false alarms, the
/*TOTAL number of OLD items on the test, and the TOTAL number of NEW
/*items at test.

/*Signal detection analyses start here.  the c means corrected
/*which RLM says is in Snodgrass & Corwin
Compute hitpc = (hit + .5)/Totold.
Compute fapc = (fa + .5)/Totnew.

Compute yhitpc = sqrt ((-2)*ln(hitpc)).
Compute yfapc = sqrt ((-2)*ln(fapc)).

Compute zhitp = (-yhitpc) + ((((0.0000453642210148*yhitpc +
                                  0.0204231210245)*yhitpc + 0.342242088547)*yhitpc + 1)*yhitpc +
                               0.322232431088)/((((0.0038560700634*yhitpc +
                                                     0.10353775285)*yhitpc + 0.531103462366)*yhitpc +
                                                   0.58858170495)*yhitpc + 0.099348462606).

Compute zfap = (-yfapc) + ((((0.0000453642210148*yfapc +
                                0.0204231210245)*yfapc + 0.342242088547)*yfapc + 1)*yfapc +
                             0.322232431088)/((((0.0038560700634*yfapc +
                                                   0.10353775285)*yfapc + 0.531103462366)*yfapc +
                                                 0.58858170495)*yfapc + 0.099348462606).

/* parametric discrim/one high T model (rlm)
Compute dprime = zhitp-zfap.
Compute da = (1.104315261*(zhitp - (0.8*zfap))).  /* unequal var assump = 0.80 RLM
Compute C = (-0.5)*(zhitp + zfap).

/* Nonparametric A' and B''d (rlm)
Compute Aprime = .5 + ((hitp - fap)*(1 + hitp - fap))/(4*hitp*(1 - fap)).
Do if hitp lt fap.
Compute Aprime = .5 + ((fap - hitp)*(1 + fap - hitp))/(4*fap*(1 - hitp)).
End if.
Compute bdonald = (((1 - hitpc)*(1 - fapc) - hitpc*fapc)/((1 - hitpc)*(1 -
fapc) + hitpc*fapc)).

/* Two high model (rlm)
Compute Pr = (hitpc - fapc).    /* to be consis w/ 1HT, not hitp-fap
execute.                        /* this needs to be here!
Compute Br = ((fapc)/(1-Pr)).
execute.

MEANS TABLES=dprime c aprime bdonald Pr Br /* BY cond
/CELLS MEAN COUNT STDDEV SEMEAN .

MEANS TABLES=hitp fap hitpc fapc /* BY cond
/CELLS MEAN COUNT STDDEV SEMEAN .
