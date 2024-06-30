# computes the axcpt measures
################################################################################
# load the libraries
pacman::p_load(tidyverse, correlation, GGally)

################################################################################
# Functions
################################################################################
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


axcpt_standardize <- function(data,
                               group_by_block = TRUE,
                               #group_by = block,
                               cols = c(rt_AX, rt_AX, rt_BX, rt_BY,
                                        dp_context, a_cue, pbi_err, pbi_rt,
                                        bxpi_rt, bxpi_acc
                               )
) {

#  if (!group_by) {
    # if group_by arguments provided
    if (group_by_block) {
      # group by block and group_by argument
      data =
        data |>
        group_by(across(any_of(unique(c("block")))))
    } else {
      # don't group
    }
 # } else {
    # NO group_by arguments provided
    #if (group_by_block) {
      # group by block
    #  data =
    #    data |>
    #    group_by(across(any_of(c("block"))))
    #}
  #}
  # standardize columns
  data =
    data |>
    mutate(across(.cols = ({{cols}}),
                  .fns = list(z = ~as.numeric(scale(.x))),
                  .names = "{.col}_{.fn}"
    )
    ) |> ungroup()

  return(data)
}


axcpt_standardize2 <- function(data,
                              group_by_block = TRUE,
                              group_by = block,
                              cols = c(rt_AX, rt_AX, rt_BX, rt_BY,
                                       dp_context, a_cue, pbi_err, pbi_rt,
                                       bxpi_rt, bxpi_acc
                                       )
                              ) {

  if (!group_by) {
    # if group_by arguments provided
    if (group_by_block) {
      # group by block and group_by argument
      data =
        data |>
        group_by(across(any_of(unique(c("block", {{group_by}})))))
    } else {
      # group only by group_by
      data =
        data |>
        group_by(across(any_of(c({{group_by}}))))
    }
  } else {
    # NO group_by arguments provided
    if (group_by_block) {
      # group by block
      data =
        data |>
        group_by(across(any_of(c("block"))))
      }
  }
  # standardize columns
  data =
    data |>
    mutate(across(.cols = ({{cols}}),
                  .fns = list(z = ~as.numeric(scale(.x))),
                  .names = "{.col}_{.fn}"
                  )
    ) |> ungroup()

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

axcpt_correlate <- function(data,
                            cols,# = c(rt_AX, rt_AX, rt_BX, rt_BY,
                                 #     dp_context, a_cue, pbi_err, pbi_rt,
                                 #     bxpi_rt, bxpi_acc
                                #     ),
                            groups = NULL,
                            filter_pvalue = 1
                            ) {
  corr =
    data |>
    select(any_of(unique(c({{cols}}, groups)))) |>
    drop_na() |>
    group_by(across(any_of(groups))) |>
    correlation::correlation(partial = TRUE) |>
    select(-Method) |>
    mutate(across(.cols = where(is.numeric),  # where the columns are numeric
                   .fns = ~round(.x, 3))       # apply the rounding function
                 ) |>
    filter(p < filter_pvalue)
  return(corr)
}

axcpt_correlate2 <- function(data,
                            #cols = c(rt_AX, rt_AX, rt_BX, rt_BY,
                            #         dp_context, a_cue, pbi_err, pbi_rt,
                            #         bxpi_rt, bxpi_acc
                            #),
                            cols,
                            groups = NULL
) {
  corr =
    data |>
    select(any_of(unique(c(quote({{cols}}), groups)))) |>
    drop_na() |>
    group_by(across(any_of(groups))) |>
    correlation::correlation(partial = TRUE) |>
    select(-Method) |>
    mutate(across(.cols = where(is.numeric),  # where the columns are numeric
                  .fns = ~round(.x, 3))       # apply the rounding function
    )
  return(corr)
}


axcpt_pairs_plot <- function(data,
                             cols,
                             group = NULL,
                             #color = "grey",
                             group_colors = c("cornflowerblue", "firebrick"),
                             ...
                             ) {

  #group_colors <- c("cornflowerblue", "firebrick")

  if (!is.null(group)) {

    return(
      data |>
      select(any_of(c(cols, group))) |>
      GGally::ggpairs(
        mapping = aes(col = {{group}},    # set color by group
                      alpha = .3      # adjust transparency
                      )
        ) +
      scale_color_manual(values = group_colors) +
      scale_fill_manual(values = group_colors)
    )

  } else {

    return(
      data |>
      select(any_of(c(cols))) |>
      GGally::ggpairs(mapping = aes(alpha = .3))
    )
  }
}


axcpt_pairs_plot2 <- function(
    data,
    cols,
    group = NULL,
    #color = "grey",
    group_colors = c("cornflowerblue", "firebrick"),
    ...
) {
  if (!is.null(group)) {
    return(
      data |>
        select(any_of(c(cols, group))) |>
        GGally::ggpairs(mapping = aes(col = block, alpha = .5),
                        lower = list(continuous = wrap("smooth", se = FALSE))
        ) +
        scale_color_manual(values = group_colors) +
        scale_fill_manual(values = group_colors)
    )
  } else {

    return(
      data |>
        select(any_of(c(cols, group))) |>
        GGally::ggpairs(mapping = aes(alpha = .5),
                        lower = list(continuous = wrap("smooth",  fullrange = TRUE, se = FALSE))
        )
    )
  }
}


axcpt_pairs_measures <- function(
    data,
    standardized = TRUE,
    group = NULL,
    group_colors = c("cornflowerblue", "firebrick"),
    ...
) {

  cols = c("dp_context", "a_cue", "pbi_err", "pbi_rt")

  #group_colors <- c("cornflowerblue", "firebrick")

  if (!is.null(group)) {

    if (standardized) {
      data = data |>
        select(any_of(c(paste0(cols, "_Z"), group)))
    } else {
      data = data |>
        select(any_of(c(cols, group)))
    }

    return(
      data |>
        GGally::ggpairs(
          mapping = aes(col = {{group}},    # set color by group
                        alpha = .3      # adjust transparency
          )
        ) +
        scale_color_manual(values = group_colors) +
        scale_fill_manual(values = group_colors)
    )

  } else {

    if (standardized) {
      data = data |>
        select(any_of(c(paste0(cols, "_Z"), group)))
    } else {
      data = data |>
        select(any_of(c(cols, group)))
    }

    return(
      data |>
        #select(any_of(c(cols))) |>
        GGally::ggpairs(mapping = aes(alpha = .3))
    )
  }
}

axcpt_summarize <- function(
    data,
    group_by_block = TRUE,
    #group_by,
    cols = c(rt_AX, rt_AX, rt_BX, rt_BY,
             dp_context, a_cue, pbi_err, pbi_rt,
             bxpi_rt, bxpi_acc
             ),
    ...
) {
  if (!missing(data)) {
    #print(group_by) #groups = group_by({{ group_var }})

  if (group_by_block) {
    # create grouping structure
    data =
      data |>
      #group_by(block)
      group_by(across(any_of(c("block"))))

  } else {
    # don't group
  }
  # perform the summary
  data =
    data |>
    summarize(across(
      .cols = {{cols}},
      .fns = list(
        mean = ~mean(na.omit(.x)),
        mdn  = ~median(na.omit(.x)),
        se   = ~sd(na.omit(.x)) / sqrt(length(na.omit(.x))),
        n    = ~length(na.omit(.x))
        ),
      .names = "{.col}_{.fn}"
    ))

  return(data |> ungroup() )

  } else {
    message("Data frame missing.")
  }
}


summarize_with_functions <- function(
    data,
    cols,
    stats
    ) {

  if (missing(stats)) {
    stats = c("mean", "mdn", "se", "n")
  }

  stats_fns <- lapply(
    stats, function(fn) {
    switch(fn,
           mean = ~mean(na.omit(.x)),
           mdn  = ~median(na.omit(.x)),
           sd   = ~sd(na.omit(.x)),
           # standard error of the mean
           se   = ~sd(na.omit(.x)) / sqrt(length(na.omit(.x))),
           # min = ,
           # max = ,
           # skew = ,
           # kurt =
           # Compute the coefficient of variation
           cv   = ~sd(na.omit(.x)) / mean(na.omit(.x)) * 100,
           n    = ~length(na.omit(.x))
    )
  })

  data =
    data |>
    summarize(across(
      .cols = {{cols}},
      .fns = stats_fns,
      .names = "{.col}_{.fn}"
    ))

  print(paste0)
  return(data |> ungroup())

}


################################################################################
# End Functions
################################################################################

