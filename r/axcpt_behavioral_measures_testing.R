################################################################################
# Testing
################################################################################

#AX |>
#  axcpt_set_vars(id = subid, block = block, trialnum = trialNum,
#                 trialtype = trial, accuracy = task_acc, rt = probe_rt
#  ) |>
#  axcpt_pivot() |>
#  axcpt_add_measures()
#}

AX_DATA <-
  AX |>
  axcpt_set_vars(id = subid, block = block, trialnum = trialNum,
                 trialtype = trial, accuracy = task_acc, rt = probe_rt
  ) |>
  axcpt_pivot() |>
  axcpt_add_measures() |>
  axcpt_standardize()

AX_DATA |> view_html()

#AX_DATA |>  axcpt_standardize() |> view_html() #cols = c(rt_AX, rt_AY, accuracy_AX))

AX_DATA |>
  axcpt_correlate(cols = c("rt_AX_z", "rt_AX_z", "rt_BX_z", "rt_BY_z",
                          "dp_context_z", "a_cue_z", "pbi_err_z", "pbi_rt_z",
                          "bxpi_rt_z", "bxpi_acc_z"
                          )
                  )

AX_DATA |> view_html()

AX_DATA |>
   axcpt_correlate(cols = c("rt_AX", "rt_AY", "rt_BX", "rt_BY"),
                   groups = "block",
                   filter_pvalue = .05
                   )


AX_DATA |>
  axcpt_correlate(cols = c("rt_AX", "rt_AY", "rt_BX", "rt_BY"),
                  groups = "block"
  )

AX_DATA |>
  axcpt_pairs_plot(cols = c("rt_AX", "rt_AY", "rt_BX", "rt_BY"),
                   group = "block"
  )

AX_DATA |>
  axcpt_pairs_plot2(cols = c("dp_context_z", "a_cue_z", "pbi_err_z", "pbi_rt_z"))
AX_DATA |>
  axcpt_pairs_plot2(cols = c("dp_context_z", "a_cue_z", "pbi_err_z", "pbi_rt_z"),
                    group = "block"
  )


AX_DATA |>
  axcpt_pairs_measures(cols = c("dp_context_z", "a_cue_z", "pbi_err_z", "pbi_rt_z"),
                   group = "block"
  )

AX_DATA |> axcpt_summarize()
AX_DATA |> axcpt_summarize(cols = c("hr_corr_AX", "hr_corr_AY", "hr_corr_BX", "hr_corr_BY"))
AX_DATA |> axcpt_summarize(cols = c(hr_corr_AX, hr_corr_AY, hr_corr_BX, hr_corr_BY))
#AX_DATA |> axcpt_summarize(cols = c("dp_context", "a_cue", "pbi_err", "pbi_rt"))

AX_DATA |> summarize_with_functions(cols = hr_corr_AX, stats = c("mean", "mdn", "n"))


AX_DATA |>
  mutate(id = as.factor(id)) |>
  # ggplot(mapping = aes(x = block, y = a_cue, color = id, fill = id, group = id)) +
  ggplot(mapping = aes(x = block, y = a_cue, group = id)) +
  geom_point(size = 1, alpha = 0.7) +
  geom_line(linewidth = .25, alpha = 0.8) +
  labs(y = '\nA-Cue Bias\n', x = 'Block') +
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_minimal(base_size = 16) +
  #scale_x_discrete(breaks = 0:1, labels = c("Off Meds", "On Meds")) +
  #theme(legend.position = "none")+
  #theme(legend.title = element_blank())+
  ggtitle("\nSubject-Level A-Cue Bias") +
  ggeasy::easy_center_title()

AX_DATA |>
  mutate(id = as.factor(id)) |>
  # ggplot(mapping = aes(x = block, y = a_cue, color = id, fill = id, group = id)) +
  ggplot(mapping = aes(x = block, y = dp_context, group = id)) +
  geom_point(size = 1, alpha = 0.7) +
  geom_line(linewidth = .25, alpha = 0.8) +
  labs(y = "d' Context", x = 'Block') +
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_minimal(base_size = 16) +
  #scale_x_discrete(breaks = 0:1, labels = c("Off Meds", "On Meds")) +
  #theme(legend.position = "none")+
  #theme(legend.title = element_blank())+
  ggtitle("Subject-Level d' Context") +
  ggeasy::easy_center_title()

AX_DATA |>
  mutate(id = as.factor(id)) |>
  # ggplot(mapping = aes(x = block, y = a_cue, color = id, fill = id, group = id)) +
  ggplot(mapping = aes(x = block, y = pbi_rt, group = id)) +
  geom_point(size = 1, alpha = 0.7) +
  geom_line(linewidth = .25, alpha = 0.8) +
  labs(y = "PBI Rt", x = 'Block') +
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_minimal(base_size = 16) +
  #scale_x_discrete(breaks = 0:1, labels = c("Off Meds", "On Meds")) +
  #theme(legend.position = "none")+
  #theme(legend.title = element_blank())+
  ggtitle("Subject-Level PBI RT") +
  ggeasy::easy_center_title()

# HR data by trialtype
AX_DATA |>
  select(id, block, rt_AX:rt_BY) |>
  pivot_longer(-c(id, block), names_to = "trialtype", values_to = "rt",
               names_prefix = "rt_"
               ) |>
  group_by(block, trialtype) |>
  summarize(rt = mean(na.omit(rt)), .groups = "drop") |>
  ggplot(mapping = aes(x = trialtype, y = rt, group = block)) +
  geom_line(mapping = aes(linetype = block), linewidth = 1) +
  geom_point(size = 3) +
  theme_minimal()

# rt by trialtype
AX_DATA |>
  select(id, block, hr_corr_AX:hr_corr_BY) |>
  pivot_longer(-c(id, block), names_to = "trialtype", values_to = "hr",
               names_prefix = "hr_corr_"
  ) |>
  group_by(block, trialtype) |>
  summarize(hr = mean(na.omit(hr)), .groups = "drop") |>
  ggplot(mapping = aes(x = trialtype, y = hr, group = block)) +
  geom_line(mapping = aes(linetype = block), linewidth = 1) +
  geom_point(size = 3) +
  theme_minimal()

axcpt_pairs_plot2 <- function(data,
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
                        lower = list(continuous = wrap("smooth", se = FALSE))
        )
    )
  }
}




AX_DATA |>
  filter(rt_AX < 4 & rt_AY < 4 & rt_BX < 4 & rt_BY < 4) |>
  axcpt_pairs_plot2(cols = c("rt_AX", "rt_AY", "rt_BX", "rt_BY"))

AX_DATA |>
  filter(rt_AX < 4 & rt_AY < 4 & rt_BX < 4 & rt_BY < 4) |>
  axcpt_pairs_plot2(cols = c("rt_AX", "rt_AY", "rt_BX", "rt_BY"),
                    group = "block"
  )
################################################################################
#  End Testing
################################################################################
