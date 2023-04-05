# fig 1H HA100 plot ####--------------------------------------------------------------------------------------------------------------
HA100_plot <-
  ggplot(
    data = HA100_data %>% 
      mutate(label_new = recode(labels, "control" = "control")) %>%
      mutate(label_new = factor(label_new, levels = c("control", "HA100"))),
    mapping = aes(
      x = label_new,
      y = runx_fold_change,
      fill = label_new,
      color = label_new
    )
  ) +
  geom_hline(yintercept = 1, color = "grey80", linetype = "dashed") +
  geom_violin() + 
  geom_jitter(
    shape = jitter_shape,
    width = jitter_width,
    size = 0.5,
    stroke = jitter_stroke,
    height = jitter_height
  ) +
  scale_fill_manual(values = alpha(c(experimental_group_palette),jitter_alpha_fill)) +
  scale_color_manual(values = experimental_group_palette) +
  stat_summary(
    fun.data = data_summary_mean_se,
    color = summarybox_color,
    size = summarybox_size,
    width = summarybox_width,
    alpha = summarybox_alpha,
    geom = summarybox_geom
  ) +
  stat_compare_means(
    method = "wilcox",
    label = "p.signif",
    label.x.npc = "center",
    label.y.npc = 0.95
  ) +
  labs(x = NULL, y = "HSPCs/CHT", title = NULL) +
  theme(legend.position = "none") + 
  labs(y = "Fold Change in HSPCs", x = NULL) 

# fig 1G prkcda runx stables####----------------------------------------------------------------------------------------------------------------------------------------------------
stable_prkcda_plot <-
  ggplot(
    data = stable_prkcda_runx_df %>% 
      mutate(label_new = recode(label, "-" = "-", "+" = "TG")) %>%
      mutate(label_new = factor(label_new, levels = c("-", "TG"))),
    mapping = aes(
      x = label,
      y = runx_fold_change,
      fill = label,
      color = label
    )
  ) +
  geom_hline(yintercept = 1, color = "grey80", linetype = "dashed") +
  geom_violin() + 
  geom_jitter(
    shape = jitter_shape,
    width = jitter_width,
    size = 0.5, 
    stroke = jitter_stroke,
    height = jitter_height
  ) +
  scale_fill_manual(values = alpha(c(experimental_group_palette),jitter_alpha_fill)) +
  scale_color_manual(values = experimental_group_palette) +
  stat_summary(
    fun.data = data_summary_mean_se,
    color = summarybox_color,
    size = summarybox_size,
    width = summarybox_width,
    alpha = summarybox_alpha,
    geom = summarybox_geom,
  ) +
  stat_compare_means(
    method = "wilcox",
    label = "p.signif",
    label.x.npc = "center",
    label.y.npc = 0.95
  ) +
  theme(legend.position = "none") +
  labs(y = "Fold Change in HSPCs", x = NULL) +
  theme(plot.title = element_text(hjust = 0.5))


# fig 1E pkc family runx imaging data ####-----------------------------------------------------------------------------------
all_pkc_runx <-
  ggplot(
    data = prkc_transients_all %>%
      mutate(label_new = recode(category, "mcs" = "control")), 
    mapping = aes(
      x = label_new,
      y = runx_fold_change,
      fill = label_new,
      color = label_new)
  ) +
  geom_hline(yintercept = 1,
             color = "grey80",
             linetype = "dashed") +
  geom_violin() + 
  geom_jitter(
    shape = jitter_shape,
    width = jitter_width,
    height = jitter_height,
    size = 0.5,
    stroke = jitter_stroke,
  ) +
  scale_fill_manual(values = alpha(c(experimental_group_palette),jitter_alpha_fill)) +
  scale_color_manual(values = experimental_group_palette) +
  stat_summary(
    fun.data = data_summary_mean_se,
    color = summarybox_color,
    size = summarybox_size,
    width = summarybox_width,
    alpha = summarybox_alpha,
    geom = summarybox_geom,
  ) +
  facet_wrap(facets = vars(gene), nrow = 1, scales = "free_x") +
  stat_compare_means(mapping = aes(group = label_new),
                     method = "t.test",
                     label = "p.signif",
                     label.x.npc = "center",
                     comparisons = list(c(1,2)),
                    ) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(y = "Fold Change in HSPCs", x = NULL) +
  theme(strip.background = element_blank(), 
        strip.text = element_blank()) 
