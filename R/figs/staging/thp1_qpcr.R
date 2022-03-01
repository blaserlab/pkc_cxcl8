# thp1 qpcr ####----------------------------------------------
thp1_pqcr_plot <- 
  ggplot(
  data = qpcr_res_final,
    mapping = aes(
      x = label,
      y = 2 ^ -ddct_18s,
      fill = label
  
)) +
  geom_jitter(
    color = "black",
    shape = jitter_shape,
    width = jitter_width,
    size = jitter_size,
    stroke = jitter_stroke,
    height = 0
  ) +
  scale_fill_manual(values = alpha(experimental_group_palette, 0.6)) +
  stat_summary(
    fun.data = data_summary_mean_se,
    color = summarybox_color,
    size = summarybox_size,
    width = summarybox_width,
    alpha = summarybox_alpha,
    geom = summarybox_geom
  ) +
  stat_compare_means(
    data = qpcr_res_final,
    mapping = aes(x = label, y = 2 ^ -ddct_18s),
    method = "t.test",
    label = "p.signif",
    label.x.npc = "center",
    label.y.npc = "top"
  ) +
  theme(legend.position = "none") +
  labs(x = NULL,
       y  = "Fold Change in CXCL8",
       fill = NULL,
       title = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0.1)))
thp1_pqcr_plot
