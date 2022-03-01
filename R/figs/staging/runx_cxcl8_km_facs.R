# km facs--------------------------------------------------------------------------------

km_facs_plot <-
  ggplot(data = km_facs_3mpf %>% filter(name == "logpml"),
         mapping = aes_string(x = "class", y = "value", color = "class", fill = "class")) +
  geom_jitter(
    shape = jitter_shape,
    width = jitter_width,
    size = jitter_size,
    stroke = jitter_stroke,
    height = 0
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
    method = "t.test",
    method.args = list(var.equal = F),
    label = "p.signif",
    label.x.npc = "center",
    label.y.npc = 0.95
  ) +
  labs(x = NULL, y = "Log<sub>2</sub>(P + M)/L", title = NULL) +
  theme(axis.title.y = element_markdown()) +
  theme(legend.position = "none")
