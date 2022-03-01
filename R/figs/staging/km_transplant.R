# KM transplant plots----------------------------------------------------------

tpa_inh_txp_plot <-
  ggplot(
    data = km_txp_data %>% dplyr::filter(experiment %in% c("150220", "150227","150327","150430")),
    mapping = aes(
      x = label,
      y = log2(gr_ratio),
      fill = label,
      color = label
    )
  ) +
  geom_jitter(
    shape = jitter_shape,
    width = jitter_width,
    size = jitter_size,
    stroke = jitter_stroke,
    height = 0
  ) +
  scale_fill_manual(values = alpha(experimental_group_palette, 0.4)) +
  scale_color_manual(values = experimental_group_palette) +
  stat_summary(
    fun.data = data_summary_mean_se,
    color = summarybox_color,
    size = summarybox_size,
    width = summarybox_width,
    alpha = summarybox_alpha,
    geom = summarybox_geom
  ) +
  stat_compare_means(method = "wilcox",
                     comparisons = list(c("control", "TPA"),c("control","TPA+PKCi"),c("control","TPA+MEKi"),c("TPA","TPA+PKCi"),c("TPA","TPA+MEKi")),
                     label = "p.signif",
                     label.x.npc = "center",vjust = 0.5,hide.ns = T
                     ) +
  theme(legend.position = "none") +
  labs(x = NULL,
       y  = "Log<sub>2</sub>(Green/Red)",
       fill = NULL,
       title = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0.025,0.075))) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  theme(axis.title.y = element_markdown())

st_lt_tpa_txp_plot <-
  ggplot(
    data = km_txp_data %>% 
      dplyr::filter(experiment %in% c("141219_lt","141219_st")) %>% 
      mutate(term = recode(experiment, "141219_st" = "4 wpt" , "141219_lt" = "12 wpt")) %>%
      mutate(term = factor(term, levels = c("4 wpt", "12 wpt"))),
    mapping = aes(
      x = label,
      y = log2(gr_ratio),
      fill = label,
      color = label
    )
  ) +
  geom_jitter(
    shape = jitter_shape,
    width = jitter_width,
    size = jitter_size,
    stroke = jitter_stroke,
    height = 0
  ) +
  scale_fill_manual(values = alpha(experimental_group_palette, 0.4)) +
  scale_color_manual(values = experimental_group_palette) +
  stat_summary(
    fun.data = data_summary_mean_se,
    color = summarybox_color,
    size = summarybox_size,
    width = summarybox_width,
    alpha = summarybox_alpha,
    geom = summarybox_geom
  ) +
  stat_compare_means(method = "wilcox",
                     label = "p.signif",
                     label.x.npc = "center") +
  theme(legend.position = "none") +
  labs(x = NULL,
       y  = "Log<sub>2</sub>(Green/Red)",
       fill = NULL,
       title = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0.1)))+
  facet_wrap(facets = vars(term)) +
  theme(strip.background = element_blank()) +
  theme(axis.title.y = element_markdown())


ddg_txp_plot <- 
  ggplot(
  data = km_txp_data %>% dplyr::filter(experiment == "130718") %>% mutate(term = "4 wpt"),
  mapping = aes(
    x = label,
    y = log2(gr_ratio),
    fill = label,
    color = label
  )
) +
  geom_jitter(
    shape = jitter_shape,
    width = jitter_width,
    size = jitter_size,
    stroke = jitter_stroke,
    height = 0
  ) +
  scale_fill_manual(values = alpha(experimental_group_palette, 0.4)) +
  scale_color_manual(values = experimental_group_palette) +
  stat_summary(
    fun.data = data_summary_mean_se,
    color = summarybox_color,
    size = summarybox_size,
    width = summarybox_width,
    alpha = summarybox_alpha,
    geom = summarybox_geom
  ) +
  stat_compare_means(method = "t.test",
                     label = "p.signif",
                     label.x.npc = "center") +
  theme(legend.position = "none") +
  labs(x = NULL,
       y  = "Log<sub>2</sub>(Green/Red)",
       fill = NULL,
       title = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0.1))) +
  facet_wrap(facets = vars(term)) +
  theme(strip.background = element_blank()) +
  theme(axis.title.y = element_markdown())
