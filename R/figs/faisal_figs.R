# source("R/stage_all_plots.R")

faisal_figs <- "~/network/P/blaser_lab_p/fellowships/faisal_pelotonia2022/figs"

faisal_fig_1 <- all_pkc_runx[["data"]] |> 
  filter(gene == "prkcda") |> 
  ggplot(mapping = aes(x = label_new, y = runx_fold_change, color = label_new, fill = label_new)) +
  geom_hline(yintercept = 1,
             color = "grey80",
             linetype = "dashed") +
  geom_jitter(
    shape = jitter_shape,
    width = jitter_width,
    height = jitter_height,
    size = jitter_size,
    stroke = jitter_stroke
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
  stat_compare_means(mapping = aes(group = label_new),
                     method = "t.test",
                     label = "p.signif",
                     label.x.npc = "center",
                     comparisons = list(c(1,2)),
                    ) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.2))) +
  theme_cowplot(font_size = 6) +
  theme(legend.position = "none") +
  # theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(y = "Fold Change in HSPCs", x = "PKC Overexpression") +
  theme(strip.background = element_blank(), 
        strip.text = element_blank()) +
  theme(plot.background = element_rect(fill = "white"))
save_plot(faisal_fig_1, file = fs::path(faisal_figs, "faisal_fig1.png"), base_width = 1.5, base_height = 1.5)

  


faisal_fig_2 <- ggplot(
  data = gestalt_20180900_named_1 %>%
    mutate(label_new = recode(class, "mcs" = "control")),
  mapping = aes(
    x = label_new,
    y = value,
    fill = label_new,
    color = label_new
  )
) +
  geom_jitter(
    shape = jitter_shape,
    width = jitter_width,
    height = 0,
    size = jitter_size
  ) +
  scale_fill_manual(values = alpha(c(experimental_group_palette), jitter_alpha_fill),
                    limits = c("control", "prkcda")) +
  scale_color_manual(values = experimental_group_palette, limits = c("control", "prkcda")) +
  stat_summary(
    fun.data = data_summary_mean_se,
    color = summarybox_color,
    size = summarybox_size,
    width = summarybox_width,
    alpha = summarybox_alpha,
    geom = summarybox_geom,
    position = position_dodge(width = 0.75),
    show.legend = FALSE
  ) +
  # stat_compare_means(
  #   mapping = aes(group = label_new),
  #   method = "t.test",
  #   label = "p.signif",
  #   label.y.npc = 0.95,
  #   label.x.npc = "center",
  #   comparisons = list(c(1, 2))
  # ) +
  facet_wrap(
    facets = vars(timepoint),
    nrow = 1,
    strip.position = "bottom"
  ) +
  theme_cowplot(font_size = 6) +
  theme(strip.placement = "outside") +
  theme(strip.background = element_blank()) +
  theme(legend.position = "top") +
  theme(legend.justification = "center") +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1)),
                     breaks = c(2, 4, 6, 8, 10, 12)) +
  theme(axis.text.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  labs(x = NULL,
       y = "HSC Clones",
       color = NULL,
       fill = NULL) +
  theme(plot.background = element_rect(fill = "white"))
save_plot(faisal_fig_2, filename = fs::path(faisal_figs, "faisal_fig2.png"), base_width = 2.5, base_height = 1.5)

scatac_trackplot <- p1/p2/p3/p4/p5 + plot_layout(heights = c(10, 2, 1, 1, 0.1))
scatac_trackplot

e4_atac_plot <- p6/p7/p8/p9 + plot_layout(heights = c(3, 2, 1, 0.1))

scatac_trackplot + e4_atac_plot + plot_layout(ncol = 1, heights =c(2, 1))

faisal_fig_4 <- plot_grid(scatac_trackplot,
          e4_atac_plot,
          ncol = 1, 
          rel_heights = c(2,1.5)
          )

save_plot(faisal_fig_4, filename = fs::path(faisal_figs, "faisal_fig4.png"), base_width = 5.0, base_height = 5.5)


