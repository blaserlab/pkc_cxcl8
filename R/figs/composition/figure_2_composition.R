if (stage_with_compose) {
  source("R/figs/staging/gestalt_analysis.R")
}

fig_2_left <- cowplot::align_plots(
  NULL,
  prkcda_clone_plot,
  gestalt_cor_summary_plot,
  gestalt_areaplot[[1]] + labs(subtitle = "control") + theme(plot.subtitle = element_text(hjust = 0.5)),
  gestalt_areaplot[[17]]+ labs(subtitle = "prkcda") + theme(plot.subtitle = element_text(hjust = 0.5)),
  align  = "v",
  axis = "l"
  
)



fig_2_bot_left <- plot_grid(
  fig_2_left[[4]],
  gestalt_areaplot[[17]] + labs(subtitle = "prkcda") + theme(plot.subtitle = element_text(hjust = 0.5)),
  ncol = 2,
  labels = c("E", "")
)

fig_2_bot_midleft <- plot_grid(
  fig_2_left[[2]],
  fig_2_left[[3]],
  fig_2_bot_left,
  nrow = 3,
  labels = c("","D",""),
  rel_heights = c(1,1,1)
)

fig_2_pacmans <- plot_grid(
  pacman_plot_mcs,
  pacman_plot_prkcda + theme(legend.position = "none"),
  ncol = 2
) 

fig_2_pacmans_plus <- plot_grid(
  pacman_legend_centered,
  fig_2_pacmans,
  nrow = 2,
  rel_heights = c(1,30)
)

fig_2_bot_mid <- plot_grid(
  fig_2_bot_midleft,
  fig_2_pacmans_plus,
  ncol = 2, 
  labels = c("B","C"),
  rel_widths = c(0.75,1)
)


fig_2 <- plot_grid(
  fig_2_left[[1]],
  fig_2_bot_mid,
  labels = c("A",""),
  nrow = 2,
  rel_heights = c(1.2,4)
)

save_plot(fig_2, 
          filename = str_glue("{figs_out}/figure2.{device}"),
          base_width = 7.5, 
          base_height = 9.75)
