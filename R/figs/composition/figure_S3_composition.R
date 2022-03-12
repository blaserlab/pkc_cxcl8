# related to figure 5

fig_S3_top <- plot_grid(
  NULL,
  NULL,
  labels = c("A", "B"),
  ncol = 2, 
  rel_widths = c(1,1)
)

fig_S3 <- plot_grid(
  fig_S3_top,
  crispr_variants_plot,
  NULL,
  labels = c("", "C", ""),
  vjust = c(1.5, 0, 1.5),
  nrow = 3,
  rel_heights = c(1.25, 2, 1)
)

save_plot(
  fig_S3,
  filename = str_glue("{figs_out}/figureS3.{device}"),
  base_width = 7.5,
  base_height = 9.75
  
)

