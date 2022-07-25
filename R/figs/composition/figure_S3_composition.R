# related to figure 5
if (stage_with_compose) {
  source("R/figs/staging/runx_cxcl8_embryo_imaging.R")
}

fig_S3_top <- plot_grid(
  NULL,
  NULL,
  labels = c("A", "B"),
  ncol = 2, 
  rel_widths = c(1,1)
)

fig_S3 <- plot_grid(
  fig_S3_top,
  crispr_variants_plot[1],
  crispr_variants_plot[2],
  NULL,
  labels = c("", "C", "","D"),
  vjust = c(0, 0, 0, -2.0),
  nrow = 4,
  rel_heights = c(1.0, 0.5, 1.7, 1.8)
)

save_plot(
  fig_S3,
  filename = str_glue("{figs_out}/figureS3.{device}"),
  # filename = "test.pdf",
  base_width = 7.5,
  base_height = 9.75
  
)

