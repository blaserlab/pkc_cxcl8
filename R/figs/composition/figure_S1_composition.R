# related to figure 3
if (stage_with_compose) {
  source("R/figs/staging/sele_prkcda_marrow_scrnaseq.R")
}




fig_S1_top <- plot_grid(
  prkcda_scrnaseq_umap_cluster,
  muench_heatmap,
  ncol = 2,
  rel_widths = c(1,1),
  labels = c("A", "B")
  
)

fig_S1_bottom <- 
  plot_grid(agg_score_heatmap, 
            labels = "C")


fig_S1 <- plot_grid(
  fig_S1_top,
  fig_S1_bottom,
  NULL,
  nrow = 3,
  rel_heights = c(1,1,1)
)

save_plot(
  fig_S1,
  # filename = "test.png",
  filename = str_glue("{figs_out}/figureS1.{device}"),
  base_width = 7.5,
  base_height = 9.75
)