# related to figure 4
if (stage_with_compose) {
  source("R/figs/staging/prkcda_expression.R")
}


fig_S2_top <- 
  plot_grid(
    tss_plot,
    nrow = 1,
    labels = "A"
  )

fig_S2_mid <- 
  plot_grid(
  ga_heatmap,
  NULL,
  nrow = 1,
  rel_widths = c(4,1),
  labels = c("B", "")
  )

fig_S2_mid2 <- 
  plot_grid(
    scatac_cluster_assignment_barplot,
    NULL,
    nrow = 1,
    labels = c("C", ""),
    rel_widths = c(9,1)
  )

fig_S2_bottom <- 
  plot_grid(
  tf_feature_plots$CEBPD + theme(legend.position = "bottom", legend.justification = "center"),
  tf_feature_plots$SPIB + theme(legend.position = "bottom", legend.justification = "center"),
  tf_feature_plots$`GATA1::TAL1` + theme(legend.position = "bottom", legend.justification = "center"),
  tf_feature_plots$ZNF148 + theme(legend.position = "bottom", legend.justification = "center"),
  ncol = 5,
  rel_widths = c(1,1,1,1,0.5),
  labels = c("D", "E", "F", "G"),
  align = "h", 
  axis = "b"
    )

fig_S2_subsubbottom <- 
  plot_grid(
    e4_atac_tss_enrichment_plot,
    tfbs_consensus_plot,
    ncol = 3,
    rel_widths = c(1,1,1), 
    labels = c("H", "I", "")
  )

fig_S2 <- 
  plot_grid(
    fig_S2_top,
    fig_S2_mid,
    fig_S2_mid2,
    fig_S2_bottom,
    fig_S2_subsubbottom,
    nrow = 5,
    rel_heights = c(1,1,1,1,1)
  )

save_plot(
  fig_S2,
  # filename = "test_s2.png",
  filename = str_glue("{figs_out}/figureS2.{device}"),
  base_width = 7.5,
  base_height = 9.75
)