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
  labels = "B"
  )

fig_S2_bottom <- 
  plot_grid(
  tf_feature_plots$CEBPD + theme(legend.position = "bottom", legend.justification = "center"),
  tf_feature_plots$SPIB + theme(legend.position = "bottom", legend.justification = "center"),
  tf_feature_plots$`GATA1::TAL1` + theme(legend.position = "bottom", legend.justification = "center"),
  tf_feature_plots$ZNF148 + theme(legend.position = "bottom", legend.justification = "center"),
  ncol = 4,
  labels = c("C", "D", "E", "F"),
  align = "h", 
  axis = "b"
    )

fig_S2_subsubbottom <- 
  plot_grid(
    e4_atac_tss_enrichment_plot,
    tfbs_consensus_plot,
    ncol = 2,
    rel_widths = c(1,1), 
    labels = c("G","H")
  )

fig_S2 <- 
  plot_grid(
    fig_S2_top,
    fig_S2_mid,
    fig_S2_bottom,
    fig_S2_subsubbottom,
    nrow = 4,
    rel_heights = c(0.7,0.7,0.7, 1)
  )

save_plot(
  fig_S2,
  filename = str_glue("{figs_out}/figureS2.{device}"),
  base_width = 7.5,
  base_height = 9.75
)