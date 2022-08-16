# related to figure 6
if (stage_with_compose) {
  # source("R/figs/staging/runx_cxcl8_emb_scrnaseq.R")
  # source("R/figs/staging/runx_cxcl8_km_facs.R")
  # source("R/figs/staging/runx_cxcl8_marrow_scrnaseq.R")
}


fig_S6_top <-
  cowplot::plot_grid(
    NULL,
    NULL,
    cxcl8_partition_umap,
    ncol = 3, 
    rel_widths = c(1, 1, 1.4),
    labels = c("A", "B", "C"))

fig_S6_mid <- 
  cowplot::plot_grid(
    cxcl8_marrow_genebubbles,
    ncol = 1,
    rel_widths = c(1),
    labels = c("D")
  )


fig_S6_bottom <-
  cowplot::plot_grid(
    cxcl8_heme_pseudotime_genes,
    cxcl8_marrow_niche_overlay,
    NULL,
    # align = "h",
    # axis = "b",
    ncol = 3,
    rel_widths = c(1, 1.25, 0.5),
    labels = c("E", "F")
  )

fig_S6_subbottom <- 
  cowplot::plot_grid(
    cxcl8_niche_aggscore_heatmap,
    NULL,
    ncol = 2,
    rel_widths = c(2,1),
    labels = c("G")
  )


fig_S6 <- 
  cowplot::plot_grid(
    fig_S6_top, 
    fig_S6_mid, 
    fig_S6_bottom,
    fig_S6_subbottom,
    align = "v", 
    axis = "l",
    nrow = 4,
    rel_heights = c(1,0.9,1,1)
  )


save_plot(
  fig_S6,
  filename = "s6_temp.png",
  # filename = str_glue("{figs_out}/figureS6.{device}"),
  base_width = 7.5,
  base_height = 9.75
)
