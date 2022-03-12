# related to figure 6
if (stage_with_compose) {
  source("R/figs/staging/runx_cxcl8_emb_scrnaseq.R")
  source("R/figs/staging/runx_cxcl8_km_facs.R")
  source("R/figs/staging/runx_cxcl8_marrow_scrnaseq.R")
}


fig_S5_top <-
  cowplot::plot_grid(
    cxcl8_partition_umap,
    NULL,
    ncol = 2, 
    rel_widths = c(1.4, 2),
    labels = c("A", ""))

fig_S5_mid <- 
  cowplot::plot_grid(
    cxcl8_marrow_tal1_umap, 
    cxcl8_marrow_spi1b_umap,
    cxcl8_marrow_mpx_umap,
    align = "h", 
    axis = "b",
    ncol = 3,
    rel_widths = c(1,1,1),
    labels = c("B", "C", "D")
  )


fig_S5_bottom <-
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

fig_S5_subbottom <- 
  cowplot::plot_grid(
    cxcl8_niche_aggscore_heatmap,
    NULL,
    ncol = 2,
    rel_widths = c(2,1),
    labels = c("G")
  )


fig_S5 <- 
  cowplot::plot_grid(
    fig_S5_top, 
    fig_S5_mid, 
    fig_S5_bottom,
    fig_S5_subbottom,
    align = "v", 
    axis = "l",
    nrow = 4,
    rel_heights = c(1,0.9,1,1)
  )


save_plot(
  fig_S5,
  filename = str_glue("{figs_out}/figureS5.{device}"),
  base_width = 7.5,
  base_height = 9.75
)
