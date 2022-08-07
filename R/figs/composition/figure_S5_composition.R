# related to figure 6
if (stage_with_compose) {
  source("R/figs/staging/runx_cxcl8_emb_scrnaseq.R")
  source("R/figs/staging/runx_cxcl8_km_facs.R")
  source("R/figs/staging/runx_cxcl8_marrow_scrnaseq.R")
}

fig_S5_top <-
  plot_grid(
    cxcl8_emb_scrnaseq_umap_all,
    cxcl8_emb_scrnaseq_umap_niche_or_not,
    emb_mod3_violin,
    align = "h",
    axis = "b",
    labels = c("A", "B", "C"),
    ncol = 3,
    rel_widths = c(1.5,1.5,0.8)
  )


fig_S5_bottom_left <-
  plot_grid(
    NULL,
    ht_emb_modules,
    ncol = 2,
    rel_widths = c(1,10),
    labels = c("D", "")
  )

fig_S5_bottom_right_top <-
  plot_grid(
    emb_lepr_expression_umap,
    emb_col1a1a_umap,
    align = "h",
    axis = "b",
    ncol = 2, 
    labels = c("E", "F")
  )




fig_S5_bottom_right_mid <-
  plot_grid(
    emb_msc_aggscore_heatmap,
    labels = "G"
  )


fig_S5_bottom_right_bottom <- 
  plot_grid(
    cxcl8_emb_proliferative_umap,
    cxcl8_emb_cxcl12a_umap,
    align = "h",
    axis = "b",
    ncol = 2,
    labels = c("H", "I")
  )

fig_S5_bottom_right <-
  plot_grid(
    fig_S5_bottom_right_top,
    fig_S5_bottom_right_mid,
    fig_S5_bottom_right_bottom,
    nrow = 3,
    rel_heights = c(1, 1.6, 1)
    
  )

fig_S5_bottom <-
  plot_grid(
    fig_S5_bottom_left,
    fig_S5_bottom_right,
    ncol = 2,
    rel_widths = c(1, 1.1)
  )

fig_S5 <-
  plot_grid(
    fig_S5_top,
    fig_S5_bottom,
    nrow = 2,
    rel_heights = c(1,3)
  )

save_plot(
  fig_S5,
  filename = "s5_test.png",
  # filename = str_glue("{figs_out}/figureS5.{device}"),
  base_width = 7.5,
  base_height = 9.75
)
