if (stage_with_compose) {
  source("R/figs/staging/runx_cxcl8_emb_scrnaseq.R")
  source("R/figs/staging/runx_cxcl8_km_facs.R")
  source("R/figs/staging/runx_cxcl8_marrow_scrnaseq.R")
}

fig_6_left <-
  cowplot::align_plots(
    cxcl8_emb_scrnaseq_umap_niche,
    emb_niche_violins,
    prkcd_enrichment_plot,
    cxcl8_heme_class_umap,
    align = "v",
    axis = "l"
  )

niche_pop_gene_dotplot_adjusted <- niche_pop_gene_dotplot + theme(plot.margin = margin(l = 7, unit = "mm"))

fig_6_top <-
  plot_grid(
    fig_6_left[[1]],
    niche_pop_gene_dotplot_adjusted,
    align = "h",
    axis = "b",
    labels = c("A", "B"),
    ncol = 2,
    rel_widths = c(1.4,2)
    
  )

fig_6_mid <- 
  plot_grid(
    fig_6_left[[2]],
    emb_cluster_representation_barplot,
    align = "h",
    axis = "b",
    ncol = 2,
    labels = c("C","D"),
    rel_widths = c(3,1)
  )

fig_6_mid_bottom <- 
  plot_grid(
    fig_6_left[[3]],
    km_facs_plot,
    cxcl8_heme_pseudotime_umap,
    align = "h",
    axis = "b",
    ncol = 3,
    rel_widths = c(1.5,1,1),
    labels = c("E","F","G")
  )

fig_6_bottom <-
  plot_grid(fig_6_left[[4]],
            cxcl8_heme_pseudotime_density,
            cxcl8_marrow_cluster_representation_barplot,
            align = "h",
            axis = "b",
            ncol = 3,
            rel_widths = c(1,1,1),
            labels = c("H","I", "J"))


fig_6 <-
  plot_grid(
    fig_6_top,
    fig_6_mid,
    fig_6_mid_bottom,
    fig_6_bottom,
    nrow = 4,
    rel_heights = c(1.1,1,0.9,1)
  )
ggsave2(
  plot = fig_6,
  filename = str_glue("{figs_out}/figure6.{device}"),
  width = 7.5,
  height = 9.75,
  device = cairo_pdf
)

