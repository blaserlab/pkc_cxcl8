if (stage_with_compose) {
  source("R/figs/staging/sele_prkcda_marrow_scrnaseq.R")
}

fig_3_left <-
  cowplot::align_plots(pmm_heme_varplot, gestalt_fishname_umaps, align = "v", axis = "l")

fig_3_top <-
  plot_grid(
    fig_3_left[[1]],
    pmm_heme_faceted_densityplot,
    ncol = 2,
    align = "h",
    axis = "b",
    rel_widths = c(1.5,2.5),
    labels = c("A", "B")
  )


fig_3_row2 <- 
  plot_grid(
    pmm_gene_dotplot,
    prkcda_scrnaseq_cluster_membership_plot,
    align = "h",
    axis = "b",
    ncol = 2,
    rel_widths = c(2, 1),
    labels = c("C","D"))


fig_3_bottom <-
  plot_grid(
    fig_3_left[[2]],
    scgestalt_barcode_sharing_plot,
    ncol = 2,
    rel_widths = c(2.5,1.1),
    labels = c("E", "F")
  )





fig_3 <-
  cowplot::plot_grid(
    fig_3_top,
    fig_3_row2,
    fig_3_bottom,
    nrow = 4,
    # align = "v",
    # axis = "l",
    rel_heights = c(0.9,1,0.9,0.9)
  )

save_plot(
  fig_3,
  filename = str_glue("{figs_out}/figure3.{device}"),
  base_width = 7.5,
  base_height = 9.75
)

