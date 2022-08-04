if (stage_with_compose) {
  source("R/figs/staging/prkcda_expression.R")
}

pmm_pkc_dotplot_adjusted <- pmm_pkc_dotplot + theme(plot.margin = margin(t = 7, unit = "mm"))

fig_4_top <-
  plot_grid(
    pmm_pkc_dotplot_adjusted,
    zf_scatac_umap,
    ncol = 2,
    rel_widths = c(1.5, 1),
    align = "h",
    axis = "b",
    labels = c("A", "B") 
  )

scatac_trackplot <- p1/p2/p3/p4/p5 + plot_layout(heights = c(10, 2, 1, 1, 0.1))
e4_atac_plot <- p6/p7/p8/p9 + plot_layout(heights = c(3, 2, 1, 0.1))

fig_4_mid_left <-
  plot_grid(
    scatac_trackplot,
    e4_atac_plot,
    nrow = 2,
    labels = c("C", "D"),
    vjust = c(1.5, 0), 
    rel_heights = c(1.65,1),
    align = "v",
    axis = "l"
  )

fig_4_mid_right <- 
  plot_grid(
    tfbs_venn,
    NULL,
    NULL,
    nrow = 3,
    rel_heights = c(1,1,1.5),
    labels = c("E", "F", "G")
  )

fig_4_mid <- 
  plot_grid(
    fig_4_mid_left,
    fig_4_mid_right,
    ncol = 2,
    rel_widths = c(1.65, 1)
  )

fig_4_subbottom <- 
  plot_grid(
    NULL,
    ncol = 1
    
  )

fig_4 <-
  cowplot::plot_grid(
    fig_4_top,
    fig_4_mid,
    fig_4_subbottom,
    nrow = 3, 
    # align = "v",
    # axis = "l",
    rel_heights = c(1.5,3,1)
  )

save_plot(
  fig_4,
  # filename = "test.png",
  filename = str_glue("{figs_out}/figure4.{device}"),
  base_width = 7.5,
  base_height = 9.75
)

