if (stage_with_compose) {
  source("R/figs/staging/km_transplant.R")
  source("R/figs/staging/runx_cxcl8_embryo_imaging.R")
  source("R/figs/staging/thp1_qpcr.R")
  source("R/figs/staging/phospho_facs.R")
}

fig_5_left_col <-
  cowplot::align_plots(tpa_inh_txp_plot,
                       runx_cxcl8_prkcda_mut_plot,
                       runx_cxcl8_cuddletime_plot,
                       
                       align = "v",
                       axis = "l")

fig_5_top <-
  plot_grid(
    NULL,
    ddg_txp_plot,
    st_lt_tpa_txp_plot,
    align = "h",
    axis = "b",
    labels = c("A", "B", "C"),
    ncol = 3,
    rel_widths = c(2, 0.75, 1)
  )

fig_5_mid <-
  plot_grid(
    fig_5_left_col[[1]],
    pfacs_density_plot,
    runx_cxcl8_restime_plot,
    labels = c("D", "E", "F"),
    align = "h",
    axis = "b",
    ncol = 3,
    rel_widths = c(1, 1, 1)
  )

fig_5_mid2  <-
  plot_grid(
    fig_5_left_col[[2]],
    elrcxc_plot,
    runx_cxcl8_competition_plot,
    align = "h",
    axis = "b",
    ncol = 3,
    rel_widths = c(1, 1, 1),
    labels = c("G", "H", "I")
  )

fig_5_bot <-
  plot_grid(
    fig_5_left_col[[3]],
    runx_cxcl8_stables_plot,
    runx_drug_plot,
    thp1_pqcr_plot,
    align = "h",
    axis = "b",
    ncol = 4,
    rel_widths = c(1.125, 1, 3, 1),
    labels = c("J", "K", "L", "M")
  )

fig_5 <-
  plot_grid(
    fig_5_top,
    fig_5_mid,
    fig_5_mid2,
    fig_5_bot,
    align = "v",
    axis = "l",
    nrow = 4,
    rel_heights = c(1, 1, 1, 1)
  )


save_plot(
  fig_5,
  # filename = "test.pdf",
  filename = str_glue("{figs_out}/figure5.{device}"),
  base_width = 7.5,
  base_height = 9.75
)
