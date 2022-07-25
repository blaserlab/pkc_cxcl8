if (stage_with_compose) {
  source("R/figs/staging/e4_bulk_rnaseq.R")  
}


fig_7 <-
  plot_grid(e4_bulkrna_heatmap,
            NULL,
            ncol = 2,
            rel_widths = c(2,1),
            labels = c("F", ""))


save_plot(fig_7,
          file = str_glue("{figs_out}/figure7.{device}"),
          base_height = 2.0,
          base_width = 7.5)

