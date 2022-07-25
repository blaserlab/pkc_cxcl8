if (stage_with_compose) {
  source("R/figs/staging/e4_bulk_rnaseq.R")  
}

fig_S7 <-
  plot_grid(gsea_plot_HSC_diff,
            gsea_plot_cytokine,
            gsea_plot_perk,
            ncol = 3, 
            rel_widths = c(1,1,1),
            labels = c("D", "E", "F"))

save_plot(fig_S7,
          file = str_glue("{figs_out}/figureS7.{device}"),
          base_height = 2.0,
          base_width = 7.5)