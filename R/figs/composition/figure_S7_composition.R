if (stage_with_compose) {
  source("R/figs/staging/e4_bulk_rnaseq.R")
  source("R/figs/staging/wb_quantification.R")
}

fig_S7_top <- plot_grid(NULL, ncol = 1)

fig_S7_middle <- plot_grid(prkcda_highlow_plot, 
                           pulldown_plot, 
                           ncol = 2, 
                           rel_widths = c(1, 1), 
                           align = "h", 
                           axis = "b", 
                           labels = c("C", "D"))

fig_S7_bottom <- plot_grid(cxcl8_treatment_erk_plot, 
                           prkcda_knockdown_erk_plot, 
                           prkcda_kd_efficiency_plot, 
                           ncol = 3, 
                           rel_widths = c(0.9, 0.9, 0.7),
                           align = "h", 
                           axis = "bt",
                           labels = c("E", "F", "G"))

fig_S7_subbottom <- plot_grid(gsea_plot_HSC_diff,
                              gsea_plot_cytokine,
                              gsea_plot_perk, 
                              ncol = 3, 
                              rel_widths = c(1,1,1),
                              labels = c("H", "I", "J"))



fig_S7 <-
  plot_grid(fig_S7_top, 
            fig_S7_middle, 
            fig_S7_bottom, 
            fig_S7_subbottom, 
            ncol = 1, 
            rel_heights = c(1,1.2,0.9,0.9))

save_plot(fig_S7,
          file = str_glue("{figs_out}/figureS7.{device}"),
          base_height = 9.75,
          base_width = 7.5)
