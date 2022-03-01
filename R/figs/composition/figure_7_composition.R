if (stage_with_compose) {
  source("R/figs/staging/e4_bulk_rnaseq.R")  
  source("R/figs/staging/phospho_facs.R")  
}


fig_7_bottom <-
  plot_grid(e4_bulkrna_heatmap,
            gsea_plot_HSC_diff,
            ncol = 2,
            rel_widths = c(2,1),
            labels = c("F", "G"))
fig_7_subbottom <-
  plot_grid(gsea_plot_cytokine,
            gsea_plot_perk,
            NULL,
            ncol = 3, 
            rel_widths = c(1,1,1),
            labels = c("H", "I", "J"))

fig_7 <- 
  plot_grid(
    NULL,
    fig_7_bottom,
    fig_7_subbottom,
    nrow = 3,
    rel_heights = c(2.5,1,1)
  )

save_plot(fig_7,
          file = str_glue("{figs_out}/figure7.{device}"),
          base_height = 9.75,
          base_width = 7.5)

# phospho facs--------------------------------------------
# save density overlay
# cyto_plot_save(
#   save_as = str_glue("{figs_out}/perk_stacked_density.{device}"),
#   width = 4.5,
#   height = 3.0)
# 
# cyto_plot(
#   facs_analysis[c(3, 4)],
#   parent = "GFP+",
#   channels = c("Alexa 647-A"),
#   density_stack = 0.4,
#   density_fill_alpha = 0.4,
#   alias = "pERK+",
#   title = NA,
#   legend = TRUE,
#   legend_text = c("control", "TPA"),
#   axes_label_text_size = 1,
#   axes_text_size = 0.8,
#   legend_text_size = 1,
#   margins = c(4, 1, 1, 7)
# )
# cyto_plot_complete()



