if (stage_with_compose) {
  source("R/figs/staging/embryo_bulk_rnaseq.R")
  source("R/figs/staging/sele_pkc_runx.R")
}


fig_1_top <- plot_grid(
  NULL,
  volcanoplot + theme(plot.margin = unit(c(0,5,0,5), "mm")),
  pkc_family_plot,
  nrow = 1,
  labels = "AUTO",
  align = "h",
  axis = "b",
  rel_widths = c(1,1,1)
)

fig_1_mid <- plot_grid(
  NULL, 
  NULL, 
  NULL,
  nrow = 1,
  rel_widths = c(1,1,1),
  labels = c("D","E","F")
)

fig_1_bot <- plot_grid(
  NULL,
  NULL,
  all_pkc_runx + theme(plot.margin = unit(c(5,0,0,5), "mm")),
  nrow = 1,
  rel_widths = c(1, 1, 1),
  labels = c("G","H","I")
)

fig_1_subbot <- plot_grid(
  NULL,
  NULL,
  NULL,
  nrow = 1,
  labels = c("J", "K", "L"),
  rel_widths = c(1, 1, 1)
)

fig_1_subsubbot <- plot_grid(
  stable_prkcda_plot + theme(plot.margin = unit(c(5,0,0,5), "mm")),
  NULL,
  HA100_plot + theme(plot.margin = unit(c(5,0,0,5), "mm")),
  NULL,
  nrow = 1,
  labels = c("M", "N", "O", ""),
  rel_widths = c(0.6, 1, 0.6, 0.8)
  
)

fig_1 <- plot_grid(
 fig_1_top,
 fig_1_mid,
 fig_1_bot,
 fig_1_subbot,
 fig_1_subsubbot,
 align = "v",
 axis = "l",
 nrow = 5,
 rel_heights = c(1,1,1,1,1)
) 

save_plot(fig_1, 
          # filename = "test.png",
          filename = str_glue("{figs_out}/figure1.{device}"),
          base_width = 7.5, 
          base_height = 9.75)
