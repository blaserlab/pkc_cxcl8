if (stage_with_compose) {
  source("R/figs/staging/embryo_bulk_rnaseq.R")
  source("R/figs/staging/sele_pkc_runx.R")
}


fig_1_top <- plot_grid(
  NULL,
  volcanoplot,
  pkc_family_plot,
  nrow = 1,
  labels = "AUTO",
  align = "h",
  axis = "b"
)

fig_1_mid <- plot_grid(
  NULL, 
  all_pkc_runx,
  nrow = 1,
  rel_widths = c(1,2),
  labels = c("D","E")
)

fig_1_bot <- plot_grid(
  NULL, 
  stable_prkcda_plot,
  HA100_plot,
  nrow = 1,
  labels = c("F","G","H")
)

fig_1 <- plot_grid(
 fig_1_top,
 fig_1_mid,
 fig_1_bot,
 NULL,
 align = "v",
 axis = "l",
 nrow = 4,
 rel_heights = c(1,1,1,1)
) 

save_plot(fig_1, 
          filename = "test.png",
          # filename = str_glue("{figs_out}/figure1.{device}"),
          base_width = 7.5, 
          base_height = 9.75)
