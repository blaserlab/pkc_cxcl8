theme_set(theme_cowplot(font_size = 14))
pres_out <- fs::path("~/network/P/blaser_lab_p/presentations/bmi_20221021/r_outs")
umap_all_leiden <- bb_var_umap(
  cds_pmm_final,
  var = "cluster",
  # leiden clusters
  overwrite_labels = T,
  legend_pos = "none",
  cell_size = 0.5,
  foreground_alpha = 0.3,
  group_label_size = 5
  
)
save_plot(umap_all_leiden, 
          filename = fs::path(pres_out, "umap_all_leiden", ext = "png"),
          base_width = 4, 
          base_height = 3.5)  

heme_gene_umap <- 
  bb_gene_umap(cds_pmm_final, 
               c("ptprc", "gata1a"), 
               ncol = 1) +
  labs(x = NULL, y = NULL) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank()) +
  theme(legend.position = "none") +
  theme(strip.text = element_text(face = "italic")) +
  theme(panel.background = element_rect(color = "black", linewidth = 1))

save_plot(heme_gene_umap, 
          filename = fs::path(pres_out, "heme_gene_umap", ext = "png"),
          base_width = 3.5,
          base_height = 6.0)

bb_cellmeta(cds_pmm_final) |> count(revision_cluster_assignment)

colData(cds_pmm_final)$heme_vs_other <- colData(cds_pmm_final)$cluster_binary_type
colData(cds_pmm_final)$heme_vs_other <- ifelse(colData(cds_pmm_final)$revision_cluster_assignment %in% "HSC/Thr",
                                               "heme",
                                               colData(cds_pmm_final)$heme_vs_other)


heme_vs_other <- bb_var_umap(cds_pmm_final, 
                             "heme_vs_other", 
                             overwrite_labels = T, 
                             group_label_size = 5)

save_plot(heme_vs_other,
          filename = fs::path(pres_out, "heme_vs_other", ext = "png"),
          base_height = 3.5, 
          base_width = 4.0)

heme_only_leiden <- 
  bb_var_umap(filter_cds(cds_pmm_final, cells = bb_cellmeta(cds_pmm_final) |> filter(heme_vs_other == "heme")), 
              "cluster",
              overwrite_labels = T,
              group_label_size = 5,
              plot_title = "Hematopoietic Cells")

save_plot(heme_only_leiden,
          filename = fs::path(pres_out, "heme_only_leiden", ext = "png"),
          base_height = 3.75,
          base_width = 4.0)


save_plot(
  agg_score_heatmap,
  filename = fs::path(pres_out, "agg_score_heatmap", ext = "png"),
  base_width = 7,
  base_height = 4
)

save_plot(
  pmm_heme_varplot,
  filename = fs::path(pres_out, "pmm_heme_varplot", ext = "png"),
  base_width = 4.0,
  base_height = 3.5
)

save_plot(
  pmm_gene_dotplot,
  filename = fs::path(pres_out, "pmm_gene_dotplot", ext = "png"),
  base_height = 4,
  base_width = 7
)


save_plot(
  pmm_heme_faceted_densityplot,
  filename = fs::path(pres_out, 
                      "pmm_heme_faceted_densityplot", ext = "png"),
  base_width = 7,
  base_height = 3.5
)

save_plot(
  prkcda_scrnaseq_cluster_membership_plot,
  filename = fs::path(pres_out,
                      "prkcda_scrnaseq_cluster_membership_plot", 
                      ext = "png"),
  base_width = 3.5,
  base_height = 3.5
)

save_plot(
  gestalt_fishname_umaps,
  filename = fs::path(pres_out,
                      "gestalt_fishname_umaps", 
                      ext = "png"),
  base_width = 8,
  base_height = 3.5
  
)

save_plot(
  scgestalt_barcode_sharing_plot,
  filename = fs::path(pres_out,
                      "scgestalt_barcode_sharing_plot",
                      ext = "png"),
  base_width = 3.5,
  base_height = 3.5)

save_plot(pmm_pkc_dotplot + 
            labs(title = "Reference scRNA-seq Expression") +
            theme(plot.title = element_text(hjust = 0.5)), 
          filename = fs::path(pres_out,
                              "pmm_pkc_dotplot",
                              ext = "png"),
          base_width = 5.0,
          base_height = 3.0)

save_plot(scatac_cluster_assignment_barplot, 
          filename = fs::path(pres_out, 
                              "scatac_cluster_assignment_barplot", 
                              ext = "png"),
          base_width = 7,
          base_height = 3.0)

save_plot(zf_scatac_umap,
          filename = fs::path(pres_out,
                              "zf_scatac_umap",
                              ext = "png"),
          base_width = 4.0, 
          base_height = 3.5)


save_plot(ga_heatmap, 
          filename = fs::path(pres_out,
                              "ga_heatmap", 
                              ext = "png"),
          base_width = 7.0,
          base_height = 3.0)

save_plot(scatac_trackplot,
          filename = fs::path(pres_out,
                              "scatac_trackplot",
                              ext = "png"),
          base_width = 6,
          base_height = 6)
