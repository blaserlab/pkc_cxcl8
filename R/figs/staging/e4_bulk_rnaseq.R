# E4 HUVEC bulk RNAseq-----------------------------
col_fun_bulkrna <-
  colorRamp2(breaks = c(min(scale(t(
    e4_heatmap_mat
  ))),
  0,
  max(scale(t(
    e4_heatmap_mat
  )))),
  colors = heatmap_3_colors)

e4_bulkrna_anno_df <- tibble(sample = rownames(scale(t(e4_heatmap_mat)))) %>%
  mutate(treatment = str_extract(sample,"control|cxcl8")) %>%
  mutate(treatment = factor(treatment, levels = c("cxcl8", "control"))) %>%
  select(-sample) %>%
  as.data.frame()
rownames(e4_bulkrna_anno_df) <- rownames(scale(t(e4_heatmap_mat)))

# make the annotation objects
e4_bulkrna_ha <- HeatmapAnnotation(
  df = e4_bulkrna_anno_df,
  col = list(treatment = c(
    "control" = "#E78AC3",
    "cxcl8" = "#A6D854"
  )),
  annotation_legend_param = list(treatment = list(
    title = "Treatment",
    title_gp = gpar(fontface = "plain", fontsize = 9),
    labels_gp = gpar(fontsize = 8),
    direction = "horizontal"
  )),
  show_annotation_name = F,
  gp = gpar(col = "white"),
  which = "row"
)


e4_highlights <-
  c(
    "DUSP5",
    "KITLG",
    "BMP2",
    "CXCL8",
    "CSF2",
    "VCAM1",
    "CCL7",
    "BIRC3",
    "LIF",
    "CCL5",
    "IL1A",
    "CSF1",
    "CXCL1",
    "CXCL2"
  )
e4_bulkrna_gene_anno_df <-
  map(
    .x = e4_highlights,
    .f = function(x) {
      index <- which(colnames(t(e4_heatmap_mat)) == x)
      return(index)
    }
  ) %>% set_names(e4_highlights) %>%
  bind_cols() %>%
  pivot_longer(everything()) %>%
  as.data.frame()

e4_bulkrna_gene_anno <- HeatmapAnnotation(
  foo = anno_mark(
    at = e4_bulkrna_gene_anno_df$value,
    labels = e4_bulkrna_gene_anno_df$name,
    labels_gp = gpar(fontsize = 8),
    padding = 1.5,
    labels_rot = 45),
  
  which = "column"
)


e4_bulkrna_heatmap <- grid.grabExpr(draw(Heatmap(
  matrix = scale(t(e4_heatmap_mat)),
  col = col_fun_bulkrna,
  heatmap_legend_param = list(title = "Normalized Counts",
                              title_gp = gpar(fontface = "plain", fontsize = 9),
                              grid_width = unit(0.14,"in"),
                              labels_gp = gpar(fontsize = 8),
                              direction = "horizontal"),
  row_dend_width = unit(2,"mm"),
  column_dend_height = unit(5,"mm"),
  column_dend_side = "bottom",
  show_row_names = F,
  show_column_names = F,
  right_annotation = e4_bulkrna_ha,
  top_annotation = e4_bulkrna_gene_anno, 
  row_dend_gp = gpar(lwd = 0.5), 
  column_dend_gp = gpar(lwd = 0.5)
  
), heatmap_legend_side = "bottom"), wrap = TRUE)


# gsea plots----------------------------------------------------- 
e4_ranks <- e4_res_table$log2FoldChange
names(e4_ranks) <- e4_res_table$gene_name

gsea_plot_HSC_diff <- fgsea::plotEnrichment(gsea_pathways[["WP_HEMATOPOIETIC_STEM_CELL_DIFFERENTIATION"]],
               e4_ranks) + 
  labs(title = "WP_HEMATOPOIETIC_STEM_\nCELL_DIFFERENTIATION") + 
  theme(plot.title = element_text(hjust = 0.5, size = 8))

gsea_plot_cytokine <- fgsea::plotEnrichment(gsea_pathways[["KEGG_CYTOKINE_CYTOKINE_RECEPTOR_INTERACTION"]],
               e4_ranks) +
  labs(title = "KEGG_CYTOKINE_CYTOKINE_\nRECEPTOR_INTERACTION") +
  theme(plot.title = element_text(hjust = 0.5, size = 8))

gsea_plot_interferon <- fgsea::plotEnrichment(gsea_pathways[["REACTOME_INTERFERON_SIGNALING"]],
               e4_ranks) +
  labs(title = "REACTOME_INTERFERON_\nSIGNALING") +
  theme(plot.title = element_text(hjust = 0.5, size = 8))

gsea_plot_perk <- fgsea::plotEnrichment(gsea_pathways[["REACTOME_PERK_REGULATES_GENE_EXPRESSION"]], 
                                              e4_ranks, gseaParam = 1) +
  labs(title = "REACTOME_PERK_REGULATES\nGENE_EXPRESSION") +
  theme(plot.title = element_text(hjust = 0.5, size = 8))

