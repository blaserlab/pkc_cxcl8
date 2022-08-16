# niche cluster UMAPs ------------------------------------------------------------------

cxcl8_emb_scrnaseq_umap_niche_or_not <- 
  bb_var_umap(
  obj = cds_embryo_aligned,
  var = "niche_or_not", 
  overwrite_labels = T,
  group_label_size = 3,
  cell_size = 0.5, 
  foreground_alpha = 0.2, 
  palette = experimental_group_palette,
  man_text_df = tribble(
    ~text_x, ~text_y, ~label,
    -4,5,"other",
    6,-3,"niche"
  ), 
  rasterize = TRUE, 
  raster_dpi = 300
  
)



cxcl8_emb_scrnaseq_umap_niche <-
  bb_var_umap(
    obj = cds_embryo_aligned[,colData(cds_embryo_aligned)$niche_or_not == "niche"],
    var = "louvain_assignment_1",
    overwrite_labels = T,
    legend_pos = "none",
    cell_size = 1,
    foreground_alpha = 0.4,
    group_label_size = 3,
    palette = experimental_group_palette
    
  )

# violin plot module 3 genes in niche clusters vs other clusters -------------------------------------
gene_module_df_emb <- bb_rowmeta(cds_embryo_aligned) %>%
  select(gene = feature_id, gene_group = module)

emb_module3_agg_expression <- t(
  aggregate_gene_expression(cds = cds_embryo_aligned[!is.na(rowData(cds_embryo_aligned)$module),], 
                            gene_group_df = gene_module_df_emb, 
                            max_agg_value = 5)
) %>% 
  as_tibble(rownames = "barcode_sample") %>% 
  pivot_longer(-barcode_sample, names_to = "module", values_to = "module_score") %>%
  left_join(colData(cds_embryo_aligned) %>% as_tibble(rownames = "barcode_sample")) %>%
  # select(barcode_sample, niche_or_not, module, module_score) %>%
  select(barcode_sample, label = niche_or_not, module ,module_score) %>%
  filter(module == "3") %>%
  mutate(label = recode(label, "not" = "other")) %>%
  mutate(label = factor(label, levels = c("niche", "other")))
  # mutate(label = recode(niche_or_not, "not" = "other"))

emb_mod3_violin <-
  ggplot(data = emb_module3_agg_expression,
         mapping = aes(x = label, y = module_score)) +
  ggrastr::rasterise(geom_jitter(
    shape = 21,
    fill = "transparent",
    color = "black",
    alpha = 0.1,
    size = 0.5,
    stroke = 0.25
  ), dpi = 300) +
  geom_violin(aes(fill = label),
              scale = "area",
              color = "black",
              alpha = 0.4) +
  scale_fill_manual(values = experimental_group_palette) +
  labs(x = NULL, y = "Module 3 Expression") + 
  theme(legend.position = "none")

# bar plot cluster representation ----------------------------------------------------------- 
emb_cluster_representation_barplot <- 
  ggplot(
  data = cluster_proportions_emb,
  aes(x = louvain_assignment_1, y = log2(fold_change_over_control), fill = enriched)
) + 
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = experimental_group_palette, breaks = c("control", "cxcl8")) + 
  labs(x = NULL, y = "Log<sub>2</sub>(cxcl8/control)", fill = "Enriched:") +
  theme(axis.title.y = element_markdown()) +
  theme(legend.position = "top") +
  theme(legend.justification = "center") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  guides(fill = guide_legend(ncol = 1, title.theme = element_text(size = 9))) +
  geom_text(mapping = aes(y = texty, label = p.signif), nudge_y = 0.15, size = 3, show.legend = F)

# baryawno agg score heatmap-------------------------------- 
col_fun_emb_msc <- colorRamp2(breaks = c(min(scale(agg_mat_baryawno)),
                                                    0,
                                                    max(scale(agg_mat_baryawno))),
                                         colors = heatmap_3_colors)

emb_msc_aggscore_heatmap <- grid.grabExpr(draw(
  Heatmap(
    scale(as.matrix(agg_mat_baryawno)),
    col = col_fun_emb_msc,
    column_names_rot = 30,
    column_names_centered = F,
    # column_title = "MSC Sub-clusters",
    # column_title_side = "bottom",
    # column_title_gp = gpar(fontsize = 10),
    row_title = "Baryawno et al. Gene Sets",
    row_title_gp = gpar(fontsize = 10),
    heatmap_legend_param = list(
      title = "Aggregate\nScore",
      title_gp = gpar(fontface = "plain", fontsize = 8),
      grid_width = unit(0.14, "in"),
      labels_gp = gpar(fontsize = 8)
    ),
    column_dend_height = unit(5, "mm"),
    row_dend_width = unit(5, "mm"),
    row_names_gp = gpar(fontsize = 9),
    column_names_gp = gpar(fontsize = 9),
    row_dend_gp = gpar(lwd = 0.5),
    column_dend_gp = gpar(lwd = 0.5)
  )
),wrap = TRUE)


# gene dotplot for niche populations------------------------------

niche_pop_gene_dotplot <- 
  bb_genebubbles(
  obj = cds_embryo_aligned[, colData(cds_embryo_aligned)$niche_or_not == "niche"],
  cell_grouping = "louvain_assignment_1",
  genes = c(
    "lepr", #
    "col1a1a", #
    "flt4", #
    "lyve1b", #
    "mrc1a", #
    # "pdgfrl",
    # "pdgfra",
    "ngfrb", #
    "sparc", #
    # "twist1a",
    "dcn", #
    "egfl7", #
    "gpr182", #
    "sele", # 
    "acta2", #
    "myh11a", #
    "fn1a",
    "the end"
)) + 
  scale_size_area(max_size = 4) +
  labs(x = NULL, y = NULL, color = "Expression", size = "Fraction\nExpressing") + 
  guides(color = guide_colorbar(title.theme = element_text(size = 9)), 
         size = guide_legend(title.theme = element_text(size = 9))) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# MSC subclusters---------------------------------------------------------------
emb_msc_sublcuster_umap <-
  bb_var_umap(
    obj = cds_embryo_aligned[, colData(cds_embryo_aligned)$partition_assignment == "MSC"],
    var = "louvain_assignment_1",
    palette = experimental_group_palette,
    overwrite_labels = T,
    foreground_alpha = 0.6,
    cell_size = 1
  )

# violin plot with marcksl1a, kitlga, cxcl12a,bmp4--------------------------------------
niche_violin_data <- plot_genes_violin(cds = cds_embryo_aligned[rowData(cds_embryo_aligned)$gene_short_name %in% c("kitlga", "cxcl12a", "bmp4", "marcksl1a"), 
                                           colData(cds_embryo_aligned)$niche_or_not == "niche"], group_cells_by = "louvain_assignment_1")[["data"]] %>%
  as_tibble() %>%
  select(gene_short_name, louvain_assignment_1, expression) %>%
  mutate(gene_short_name = factor(gene_short_name, levels = c("kitlga", "cxcl12a", "bmp4", "marcksl1a")))

emb_niche_violins <- 
ggplot(data = niche_violin_data, 
       mapping = aes(
         x = louvain_assignment_1,
         y = log10(expression +1)# already size factor normalized by plot_genes_violin,
       )) +
  geom_jitter(
    shape = 21,
    fill = "transparent",
    mapping = aes(color = louvain_assignment_1),
    show.legend = F
  ) +
  geom_violin(
    color = "black",
    draw_quantiles = 0.5,
    scale = "width",
    mapping = aes(fill = louvain_assignment_1)
  ) +
  scale_fill_manual(values = alpha(experimental_group_palette[c("sinusoidal", "lepr+ MSC", "osteoblast", "fibroblast", "chondrocyte", "pericyte")],alpha = 0.4)) +
  scale_color_manual(values = alpha(experimental_group_palette[c("sinusoidal", "lepr+ MSC", "osteoblast", "fibroblast", "chondrocyte", "pericyte")], alpha = 0.8)) +
  facet_wrap(facets = vars(gene_short_name), nrow = 1, scales = "free") +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.text.x = element_blank()) +
  theme(strip.text = element_text(face = "italic")) + 
  theme(legend.position = "bottom") +
  theme(legend.justification = "center") +
  theme(strip.background = element_blank()) +
  guides(fill = guide_legend(ncol = 6)) +
  labs(x = NULL, fill = NULL, color = NULL, y = "Log<sub>10</sub>(Expr+1)") +
  theme(axis.title.y = element_markdown())

# gene module heatmap ------------------------------------------
emb_highlights <- rowData(cds_embryo_aligned) %>%
  as_tibble() %>%
  filter(
    gene_short_name %in% c(
       "wnt16",
       "cdh5",
       "mmp2",
       "mrc1a",
       "wnt11",
       "wnt9b",
       "lfng",
       "pecam1",
       "igfbp2a",
       "lepr",
       "cxcl8a",
       "etv2",
       "fli1b",
       "wnt5a",
       "bmp2a",
       "bmp4",
       "pdgfrb",
       "marcksl1a",
       "flt4",
       "kdrl",
       "jag2a",
       "hgfb",
       "lyve1b",
       "meox1",
       "tgfb1a",
       "gpr182",
       "egfl7",
       "csf1a",
       "cxcl12a",
       "kitlga",
       "col1a1a",
       "fli1a",
       "angpt1",
       "drl",
       "lmo2",
       "lck",
       "btk",
       "il2rb",
       "ly6m5"
       
       
       
    )
  ) %>%
  select(module,gene_short_name) %>% 
  arrange(gene_short_name) %>%
  mutate(module_full = paste0("Module ",module)) %>%
  distinct()

# emb_highlights %>% filter(module == "3") %>% View()

col_fun_emb_modules <-
  colorRamp2(breaks = c(min(scale(agg_mat_emb)), 0, max(scale(agg_mat_emb))),
             colors = heatmap_3_colors)


#remove "Module " from agg_mat_emb rownames

agg_mat_emb_alt <- agg_mat_emb
rownames(agg_mat_emb_alt) <- str_replace(string = rownames(agg_mat_emb), pattern = "Module ", replacement = "")
colnames(agg_mat_emb_alt) <- str_replace(string = colnames(agg_mat_emb), pattern = "Partition ", replacement = "")

ht_emb_modules <- grid.grabExpr(draw(
  Heatmap(
    matrix = scale(agg_mat_emb_alt),
    col = col_fun_emb_modules,
    show_row_dend = F,
    rect_gp = gpar(col = "black", lwd = 1),
    row_names_gp = gpar(fontsize = 8),
    row_title = "Module",
    row_title_side = "right",
    row_title_gp = gpar(fontsize = 10),
    column_names_gp = gpar(fontsize = 6),
    column_names_rot = 0,
    column_names_centered = T,
    column_title = "Partition",
    column_title_side = "top",
    column_title_gp = gpar(fontsize = 10),
    column_dend_height = unit(5, "mm"),
    heatmap_legend_param = list(
      title = "Module\nScore",
      direction = "horizontal",
      title_position = "lefttop",
      title_gp = gpar(fontsize = 8),
      labels_gp = gpar(fontsize = 6)
    )
  ),
  heatmap_legend_side = "bottom"
), wrap = TRUE)

# overview umap plot-------------------------------
cxcl8_emb_scrnaseq_umap_all <- 
  bb_var_umap(
    obj = cds_embryo_aligned, 
    var = "partition", 
    overwrite_labels = T,
    group_label_size = 3,
    foreground_alpha = 0.6,
    rasterize = TRUE,
    raster_dpi = 300)

# zoom in on cxcl12+ cluster and plot aggregate proliferative marker expression--------------------------------------------
cxcl8_emb_prolif_markers <-
  rowData(cds_embryo_aligned) %>%
  as_tibble() %>%
  filter(gene_short_name %in% c("cdk1", "mki67", "cenpf", "myca")) %>%
  select(id) %>%
  mutate(group = "Proliferative Markers")

cxcl8_emb_proliferative_umap <-
  bb_gene_umap(
    obj = cds_embryo_aligned[, colData(cds_embryo_aligned)$partition_assignment == "MSC"],
    gene_or_genes = cxcl8_emb_prolif_markers
  ) +
  theme_cowplot(font_size = 8) + 
  theme(strip.text = element_blank()) +
  labs(title = "Proliferative Markers", color = NULL) +
  theme(plot.title = element_text(face = "plain", hjust = 0.5)) +
  theme(legend.position = c(0.6, 0.1)) +
  theme(legend.direction = "horizontal") +
  guides(color = guide_colorbar(barheight = 0.5))

cxcl8_emb_cxcl12a_umap <-
  bb_gene_umap(
    obj = cds_embryo_aligned[, colData(cds_embryo_aligned)$partition_assignment == "MSC"],
    gene_or_genes = "cxcl12a"
  ) + 
  theme_cowplot(font_size = 8) +
  theme(strip.text = element_blank()) +
  labs(title = "cxcl12a", color = NULL) +
  theme(plot.title = element_text(face = "italic", hjust = 0.5)) +
  theme(legend.position = c(0.6, 0.1)) +
  theme(legend.direction = "horizontal") +
  guides(color = guide_colorbar(barheight = 0.5)) +
  scale_color_viridis_c(limits = c(0, 2.2), breaks = c(0, 1.0, 2.0), na.value = "grey80")

emb_lepr_expression_umap <- 
  bb_gene_umap(obj = cds_embryo_aligned[, colData(cds_embryo_aligned)$niche_or_not == "niche"], 
               gene_or_genes = "lepr", max_expr_val = 0.65) +
  theme_cowplot(font_size = 8) + 
  theme(strip.text = element_blank()) + 
  labs(title = "lepr", color = NULL) +
  theme(strip.text = element_blank()) +
  theme(plot.title = element_text(face = "italic", hjust = 0.5)) +
  theme(legend.position = c(0.6, 0.1)) +
  theme(legend.direction = "horizontal") +
  guides(color = guide_colorbar(barheight = 0.5)) +
  scale_y_continuous(breaks = c(-6, -3, 0, 3))
emb_lepr_expression_umap 

# col1a1a violin plot and gene umap----------------------------------------------
emb_col1a1a_violin <- 
  bb_gene_violinplot(
  cds = cds_embryo_aligned[, colData(cds_embryo_aligned)$niche_or_not == "niche"],
  variable = "partition_assignment",
  genes_to_plot = "col1a1a",
  palette = experimental_group_palette[c("lepr+ MSC", "MSC", "osteoblast", "sinusoidal")],
  include_jitter = T,violin_alpha = 0.4, jitter_alpha = 0.4
) +
  theme_cowplot(font_size = 8) + 
  theme(strip.text = element_blank()) +
  labs(title = "col1a1a") +
  theme(plot.title = element_text(face = "italic", hjust = 0.5)) +
  theme(legend.position = "none")

emb_col1a1a_umap <- 
  bb_gene_umap(obj = cds_embryo_aligned[, colData(cds_embryo_aligned)$niche_or_not == "niche"], 
               gene_or_genes = "col1a1a") +
  theme_cowplot(font_size = 8) + 
  theme(strip.text = element_blank()) + 
  labs(title = "col1a1a", color = NULL) +
  theme(strip.text = element_blank()) +
  theme(plot.title = element_text(face = "italic", hjust = 0.5)) +
  theme(legend.position = c(0.6, 0.1)) +
  theme(legend.direction = "horizontal") +
  guides(color = guide_colorbar(barheight = 0.5)) +
  scale_y_continuous(breaks = c(-6, -3, 0, 3))

emb_msc_raw_sublcuster_umap <-
  bb_var_umap(
    obj = cds_embryo_aligned[, colData(cds_embryo_aligned)$partition_assignment == "MSC"],
    var = "louvain",
    overwrite_labels = T,
    foreground_alpha = 0.6,
    cell_size = 1
  )

# revision:  prkcd gene signature gsea ------------------------------------

zf_human_fc <- sinusoidal_pseudobulk_res$Result |>
  select(`ZFIN Symbol` = gene_short_name, log2FoldChange) |> 
  left_join(zf_human_orthos) |> 
  select(`Gene ID`, log2FoldChange) |> 
  distinct() |> 
  filter(!is.na(`Gene ID`)) |>
  group_by(`Gene ID`) |> 
  mutate(n = n()) |> 
  filter(n == 1) |> 
  select(-n) |> 
  deframe()


prkcd_target_genes$prkcd_targets <- unique(prkcd_target_genes$prkcd_targets)

prkcd_enrichment_plot <- fgsea::plotEnrichment(pathway = prkcd_target_genes$prkcd_targets, 
                      stats = zf_human_fc) + 
  labs(title = "PKC-\u03B4 Transcriptional Targets", 
       x = "Genes ranked by<br />Log<sub>2</sub>(cxcl8/control)") +
  theme(axis.title.x = element_markdown(size = 10)) +
  theme(plot.title = element_text(hjust = 0.5, size = 10))
  
# fgsea::fgsea(pathways = prkcd_target_genes, stats = zf_human_fc)
