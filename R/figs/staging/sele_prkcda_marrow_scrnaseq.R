
pmm_heme_labeldf <- 
  tribble(
    ~text_x, ~text_y, ~label,
    -3, 6,"Myeloid",
    -3, 8,"Lymphoid",
    3.5,-6,"Erythroid",
    7.5,9,"Pro-neutrophil",
    10,0.5,"Neutrophil 1",
    5,6,"Neutrophil 2",
    4,1.75,"Prolif",
    1,-0.5, "HSC/Thr"
  )

pmm_heme_varplot <- bb_var_umap(
  obj = cds_pmm_final[, colData(cds_pmm_final)$revision_cluster_assignment %in% c(
    "Proliferative",
    "Erythroid",
    "Myeloid",
    "Lymphoid",
    "Neutrophil 1",
    "Neutrophil 2",
    "Pro-neutrophil",
    "HSC/Thr"
  )],
  var = "cluster_assignment",
  palette = brewer.pal(n = 8, name = "Dark2"),
  foreground_alpha = 0.1,
  cell_size = 1,
  man_text_df = pmm_heme_labeldf
)  

pmm_heme_faceted_densityplot <- bb_var_umap(
  obj = cds_pmm_final[, colData(cds_pmm_final)$revision_cluster_assignment %in% c(
    "Proliferative",
    "Erythroid",
    "Lymphoid",
    "Myeloid",
    "Neutrophil 1",
    "Neutrophil 2",
    "Pro-neutrophil",
    "HSC/Thr"
  )],
  var = "density",
  sample_equally = F,
  foreground_alpha = 0.6,
  facet_by = "label_new", 
  overwrite_labels = F, 
  cell_size = 1
)  + 
  labs(color = "Cell\nDensity") +
  guides(color = guide_colorbar(title.theme = element_text(size = 9)))

# barplot for cluster membership...maybe make this a heatmap???####-------------------------------------------------------------
prkcda_scrnaseq_cluster_membership_plot <- bb_cluster_representation(cds =  cds_pmm_final, 
                          cluster_var = "cluster", 
                          class_var = "label_new", 
                          experimental_class = "prkcda", 
                          control_class = "control", 
                          return_value = "table") |> 
  left_join(bb_cellmeta(cds_pmm_final) |> group_by(cluster, revision_cluster_assignment) |> summarise()) |> 
  filter(revision_cluster_assignment %in% c("Myeloid", "Lymphoid", "Erythroid", "Pro-neutrophil", "Neutrophil 1", "Neutrophil 2", "Proliferative", "HSC/Thr")) |> ggplot(mapping = aes(x = revision_cluster_assignment, y = log2fold_change_over_control, fill = enriched)) + 
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = experimental_group_palette, limits = c("control", "prkcda")) +
  labs(x = NULL, y = "\nLog<sub>2</sub>(prkcda/control)", fill = "Enriched:") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  theme(axis.title.y = element_markdown()) +
  theme(legend.position = "top") +
  theme(legend.justification = "left") +
  geom_text(mapping  = aes(x = revision_cluster_assignment,
                           y = texty,
                           label = p.signif),
            nudge_y = 0.5,
            show.legend = F, size = 3) +
  guides(fill = guide_legend(title.theme = element_text(size = 9)))


# heatmap of aggregate score of genesets-----------------------------


col_fun_aggretate_genesets <- colorRamp2(breaks = c(min(t(scale(agg_mat_heme_genesets))),
                                                    0,
                                                    max(t(scale(agg_mat_heme_genesets)))),
                                         colors = heatmap_3_colors)
# annotation table for the heatmap
agg_score_anno_tbl <- left_join(tibble(gene_set = colnames(t(agg_mat_heme_genesets))),
          bind_rows(
		    list(
			 tibble(gene_set = names(muench_gene_sets), gene_set_origin = "Mouse, Muench et al."), # these define the annotation labels
			 tibble(gene_set = names(tusi_gene_sets), gene_set_origin = "Mouse, Tusi et al."),
               		 tibble(gene_set = names(zf_gene_sets), gene_set_origin = "Zebrafish, Tang et al."))))
# convert to df
agg_score_anno_df <- data.frame(Origin = agg_score_anno_tbl$gene_set_origin, row.names = agg_score_anno_tbl$gene_set)

# make the annotation object
agg_score_ha <- HeatmapAnnotation(df = agg_score_anno_df, 
			     col = list(Origin = c("Mouse, Muench et al." = "#E78AC3", 
						   "Mouse, Tusi et al." = "#A6D854", 
						   "Zebrafish, Tang et al." = "#FFD92F")),
			     show_annotation_name = F,
			     gp = gpar(col = "white"),
			     show_legend = F)

lgd1 <- Legend(
  col_fun = col_fun_aggretate_genesets,
  title = "Aggregate\nScore",
  direction = "horizontal",
  labels_gp = gpar(fontsize = 8),
  title_gp = gpar(fontface = "plain", fontsize = 9)
)

lgd2 <-
  Legend(
    at = agg_score_ha@anno_list$Origin@color_mapping@levels,
    legend_gp = gpar(fill = agg_score_ha@anno_list$Origin@color_mapping@colors),
    title = "Gene Set Reference",
    labels_gp = gpar(fontsize = 8),
    title_gp = gpar(fontface = "plain", fontsize = 9)
  )

pd <- packLegend(lgd1, lgd2, gap = unit(2, "mm"))

ht_agg_mat_heme_genesets <- Heatmap(
  matrix = t(scale(agg_mat_heme_genesets)),
  col = col_fun_aggretate_genesets,
  show_heatmap_legend = F,
  show_row_names = T,
  row_dend_width = unit(5, "mm"),
  column_dend_height = unit(5, "mm"),
  row_names_gp = gpar(fontsize = 9),
  column_names_gp = gpar(fontsize = 9),
  top_annotation = agg_score_ha,
  row_dend_gp = gpar(lwd = 0.5),
  column_dend_gp = gpar(lwd = 0.5),
  column_names_rot = 30
  
)


agg_score_heatmap <- grid.grabExpr(draw(ht_agg_mat_heme_genesets, heatmap_legend_list = pd),
                                   wrap = TRUE)
# plot_grid(agg_score_heatmap)

# canonical gene dotplot-------------------------------------------------------------------
pmm_dotplot_markers <- c(
  "tal1",
  "lmo2",
  "meis1b",
  "pbx1b",
  "mpl",
  "itga2b",
  "runx1",
  "csf1rb",
  "gata2a",
  "gata2b",
  "gata1a",
  "ccr9a",# ref: PMID: 31511326, Baron cell 2019, https://doi.org/10.1038/srep44145, 10.1182/blood-2009-08-237784 
  "coro1a",# coro1a+ lyz- are T progenitors and monocytes, ref:  https://doi.org/10.4049/jimmunol.1901494
  "cd81a",# https://doi.org/10.1159/000075685, PMID: 1695320
  "gpr84",
  "spi1b",
  "lyz",
  "mpx",
  "mki67"
)

pmm_gene_dotplot <- bb_genebubbles(
  obj = cds_pmm_final,
  genes = pmm_dotplot_markers,
  cell_grouping = "revision_cluster_assignment",
  scale_expr = T,
  return_value = "data",
  gene_ordering = "as_supplied"
) |>
  filter(
    revision_cluster_assignment %in% c(
      "HSC/Thr",
      "Myeloid",
      "Lymphoid",
      "Erythroid",
      "Pro-neutrophil",
      "Neutrophil 1",
      "Neutrophil 2",
      "Proliferative"
    )
  ) |>
  ggplot(
    aes(
      x = gene_short_name,
      y = revision_cluster_assignment,
      size = proportion,
      color = expression
    )
  ) +
  geom_point() +
  scale_size(range = c(1,5)) +
  scale_color_viridis_c() +
  labs(x = NULL,
       y = NULL,
       size = "Fraction\nExpressing",
       color = "Expression") +
  guides(
    size = guide_legend(title.theme = element_text(size = 9)),
    color = guide_colorbar(title.theme = element_text(size = 9))
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))



# shared barcodes-----------------------------------------------------

scgestalt_barcode_sharing_plot <- colData(cds_pmm_final) %>%
  as_tibble(rownames = "barcode_sample") %>%
  filter(!is.na(fishname)) %>%
  filter(
    revision_cluster_assignment %in% c(
      "HSC/Thr",
      "Myeloid",
      "Lymphoid",
      "Erythroid",
      "Pro-neutrophil",
      "Neutrophil 1",
      "Neutrophil 2",
      "Proliferative"
    )
  ) |>
  filter(fishname == "prkcda fish 8") %>% 
  mutate(gestalt_mapping = fct_infreq(factor(gestalt_mapping), ordered = T)) %>%
  arrange(gestalt_mapping, revision_cluster_assignment) %>%
  mutate(barcode_sample = fct_rev(fct_inorder(factor(barcode_sample)))) %>%
  group_by(fishname) %>%
  mutate(gestalt_rank = dense_rank(gestalt_mapping)) %>%
  mutate(fillcolor = brewer.pal(n = 8, name = "Dark2")[gestalt_rank]) %>%
  mutate(cellnum = row_number(barcode_sample)) %>%
  ggplot(mapping = aes(x = revision_cluster_assignment, y = cellnum, fill = fillcolor)) +
  geom_tile() +
  scale_fill_identity() +
  theme(legend.position = "none") +
  facet_wrap(facets = vars(fishname), scales = "free_y") +
  theme(strip.background = element_blank()) +
  labs(x = NULL, y = "Cells") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# fishname umaps----------------------------------------------------------------------
colData(cds_pmm_final)$fishname_new <- str_replace(colData(cds_pmm_final)$fishname, "mcs", "control")
colData(cds_pmm_final)$fishname_new <- factor(colData(cds_pmm_final)$fishname_new, levels = c(
  "control fish 1", 
  "control fish 2",
  "control fish 17",
  "control fish 18",
  "prkcda fish 1",
  "prkcda fish 2",
  "prkcda fish 5",
  "prkcda fish 8",
  "prkcda fish 10",
  "prkcda fish 20",
  "prkcda fish 21",
  "prkcda fish 22"
))

gestalt_fishname_umaps <-
  bb_var_umap(
    filter_cds(
      cds = cds_pmm_final,
      cells = bb_cellmeta(cds_pmm_final) |> filter(
        revision_cluster_assignment %in% c(
          "HSC/Thr",
          "Myeloid",
          "Lymphoid",
          "Erythroid",
          "Pro-neutrophil",
          "Neutrophil 1",
          "Neutrophil 2",
          "Proliferative"
        )
      )
    ),
    var = "fishname_new",
    legend_pos = "right",
    cell_size = 1,
    value_to_highlight = as_tibble(colData(cds_pmm_final)) %>%
      filter(!is.na(fishname_new)) %>%
      pull(fishname_new) %>%
      unique(),
    palette = "rcolorbrewer",
    foreground_alpha = 0.6
  ) +
  facet_wrap(facets = vars(label_new)) +
  theme(strip.background = element_blank())


# top line umap plot to show clusters---------------------------------------------

prkcda_scrnaseq_umap_cluster <-
  bb_var_umap(
    cds_pmm_final,
    var = "revision_cluster_assignment",# leiden clusters
    overwrite_labels = T,
    legend_pos = "none",
    cell_size = 0.5,
    foreground_alpha = 0.3,
    group_label_size = 3
    
  )

# heatmap from muench data do define cell and gene clusters ####----------------------------------------------------
# make the color function for the heatmap
col_fun_muench <-
  colorRamp2(breaks = c(0, max(scale(muench_matrix))),
             colors = c("grey90", "red"))

# make the annotation object
muench_ha <- HeatmapAnnotation(`Gene Cluster` = 
                                 anno_block(labels = names(muench_col_order),
                                            show_name = F, 
                                            height = unit(3, "mm"),
                                            labels_gp = gpar(fontsize = 8)))

# draw the heatmap using the same random seed that was used to define the clusters
set.seed(123)
muench_heatmap <- grid.grabExpr(draw(Heatmap(
  matrix = scale(muench_matrix),
  col = col_fun_muench, 
  top_annotation = muench_ha,
  column_km = 9, 
  row_km = 8, 
  show_column_names = F,
  name = "Expression",row_title = NULL,
  column_title = "Analysis of Muench et al., 2020 Data",
  column_title_gp = gpar(fontsize = 10),
  row_dend_gp = gpar(lwd = 0.5),
  row_dend_width = unit(5, "mm"),
  column_dend_gp = gpar(lwd = 0.5),
  column_dend_height = unit(5, "mm"),
  column_names_gp = gpar(fontsize = 10),
  row_names_gp = gpar(fontsize = 8),
  heatmap_legend_param = list(direction = "horizontal", 
                              title_position = "lefttop",
                              title_gp = gpar(fontsize = 8),
                              labels_gp = gpar(fontsize = 6))
  
),heatmap_legend_side = "bottom", ), wrap=TRUE)

