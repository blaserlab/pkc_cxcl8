colData(cds_pmm_final)$label_new <- recode(colData(cds_pmm_final)$class, "mcs" = "control")

pmm_heme_labeldf <- 
  tribble(
    ~text_x, ~text_y, ~label,
    -4.5,6,"Progenitor 1",
    -3,8,"Progenitor 2",
    3.5,-6,"Erythroid",
    7.5,9,"Pro-neutrophil",
    10,0,"Neutrophil 1",
    5,6,"Neutrophil 2",
    4,-1,"Proliferative"
  )

pmm_heme_varplot <- bb_var_umap(
  cds = cds_pmm_final[, colData(cds_pmm_final)$cluster_assignment %in% c(
    "Proliferative",
    "Erythroid",
    "Progenitor 1",
    "Progenitor 2",
    "Neutrophil 1",
    "Neutrophil 2",
    "Pro-neutrophil"
  )],
  var = "cluster_assignment",
  palette = brewer.pal(n = 7, name = "Dark2"),
  foreground_alpha = 0.1,
  cell_size = 1,
  man_text_df = pmm_heme_labeldf
)  

pmm_heme_faceted_densityplot <- bb_var_umap(
  cds = cds_pmm_final[, colData(cds_pmm_final)$cluster_assignment %in% c(
    "Proliferative",
    "Erythroid",
    "Progenitor 1",
    "Progenitor 2",
    "Neutrophil 1",
    "Neutrophil 2",
    "Pro-neutrophil"
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
prkcda_scrnaseq_cluster_membership_plot <- 
  ggplot(
  data = cluster_proportions_pmm %>% mutate(label_new = recode(enriched, "mcs" = "control")),
  mapping = aes(x = cluster_assignment, y = log2fold_change_over_mcs, fill = label_new)
) + 
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = experimental_group_palette, limits = c("control", "prkcda")) +
  labs(x = NULL, y = "\nLog<sub>2</sub>(prkcda/control)", fill = "Enriched:") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  theme(axis.title.y = element_markdown()) +
  theme(legend.position = "top") +
  theme(legend.justification = "left") +
  geom_text(mapping  = aes(x = cluster_assignment,
                           y = texty,
                           label = p.signif),
            nudge_y = 0.5,
            show.legend = F, size = 3) +
  guides(fill = guide_legend(title.theme = element_text(size = 9)))
prkcda_scrnaseq_cluster_membership_plot

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
  "csf1rb",
  "gata2a",
  "gata1a",
  "gpr84",
  "spi1b",
  "mpx",
  "mki67"
)

pmm_gene_dotplot <- bb_gene_dotplot(
    cds = cds_pmm_final[, colData(cds_pmm_final)$cluster_binary_type == "heme"],
    markers = pmm_dotplot_markers,
    group_cells_by = "cluster_assignment", 
    colorscale_name = "Expression", 
    sizescale_name = "Fraction\nExpressing", 
    max.size = 5, 
    group_ordering = c("Progenitor 1",
                       "Progenitor 2",
                       "Erythroid",
                       "Pro-neutrophil",
                       "Neutrophil 1",
                       "Neutrophil 2",
                       "Proliferative"),
    gene_ordering = pmm_dotplot_markers
  
) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  theme(legend.box = "vertical") +
  labs(x = NULL, y = NULL) + 
  guides(size = guide_legend(title.theme = element_text(size = 9)),
         color = guide_colorbar(title.theme = element_text(size = 9)))



# shared barcodes-----------------------------------------------------

scgestalt_barcode_sharing_plot <- colData(cds_pmm_final) %>%
  as_tibble(rownames = "barcode_sample") %>%
  filter(!is.na(fishname)) %>%
  filter(cluster_binary_type == "heme") %>%
  filter(fishname == "prkcda fish 8") %>%
  # group_by(gestalt_mapping) %>%
  # mutate(n = n()) %>%
  mutate(cluster_assignment = factor(
    cluster_assignment,
    levels = c(
      "Progenitor 1",
      "Progenitor 2",
      "Erythroid",
      "Pro-neutrophil",
      "Neutrophil 1",
      "Neutrophil 2",
      "Proliferative"
    )
  )) %>%
  mutate(gestalt_mapping = fct_infreq(factor(gestalt_mapping), ordered = T)) %>%
  arrange(gestalt_mapping, cluster_assignment) %>%
  mutate(barcode_sample = fct_rev(fct_inorder(factor(barcode_sample)))) %>%
  group_by(fishname) %>%
  mutate(gestalt_rank = dense_rank(gestalt_mapping)) %>%
  mutate(fillcolor = brewer.pal(n = 8, name = "Dark2")[gestalt_rank]) %>%
  mutate(cellnum = row_number(barcode_sample)) %>%
  ggplot(mapping = aes(x = cluster_assignment, y = cellnum, fill = fillcolor)) +
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
gestalt_fishname_umaps <- bb_var_umap(cds_pmm_final[,colData(cds_pmm_final)$cluster_binary_type == "heme"], 
                     var = "fishname_new", 
                     legend_pos = "right", 
                     cell_size = 1,
                     value_to_highlight = as_tibble(colData(cds_pmm_final)) %>% 
                       filter(!is.na(fishname_new)) %>% 
                       pull(fishname_new) %>% 
                       unique(), 
                     palette = "rcolorbrewer",foreground_alpha = 0.6) +
  facet_wrap(facets = vars(label_new)) +
  theme(strip.background = element_blank())
gestalt_fishname_umaps
# top line umap plot to show clusters---------------------------------------------

prkcda_scrnaseq_umap_cluster <-
  bb_var_umap(
    cds_pmm_final,
    var = "cluster_assignment",# leiden clusters
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

# gene module heatmap####-----------------------------------------------------------------------------

pmm_highlights <- c(
  "hoxb8a",
  "clcnk",
  "cldnh",
  "slc13a3",
  "slc26a1",
  "slc5a12",
  "crestin",
  "hps4",
  "hps1",
  "cdh5",
  "kdrl",
  "egfl7",
  "ptges",
  "lfng",
  "pdgfra",
  "krt17",
  "krt97",
  "krt96",
  "dnaaf1",
  "tube1",
  "kif17",
  "gfi1b",
  "gata1a",
  "tal1",
  "ccr9a",
  "il21",
  "gata2a",
  "spi1b",
  "cebpa",
  "lyz"
)


col_fun_pmm_modules <-
  colorRamp2(breaks = c(min(scale(agg_mat_pmm)), 0, max(scale(agg_mat_pmm))),
             colors = heatmap_3_colors)

align_to <- as.list(rowData(cds_pmm_final) %>%
                      as_tibble() %>%
                      filter(!is.na(module)) %>%
                      filter(gene_short_name %in% pmm_highlights)%>% 
                      pull(module) %>% 
                      unique() %>% 
                      as.numeric())
names(align_to) <- rowData(cds_pmm_final) %>%
  as_tibble() %>%
  filter(!is.na(module)) %>%
  filter(gene_short_name %in% pmm_highlights) %>%
  mutate(module_full = paste0("Module ", module)) %>%
  pull(module_full) %>%
  unique()


labels <-
  lapply(names(align_to), function(nm) {
    ls <- 
      rowData(cds_pmm_final) %>%
      as_tibble() %>%
      filter(!is.na(module)) %>%
      filter(gene_short_name %in% pmm_highlights) %>%
      mutate(module_full = paste0("Module ", module)) %>%
      filter(module_full == nm) %>%
      pull(gene_short_name)
    label <- text_grob(
      label = paste(ls,
                    collapse = "\n"),
      size = rep(6, times = length(ls)),
      color = rep("black", times = length(ls)),
    )
  })
names(labels) <- names(align_to)

margin_pmm <- unit(0,"pt")
label_h <- lapply(labels, function(x) convertHeight(grobHeight(x),"cm")*1 + margin_pmm)
label_h <- do.call(unit.c, label_h)

label_w <- lapply(labels, function(x) convertWidth(grobWidth(x),"cm"))
label_w <- do.call(unit.c, label_w)
label_w <- max(label_w) + margin_pmm

panel_fun = function(index, nm) {
  grid.rect(gp = gpar(fill = "grey90", col = NA))
  grid.lines(c(1, 0, 0, 1),
             c(0, 0, 1, 1),
             gp = gpar(col = "black"),
             default.units = "npc")
  label <- labels[[nm]]
  pushViewport(viewport(
    # x = margin_pmm/2,
    # y = margin_pmm/2,
    width = grobWidth(label),
    height = grobHeight(label),
    just = "center"
  ))
  grid.draw(label)
  popViewport()
}

pmm_anno = anno_link(align_to = align_to,
                     which = "row",
                     panel_fun = panel_fun,
                     size = label_h,
                     gap = unit(3, "pt"),
                     width = label_w + unit(10,"mm"),# 5mm for the link
                     side = "left",
                     link_gp = gpar(fill = "grey90", col = "black"),
                     internal_line = F)

ht_pmm_module_heatmap <- grid.grabExpr(draw(Heatmap(
  matrix = scale(agg_mat_pmm),
  col = col_fun_pmm_modules,
  show_row_dend = F,
  left_annotation = rowAnnotation(foo = pmm_anno),
  rect_gp = gpar(col = "black", lwd = 1),
  heatmap_legend_param = list(title = "Module\nScore"),
  column_title = "Cluster",
  column_title_side = "bottom",
  column_dend_height = unit(5, "mm"),
  column_names_rot = 30,
  column_names_gp = gpar(fontsize = 8),
  row_names_gp = gpar(fontsize = 8)
)), wrap = TRUE)



# generate the data for making the violin plots; see fig 3 stagign for the plot
muench_plot_data <- plot_cells(cds_pmm_final, genes = muench_genes_to_plot)[["data"]] %>% as_tibble()

muench_violin_plot <- ggplot(data = muench_plot_data %>% filter(cluster_binary_type == "heme"),
       mapping = aes(x = cluster_assignment,
                     y = value, 
                     fill= value))+
  geom_jitter(shape = 21, fill = "transparent", color = "grey80", alpha = 0.6, size = 0.5, stroke = 0.25)+
  geom_violin(scale = "area", color = "black", alpha = 0.4) +
  facet_wrap(facets = vars(feature_label),scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(strip.background = element_blank()) +
  labs(x = NULL, y = "Aggregate Expression")

# generate the data for making the violin plots; see fig 3 stagign for the plot
tusi_plot_data <- plot_cells(cds_pmm_final, genes = tusi_genes_to_plot)[["data"]] %>% as_tibble()

tusi_violin_plot <- ggplot(data = tusi_plot_data %>% filter(cluster_binary_type == "heme"),
       mapping = aes(x = cluster_assignment,
                     y = value, 
                     fill= value))+
  geom_jitter(shape = 21, fill = "transparent", color = "grey80", alpha = 0.6, size = 0.5, stroke = 0.25)+
  geom_violin(scale = "area", color = "black", alpha = 0.4) +
  facet_wrap(facets = vars(feature_label),scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(strip.background = element_blank()) +
  labs(x = NULL, y = "Aggregate Expression")

# generate the data for making the violin plots; see fig 3 stagign for the plot
tang_plot_data <- plot_cells(cds_pmm_final, genes = tang_genes_to_plot)[["data"]] %>% as_tibble()

tang_violin_plot <- ggplot(data = tang_plot_data %>% filter(cluster_binary_type == "heme"),
                           mapping = aes(x = cluster_assignment,
                                         y = value, 
                                         fill= value))+
  geom_jitter(shape = 21, fill = "transparent", color = "grey80", alpha = 0.6, size = 0.5, stroke = 0.25)+
  geom_violin(scale = "area", color = "black", alpha = 0.4) +
  facet_wrap(facets = vars(feature_label),scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(strip.background = element_blank()) +
  labs(x = "Zebrafish scRNA-seq Cluster", y = "Aggregate Expression")

