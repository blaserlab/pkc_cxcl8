# dotplot to show expression of pkc isoforms ------------------------------------------
pmm_pkc_dotplot <- bb_gene_dotplot(
  cds = cds_pmm_final[,colData(cds_pmm_final)$cluster_assignment %in% c("Progenitor 1", "Progenitor 2", "Erythroid", "Pro-neutrophil", "Neutrophil 1", "Neutrophil 2")],
                group_cells_by = "cluster_assignment", 
                markers = rowData(cds_pmm_final) %>% 
                  as_tibble() %>% 
                  filter(gene_short_name %in% c("prkcaa", "prkcba", "prkcbb", "prkcda", "prkcdb", "prkcea", "prkceb", "prkcg", "prkcha", "prkchb", "prkci", "prkcz")) %>% 
                  arrange(gene_short_name) %>% 
                  pull(gene_short_name),
                max.size = 4,
  colorscale_name = "Expression", sizescale_name = "Fraction\nExpressing") +
  guides(color = guide_colorbar(title.theme = element_text(size = 9)),
         size = guide_legend(title.theme = element_text(size = 9))) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))  +
  theme(legend.key.height = unit(3.5, "mm")) +
  labs(x = NULL, y = NULL) 

# scatac umap ----------------------------------------------------------------

zf_scatac_umap <- bb_var_umap(zf_cds, 
                              "leiden_assignment", 
                              overwrite_labels = T, 
                              group_label_size = 3, 
                              cell_size = 1, 
                              palette = experimental_group_palette_2, 
                              foreground_alpha = 0.4)

# tracks and connections plots------------------------------------------------------
# replace the local network fragment file with the copy supplied in extdata.
new_frag_path <- system.file("extdata", "fragments.tsv.gz", package = "pkc.cxcl8.datapkg")
new_frag <- Signac::CreateFragmentObject(
  path = new_frag_path,
  cells = colnames(zf),
  validate.fragments = TRUE
)
Fragments(zf) <- NULL
Fragments(zf) <- new_frag

DefaultAssay(zf) <- "peaks"
# relevel leiden clusters
zf@meta.data$leiden_assignment <- fct_relevel(zf@meta.data$leiden_assignment, c("Progenitor 1", "Progenitor 2", "Neutrophil", "Erythroid", "Renal"))
Idents(zf) <- "leiden_assignment"

scatac_plot_config <- tribble(
  ~gene,~show_links,~ext_up,~ext_down,
  "prkcda",TRUE,5000,1000
)


scatac_plotlist <- pmap(
  .l = list(
    gene = scatac_plot_config$gene,
    show_links = scatac_plot_config$show_links,
    ext_up = scatac_plot_config$ext_up,
    ext_down = scatac_plot_config$ext_down
  ),
  .f = function(gene, show_links, ext_up, ext_down, coords = grcz11_gene_coords, obj = zf, pal = experimental_group_palette_2) {
    reg <- coords %>% filter(`Gene name` == gene) %>% pull(region)
    plot <- Signac::CoveragePlot(
      object = obj,
      region = reg,
      extend.upstream = ext_up,
      extend.downstream = ext_down,
      links = show_links)
    plot <- plot & scale_fill_manual(values = pal)


  }
) %>%
  set_names(scatac_plot_config$gene)

scatac_features <- Signac::granges(zf)
GenomicRanges::mcols(scatac_features)$width <- width(scatac_features)
GenomicRanges::mcols(scatac_features)$type <- "Peaks"

scatac_trace <- bb_makeTrace(
  trace_data = scatac_plotlist$prkcda[[1]][[1]][["data"]],
  features = scatac_features,
  links = Links(zf),
  startcol = "position",
  endcol = "position",
  genome = "danRer11",
  seqname = "chr6",
  plot_range = "trace"
)


theme_shrinky <- function(size) {
  theme(axis.title.y = element_text(size = size)) +
  theme(strip.text.y.left = element_text(size = size))
}

p1 <-
  bb_plot_trace_data(scatac_trace,
                     yvar = "coverage",
                     facet_var = "group",
                     colvar = "group") + 
  scale_color_manual(values = experimental_group_palette_2) + 
  labs(y = "Accessibility") + 
  scale_y_continuous(breaks = c(0, 20)) +
  theme_shrinky(size = 8) + 
  theme(axis.text.y.left = element_text(size = 6))

p2 <- bb_plot_trace_model(scatac_trace) + 
  theme_shrinky(size = 8) 

p3 <- bb_plot_trace_feature(scatac_trace, type_to_plot = "Peaks") +
  theme_shrinky(size = 8)

p4 <-
  bb_plot_trace_links(scatac_trace) + scale_color_viridis_c(
    breaks = c(0.5, 0.6, 0.7),
    option = "F",
    direction = -1,
    end = 0.8 ,
    guide = guide_colorbar(title.position = "top", 
                           barheight = 0.5, 
                           title.theme = element_text(size = 8), 
                           label.theme = element_text(size = 6))
  ) +
  theme(legend.direction = "horizontal",
        legend.position = c(0.75, 0.5)) + 
  labs(color = "Link Score") +
  theme_shrinky(size = 8)

p5 <- bb_plot_trace_axis(scatac_trace, xtitle = "Chr. 6") +
  theme(axis.text.x = element_text(size = 8))


# motif activity plot----------------------------------------------------

tf_features_toplot <- c("CEBPD", 
                        "CEBPA",
                        "GATA1::TAL1", 
                        "SPIB", 
                        "IKZF1", 
                        "ZNF148", 
                        "EGR1", 
                        "KLF9")
DefaultAssay(zf) <- "chromvar"
tf_feature_plots <- 
  map(.x = tf_features_toplot, 
      .f = function(x, obj = zf, lookup = motif_lookup) {
        subt <- paste0("*", x, "*")
        p <- FeaturePlot(
          object = obj,
          features = lookup %>% filter(motif_alt_id == x) %>% pull(motif_id),
          min.cutoff = "q10",
          max.cutoff = "q90",
          pt.size = 0.1,
          reduction = "monocle_umap"
        ) +
          theme_cowplot(font_size = main_fontsize) +
          labs(subtitle = subt, title = NULL, x = "UMAP 1", y = "UMAP 2") +
          theme(plot.subtitle = element_markdown(hjust = 0.5))
          
      }) %>% 
  set_names(tf_features_toplot)
tf_feature_plots$CEBPD
tf_feature_plots$`GATA1::TAL1`
tf_feature_plots$SPIB
tf_feature_plots$IKZF1
tf_feature_plots$ZNF148
tf_feature_plots$EGR1
tf_feature_plots$KLF9

# gene_activity heatmap------------------------------------------------------
# aggregate top marker gene activity by leiden assignment
ga_hm_mat <- scale(t(as.matrix(aggregate_gene_expression(cds = zf_cds_ga[rowData(zf_cds_ga)$gene_short_name %in% zf_cds_ga_tm$gene_id,], 
                          cell_group_df = data.frame(cell = rownames(colData(zf_cds_ga)), 
                                                     cell_grouping = colData(zf_cds_ga)$leiden_assignment)))))


col_fun_ga_hm_mat <- 
  colorRamp2(breaks = c(min(ga_hm_mat), 0, max(ga_hm_mat)), colors = heatmap_3_colors)
  
ga_highlights <-
  c(
    "runx1",
    "prkcda",
    "myb",
    "sptb",
    "gfi1b",
    "hbba2",
    "csf3r",
    "npsn",
    "meis1b",
    "spi1b",
    "cldn7b"
  )
ga_gene_anno_df <-
  map(
    .x = ga_highlights,
    .f = function(x) {
      index <- which(colnames(ga_hm_mat) == x)
      return(index)
    }
  ) %>% set_names(ga_highlights) %>%
  bind_cols() %>%
  pivot_longer(everything()) %>%
  as.data.frame()

ga_gene_anno <- HeatmapAnnotation(
  foo = anno_mark(
    at = ga_gene_anno_df$value,
    labels = ga_gene_anno_df$name,
    labels_gp = gpar(fontsize = 8),padding = 1.5,labels_rot = 45
  ),
  which = "column"
)

ga_heatmap <- grid.grabExpr(draw(
  Heatmap(
    matrix = ga_hm_mat,
    col = col_fun_ga_hm_mat,
    name = "Aggregate\nGene Activity\nScore",
    heatmap_legend_param = list(
      title_gp = gpar(fontface = "plain", fontsize = 9),
      grid_width = unit(0.14, "in"),
      labels_gp = gpar(fontsize = 8)
    ),
    row_dend_width = unit(5, "mm"),
    column_dend_height = unit(5, "mm"),
    column_dend_side = "bottom",
    show_row_names = T,
    row_names_gp = gpar(fontsize = 9),
    show_column_names = F,
    top_annotation = ga_gene_anno,
    row_dend_gp = gpar(lwd = 0.5), 
    column_dend_gp = gpar(lwd = 0.5)
  )
), wrap = T)
plot_grid(ga_heatmap)

# Plot TSS enrichment---------------------------------------------------
DefaultAssay(zf) <- 'peaks'
Idents(zf) <- "leiden_assignment"
tss_plot <- TSSPlot(zf, group.by = "leiden_assignment") +
  facet_wrap(~factor(group, levels = c("Progenitor 1", "Progenitor 2", "Neutrophil", "Erythroid", "Renal")), nrow = 1) +
  theme_cowplot(font_size = main_fontsize) +
  labs(title = NULL, y = "Mean TSS\nEnrichment Score") +
  theme(legend.position = "none") +
  theme(strip.background = element_blank()) +
  scale_color_manual(values = experimental_group_palette_2[c("Progenitor 1", "Progenitor 2", "Neutrophil", "Erythroid", "Renal")]) +
  scale_x_continuous(breaks = c(-800, -400, 0, 400, 800))
tss_plot
# fimo----------------------------------------------------------
prkcda_peak1_fimo_plot <- 
  prkcda_peak1_fimo %>%
  group_by(motif_alt_id) %>%
  summarise(mean_q = mean(`q-value`), mean_score = mean(score)) %>%
  mutate(color = ifelse(mean_q < 0.01, alpha("red3", alpha = 1), "grey80")) %>%
  mutate(fill = ifelse(mean_q < 0.01, alpha("red3", alpha = 0.4), "transparent")) %>%
  mutate(motif_alt_id_label = ifelse(mean_q < 0.01, motif_alt_id, NA)) %>%
  ggplot(mapping = aes(x = log10(mean_score), y = -log10(mean_q), fill = fill, color = color, label = motif_alt_id_label)) +
  geom_point(shape = jitter_shape) +
  scale_fill_identity() +
  scale_color_identity() +
  geom_text_repel(size = 2.5, 
                  force = 2, 
                  seed = 1234, 
                  segment.curvature = -0.1, 
                  segment.square = TRUE, 
                  segment.inflect = TRUE, 
                  min.segment.length = 0, 
                  segment.color = "grey40", 
                  segment.size = 0.25,
                  max.overlaps = 1000) +
  labs(x = "log<sub>10</sub>FIMO score", y = "-log<sub>10</sub>q vale") +
  theme(axis.title.x = element_markdown())+
  theme(axis.title.y = element_markdown())
prkcda_peak1_fimo_plot

prkcda_peak3_fimo_plot <- 
  prkcda_peak3_fimo %>%
  group_by(motif_alt_id) %>%
  summarise(mean_q = mean(`q-value`), mean_score = mean(score)) %>%
  mutate(color = ifelse(mean_q < 0.01, alpha("red3", alpha = 1), "grey80")) %>%
  mutate(fill = ifelse(mean_q < 0.01, alpha("red3", alpha = 0.4), "transparent")) %>%
  mutate(motif_alt_id_label = ifelse(mean_q < 0.01, motif_alt_id, NA)) %>%
  ggplot(mapping = aes(x = log10(mean_score), y = -log10(mean_q), fill = fill, color = color, label = motif_alt_id_label)) +
  geom_point(shape = jitter_shape) +
  scale_fill_identity() +
  scale_color_identity() +
  geom_text_repel(size = 2.5, 
                  force = 2, 
                  seed = 1234, 
                  segment.curvature = -0.1, 
                  segment.square = TRUE, 
                  segment.inflect = TRUE, 
                  min.segment.length = 0, 
                  segment.color = "grey40", 
                  segment.size = 0.25, 
                  max.overlaps = 1000) +
  labs(x = "log<sub>10</sub>FIMO score", y = "-log<sub>10</sub>q value") +
  theme(axis.title.x = element_markdown()) +
  theme(axis.title.y = element_markdown())
prkcda_peak3_fimo_plot






# prkcda bulk atac trace--------------------------------------------------

e4_huvec_GRange_reduced <- subsetByOverlaps(e4_huvec_GRange_full, GRanges(seqnames = "chr3", ranges = IRanges(start = 53150000, end = 53200000))) %>% plyranges::mutate(group = "E4-HUVEC")

e4_trace <-
  bb_makeTrace(
    trace_data = e4_huvec_GRange_reduced,
    genome = "hg38",
    plot_range = "PRKCD",
    extend_left = 5000,
    extend_right = 1000,
    features = e4_huvec_peaks %>% plyranges::mutate(type = "Peaks")
    
  )

p6 <- bb_plot_trace_data(e4_trace, facet_var = "group", colvar = "group") + 
  labs(y = "Accessibility") + 
  scale_color_manual(values = "blue") +
  theme_shrinky(size = 8) +
  theme(axis.text.y.left = element_text(size = 6))
p7 <- bb_plot_trace_model(e4_trace) +
  theme_shrinky(size = 8)
p8 <- bb_plot_trace_feature(e4_trace, type_to_plot = "Peaks") +
  theme_shrinky(size = 8)
p9 <- bb_plot_trace_axis(e4_trace, xtitle = "Chr. 3") +
  theme(axis.text.x = element_text(size = 8))


# extract the sequence from the human and zfish enhancer peaks

top_tfbs_e4 <- mcols(hg38_prkcd_24105_fimo) %>%
  as_tibble() %>%
  group_by(motif_id) %>%
  arrange(q.value) %>%
  slice_head(n = 1)
hg38_prkcd <- Ape.setFeatures(ape = hg38_prkcd, gr = c(Ape.granges(hg38_prkcd), 
                                                       hg38_prkcd_24105_fimo %>% plyranges::filter(str_detect(locus_tag, "EWSR1-FLI1|KLF9|KLF4|Wt1"))))
# Ape.save(hg38_prkcd, out = "~/network/X/Labs/Blaser/Brad/hg38_prkcd.ape")

top_tfbs_zf <- mcols(dr11_prkcda_intron_enhancer_fimo) %>% 
  as_tibble() %>%
  group_by(motif_id) %>%
  arrange(q.value) %>%
  slice_head(n = 1)
dr11_prkcda <- Ape.setFeatures(dr11_prkcda, gr = c(Ape.granges(dr11_prkcda), 
                                    dr11_prkcda_intron_enhancer_fimo %>% plyranges::filter(str_detect(locus_tag, "EWSR1-FLI1|KLF9|KLF4|Wt1"))))
# Ape.save(dr11_prkcda, out = "~/network/X/Labs/Blaser/Brad/dr11_prkcda.ape")


tfbs_consensus <- full_join(top_tfbs_e4 %>% select(motif_id, q.value_e4 = q.value, locus_tag_e4 = locus_tag),
           top_tfbs_zf %>% select(motif_id, q.value_zf = q.value, locus_tag_zf = locus_tag)) %>%
  mutate(minus_log10_q_e4 = -1*log10(q.value_e4)) %>%
  mutate(minus_log10_q_zf = -1*log10(q.value_zf)) %>%
  mutate(label = str_replace(locus_tag_e4, "_[:graph:]*", "")) %>%
  mutate(threshold = ifelse(q.value_e4 < 0.05 && q.value_zf < 0.05, "yes", "no")) %>%
  mutate(minus_log10_q_zf = replace_na(minus_log10_q_zf, -Inf)) %>%
  mutate(minus_log10_q_e4 = replace_na(minus_log10_q_e4, -Inf)) %>%
  mutate(label = ifelse(threshold == "yes", label, NA))

tfbs_consensus_plot <- 
  ggplot(tfbs_consensus, mapping = aes(x = minus_log10_q_e4, y = minus_log10_q_zf, color = threshold, fill = threshold, label = label)) +
  geom_point(pch = 21) +
  geom_text_repel(size = 2.5, 
                  force = 2, 
                  seed = 1234, 
                  segment.curvature = -0.1, 
                  segment.square = TRUE, 
                  segment.inflect = TRUE, 
                  min.segment.length = 0, 
                  segment.color = "grey40", 
                  segment.size = 0.25, 
                  max.overlaps = 1000) +
  scale_color_manual(values = c("yes" = "red4", "no" = "grey80")) +
  scale_fill_manual(values = alpha(c("yes" = "red4", "no" = "transparent"), alpha = 0.4)) +
  theme(legend.position = "none") +
  labs(x = "-log<sub>10</sub> q E4-HUVEC", y = "-log<sub>10</sub> q zebrafish") +
  theme(axis.title.x = element_markdown()) +
  theme(axis.title.y = element_markdown())
tfbs_consensus_plot  


dr11_tfbs <- mcols(dr11_prkcda_intron_enhancer_fimo) %>% 
         as_tibble() %>%
         filter(q.value < 0.05) %>%
         mutate(tfbs = str_replace(locus_tag, "_[:graph:]*", "")) %>% 
         pull(tfbs) %>%
         unique()

hg38_tfbs <- mcols(hg38_prkcd_24105_fimo) %>% 
         as_tibble() %>% 
         filter(q.value < 0.05) %>%
         mutate(tfbs = str_replace(locus_tag, "_[:graph:]*", "")) %>% 
         pull(tfbs) %>%
         unique()

tfbs_venndata <- tibble(value = union(dr11_tfbs, hg38_tfbs)) |>
  mutate(dr11 = ifelse(value %in% dr11_tfbs, TRUE, FALSE)) |> 
  mutate(hg38 = ifelse(value %in% hg38_tfbs, TRUE, FALSE))


tfbs_venn <- ggplot(tfbs_venndata, aes(A = dr11, B =  hg38)) +
  ggvenn::geom_venn(set_names = c("D. rerio", "H. Sapiens"), 
                    stroke_size = 0.5, 
                    set_name_size = 3, 
                    text_size = 3) + 
  theme_void() + 
  coord_fixed(xlim = c(-2, 2), ylim = c(-1.5, 1.5))

# e4 atac tss enrichment------------------------------------
  
e4_atac_tss_enrichment_plot <- ggplot(e4_atac_tss_metafeature_data %>%
                                        mutate(label = str_replace(replicate, "rep", "")), 
                                      mapping = aes(x = target_distance, 
                                                    y = mean_normalized_count, 
                                                    color = label)) +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Distance to TSS", y = "Mean Normalized Counts", color = "Biological\nReplicate") +
  geom_vline(xintercept = 0,color = "grey80", linetype = "dashed") +
  theme(legend.position = c(0.75, 0.75))
e4_atac_tss_enrichment_plot

# revision:  table of cell assignments--------------------------------------
scatac_cluster_assignment_barplot <- bb_cellmeta(zf_cds) |> 
  select(leiden_assignment, seurat_predicted_id) |> 
  group_by(leiden_assignment, seurat_predicted_id) |> 
  summarise(n = n()) |> 
  filter(n>10) |> 
  mutate(leiden_assignment = factor(leiden_assignment, levels = c("Progenitor 1", "Progenitor 2", "Neutrophil", "Erythroid", "Renal"))) |>
  mutate(seurat_predicted_id = factor(seurat_predicted_id)) |> 
  ggplot(mapping = aes(x = leiden_assignment, y = n, fill = fct_reorder(seurat_predicted_id, n))) +
  geom_col(position = "fill", color = "black") +
  scale_fill_manual(values = revision_palette_1) +
  labs(x = "Cluster Assignment", y = "Proportion of Cluster", fill = "Predicted Label")
scatac_cluster_assignment_barplot
