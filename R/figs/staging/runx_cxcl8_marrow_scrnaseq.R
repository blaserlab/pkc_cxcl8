# make a useful function for later
GeomSplitViolin <- ggproto(
  "GeomSplitViolin",
  GeomViolin,
  draw_group = function(self, data, ..., draw_quantiles = NULL) {
    data <-
      transform(
        data,
        xminv = x - violinwidth * (x - xmin),
        xmaxv = x + violinwidth * (xmax - x)
      )
    grp <- data[1, "group"]
    newdata <-
      plyr::arrange(transform(data, x = if (grp %% 2 == 1)
        xminv
        else
          xmaxv), if (grp %% 2 == 1)
            y
        else-y)
    newdata <-
      rbind(newdata[1,], newdata, newdata[nrow(newdata),], newdata[1,])
    newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <-
      round(newdata[1, "x"])
    
    if (length(draw_quantiles) > 0 &
        !scales::zero_range(range(data$y))) {
      stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                1))
      quantiles <-
        ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
      aesthetics <-
        data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
      aesthetics$alpha <-
        rep(1, nrow(quantiles))
      both <- cbind(quantiles, aesthetics)
      quantile_grob <-
        GeomPath$draw_panel(both, ...)
      ggplot2:::ggname("geom_split_violin",
                       grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
    }
    else {
      ggplot2:::ggname("geom_split_violin",
                       GeomPolygon$draw_panel(newdata, ...))
    }
  }
)

geom_split_violin <-
  function(mapping = NULL,
           data = NULL,
           stat = "ydensity",
           position = "identity",
           ...,
           draw_quantiles = NULL,
           trim = TRUE,
           scale = "area",
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE) {
    layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomSplitViolin,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        trim = trim,
        scale = scale,
        draw_quantiles = draw_quantiles,
        na.rm = na.rm,
        ...
      )
    )
  }
#---------------------------
cxcl8_heme_class_umap <- 
  bb_var_umap(
  cds_cxcl8_marrow_heme,
  var = "label",
  cell_size = 2,
  palette = experimental_group_palette, foreground_alpha = 0.6,shape = 16
) +
  facet_wrap(facets = vars(value)) +
  theme(strip.background = element_blank())+
  theme(legend.position = "none")
cxcl8_heme_class_umap

cxcl8_partition_umap <-
  bb_var_umap(
    cds = cds_cxcl8_marrow_final,
    var = "partition_assignment_1",
    overwrite_labels = T,
    foreground_alpha = 0.6,
    palette = experimental_group_palette,
    group_label_size = 3
  )

# cxcl12a umap--------------------------------------------------------------------------
cxcl8_marrow_niche_cxcl12a_umap <-
  bb_gene_umap(cds_cxcl8_marrow_niche, gene_or_genes = "cxcl12a") +
  bb_annotate_npc(label = "cxcl12a",x = 0.4, y = 0.2, gp = gpar(fontface = "italic")) +
  theme_nothing() +
  theme(plot.background = element_rect(color = "black",size = 1))
cxcl8_marrow_niche_cxcl12a_umap + theme_cowplot()

# overlay ----------------------------------------------------------------------
cxcl8_marrow_niche_overlay <-
  ggdraw(
    bb_var_umap(
      cds = cds_cxcl8_marrow_niche,
      var = "recluster_louvain_assignment",
      overwrite_labels = T,
      cell_size = 2,
      foreground_alpha = 0.6,
      palette = experimental_group_palette,
      man_text_df = tribble(
        ~text_x, ~text_y, ~label,
        12.675, -1.775, "km pericyte",
        12.8, -2.125, "km sinusoidal",
        12.7, -1.6, "km osteoblast",
        12.625, -1.3, "km fibroblast"
      )
      
    )
  ) +
  draw_plot(cxcl8_marrow_niche_cxcl12a_umap, .25, .2, .3, .27)
cxcl8_marrow_niche_overlay

# plot a pseudotime trajectory for the hematopoietic cells-----------------------------------
cxcl8_heme_pseudotime_umap <-
  monocle3::plot_cells(
    cds_cxcl8_marrow_heme,
    color_cells_by = "pseudotime",
    label_leaves = F,
    cell_size = 1
  ) +
  theme_cowplot(font_size = 10) +
  scale_color_viridis_c(option = "plasma",
                        begin = 0,
                        end = 0.95) +
  labs(color = "pseudotime") +
  theme(legend.position = c(0.4, 0.8)) +
  theme(legend.direction = "horizontal") +
  guides(color = guide_colorbar(title.position = "top", 
                                barheight = 0.5, 
                                title.hjust = 0.5, 
                                title.theme = element_text(size = 9))) 
  
  
# plot gene expression in pseudotime---------------------------------------------------------
cxcl8_heme_pseudotime_genes <-
  monocle3::plot_genes_in_pseudotime(
    cds_cxcl8_marrow_heme[rowData(cds_cxcl8_marrow_heme)$gene_short_name %in% c("mpx", "spi1b", "tal1"), ],
    nrow = 3,
    panel_order = c("mpx", "spi1b", "tal1"),
    min_expr = 0,
    trend_formula = "~ splines::ns(pseudotime, df=2)",
    cell_size = 1
  ) + 
  theme_cowplot(font_size = 10) + 
  theme(strip.background = element_blank()) + 
  scale_color_viridis_c(option = "plasma",begin = 0, end = 0.95) + 
  theme(legend.position = "none")


# class distribution over pseudotime-------------------------------------------
cxcl8_heme_pseudotime_split_violin <- 
  colData(cds_cxcl8_marrow_heme) %>%
  as_tibble() %>%
  ggplot(mapping = aes(x = partition_assignment_1, y = pseudotime, fill = label)) +
  geom_split_violin() + 
  scale_fill_manual(values = alpha(experimental_group_palette, 0.4), breaks = c("control", "cxcl8")) +
  labs(fill = NULL, x = NULL, y = "Differentiation \u21D2") +
  theme(legend.position = "top") +
  theme(legend.justification = "center") +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.text.x = element_blank()) +
  bb_annotate_npc(label = "****", x = 0.5, y = 0.95) +
  scale_y_continuous(expand = expansion(mult = c(0.05,0.1)))

# gex umaps--------------------------------------------------------
cxcl8_marrow_spi1b_umap <-
  bb_gene_umap(cds_cxcl8_marrow_final, 
                 gene_or_genes = "spi1b") +
  theme(strip.text = element_blank()) +
  labs(title = "spi1b",color = NULL) + 
  theme(plot.title = element_text(face = "italic")) +
  theme(legend.position = c(0.35,0.2)) +
  theme(legend.direction = "horizontal") +
  guides(color = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
  guides(color = guide_colorbar(barheight = 0.5)) 

cxcl8_marrow_tal1_umap <-
  bb_gene_umap(cds_cxcl8_marrow_final, 
                 gene_or_genes = "tal1") +
  theme(strip.text = element_blank()) +
  labs(title = "tal1",color = NULL) + 
  theme(plot.title = element_text(face = "italic")) +
  theme(legend.position = c(0.35,0.2)) +
  theme(legend.direction = "horizontal") +
  guides(color = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
  guides(color = guide_colorbar(barheight = 0.5)) 

cxcl8_marrow_mpx_umap <-
  bb_gene_umap(cds_cxcl8_marrow_final, 
                 gene_or_genes = "mpx") +
  scale_color_viridis_c(breaks = c(-0.5, 0.5, 1.5), end = 0.8)+
  theme(strip.text = element_blank()) +
  labs(title = "mpx",color = NULL) + 
  theme(plot.title = element_text(face = "italic")) +
  theme(legend.position = c(0.35,0.2)) +
  theme(legend.direction = "horizontal") +
  guides(color = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
  guides(color = guide_colorbar(barheight = 0.5)) 

# niche gene heatmap--------------------------------------------------------------------
emb_niche_genesets <- 
  emb_louvain_assignment_top_markers %>%
  left_join(
    colData(cds_embryo_aligned) %>%
      as_tibble() %>%
      select(cell_group = louvain, louvain_assignment_1) %>%
      unique()
  ) %>%
  filter(
    louvain_assignment_1 %in% c(
      "sinusoidal",
      "lepr+ MSC",
      "osteoblast",
      "fibroblast",
      "chondrocyte",
      "pericyte"
    )
  ) %>%
  select(id = gene_id, gene_group = louvain_assignment_1)
cxcl8_marrow_niche_heatmap_mtx <- aggregate_gene_expression(
  cds = cds_cxcl8_marrow_niche,
  gene_group_df = as_tibble(emb_niche_genesets) %>% distinct(),
  cell_group_df = tibble(cell = rownames(colData(cds_cxcl8_marrow_niche)),
                         cell_group = colData(cds_cxcl8_marrow_niche)$recluster_louvain_assignment)
)

col_fun_cxcl8_marrow_niche <- colorRamp2(breaks = c(min(scale(cxcl8_marrow_niche_heatmap_mtx)),
                                                    0,
                                                    max(scale(cxcl8_marrow_niche_heatmap_mtx))),
                                         colors = heatmap_3_colors)

cxcl8_niche_aggscore_heatmap <- 
  grid.grabExpr(draw(
    Heatmap(
      t(scale(
        as.matrix(cxcl8_marrow_niche_heatmap_mtx)
      )),
      col = col_fun_cxcl8_marrow_niche,
      column_names_rot = 30,
      column_names_centered = F,
      column_title = "Embryo Niche Markers",
      column_title_side = "bottom",
      column_title_gp = gpar(fontsize = 10),
      row_title = "Marrow Niche\nCluster",
      row_title_gp = gpar(fontsize = 10),
      heatmap_legend_param = list(
        title = "Aggregate Score",
        title_gp = gpar(fontface = "plain", fontsize = 9),
        grid_width = unit(0.14, "in"),
        labels_gp = gpar(fontsize = 8)
      ),
      column_dend_height = unit(2.5, "mm"),
      row_dend_width = unit(2.5, "mm"),
      row_names_gp = gpar(fontsize = 9),
      column_names_gp = gpar(fontsize = 9),
      row_dend_gp = gpar(lwd = 0.5),
      column_dend_gp = gpar(lwd = 0.5)
    )
  ), wrap = TRUE)

# cluster representation barplot--------------------------------------------
cxcl8_marrow_cluster_representation_barplot <- 
  ggplot(data = cluster_proportions_cxcl8_marrow, 
         aes(x = recluster_louvain_assignment, y = log2fold_change_over_control, fill = enriched)
) + 
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = experimental_group_palette, breaks = c("km osteoblast", "km fibroblast", "km sinusoidal", "km pericyte")) + 
  labs(x = NULL, y = "Log<sub>2</sub>(cxcl8/control)") +
  theme(axis.title.y = element_markdown()) + 
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  guides(fill = guide_legend(ncol = 1)) +
  geom_text(mapping = aes(y = texty, label = p.signif), nudge_y = 0.3, size = 3, show.legend = F)
