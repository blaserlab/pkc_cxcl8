bb_cellmeta(cds_pmm_final) |> glimpse()
bb_var_umap(cds_pmm_final, "cluster_assignment")
# lookoing for lymphoid cells
bb_gene_umap(cds_pmm_final, "syk")
bb_gene_umap(cds_pmm_final, "ccr9a")
bb_gene_umap(cds_pmm_final, "il2rga")

bb_gene_umap(cds_pmm_final, "osmr")
bb_gene_umap(cds_pmm_final, "ccl34b.9")
bb_gene_umap(cds_pmm_final, "cd79a")
bb_gene_umap(cds_pmm_final, "cd81a")
bb_gene_umap(cds_pmm_final, "wasb")
bb_gene_umap(cds_pmm_final, "cd3d")
bb_gene_umap(cds_pmm_final, "ikzf1")
bb_gene_umap(cds_pmm_final, "tal1")
bb_gene_umap(cds_pmm_final, "lmo2")
bb_gene_umap(cds_pmm_final, "itga2b")
bb_gene_umap(cds_pmm_final, "nfe2")
bb_gene_umap(cds_pmm_final, "meis1b")


bb_var_umap(cds_pmm_final, "cluster", overwrite_labels = T)
bb_gene_umap(filter_cds(cds_pmm_final, cells = bb_cellmeta(cds_pmm_final) |> filter(cluster_assignment == "Progenitor 1")), "itga2b")
bb_gene_umap(filter_cds(cds_pmm_final, cells = bb_cellmeta(cds_pmm_final) |> filter(cluster_assignment == "Progenitor 1")), "nfe2")
bb_genebubbles(cds_pmm_final, genes = c("itga2b", "nfe2", "mpl"), cell_grouping = "cluster")
bb_var_umap(cds_pmm_final, "")
bb_var_umap(filter_cds(cds_pmm_final, cells = bb_cellmeta(cds_pmm_final) |> filter(cluster == "22")), "class")
bb_cluster_representation(cds_pmm_final, cluster_var = "cluster", class_var = "class", experimental_class = "prkcda", control_class = "mcs", return_value = "plot")



# coro1a+ lyz- are T progenitors and monocytes:  https://doi.org/10.4049/jimmunol.1901494
bb_gene_umap(cds_pmm_final, "coro1a")
bb_gene_umap(cds_pmm_final, "lyz")
bb_gene_umap(cds_pmm_final, "mpeg1.1")
bb_gene_umap(cds_pmm_final, "mpx")
bb_gene_umap(cds_pmm_final, "rac2")
bb_gene_umap(cds_pmm_final, "hp")

bb_var_umap(cds_pmm_final, "class", facet_by = "value")

bb_gene_umap(cds_embryo_aligned, "cxcr2")

supplemental_table_4 |> group_by(related_figure) |> summarise()
supplemental_table_4 |> filter(related_figure == "Figure 3") |> filter(cell_group == "Progenitor 2") |> View()

bb_genebubbles(obj = cds_pmm_final, genes = c("coro1a", "lyz", "cd81a"), cell_grouping = "cluster_assignment")



baron_genesets <- map_dfr(.x = readxl::excel_sheets("~/network/X/Labs/Blaser/Brad/projects/cxcl8_pkc/reference/baron_cell_2019/1-s2.0-S0092867419308943-mmc3.xlsx"),
    .f = \(x, 
           rd = bb_rowmeta(cds_pmm_final),
           xl = "~/network/X/Labs/Blaser/Brad/projects/cxcl8_pkc/reference/baron_cell_2019/1-s2.0-S0092867419308943-mmc3.xlsx") {
      dat <- readxl::read_excel(xl, sheet = x, col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric")) |> 
        filter(`Adjusted P-value` < 0.05) |>
        filter(`Fold change` >1) |> 
        select(feature_id = 1) |> 
        mutate(celltype = x) |>
        mutate(feature_id = str_remove(feature_id, "_.*")) |> 
        right_join(rd) |> 
        select(feature_id, celltype) |> 
        mutate(value = ifelse(is.na(celltype), FALSE, TRUE)) |> 
        mutate(celltype = x)
    }) |> pivot_wider(names_from = "celltype", values_from = "value")
cds_pmm_final <- bb_tbl_to_rowdata(cds_pmm_final, min_tbl = baron_genesets)
bb_rowmeta(cds_pmm_final)

bb_gene_umap(cds_pmm_final, gene_or_genes = bb_rowmeta(cds_pmm_final) |> select(feature_id, Eosinophils) |> filter(Eosinophils))
bb_gene_umap(cds_pmm_final, "pax8")


test <- read_delim("/workspace/refdata/cellranger_arc_drerio/Danio_rerio.GRCz11.107.gtf", delim = "\t", skip = 5, col_names = c("seqname", "source", "feature", "start", "end", "score", "strand", "frame", "attribute"))
test1 <- test |> mutate(gb = str_extract(attribute, "gene_biotype.*")) |> mutate(gb = str_remove_all(gb, ";.*")) |> count(gb)
test1

