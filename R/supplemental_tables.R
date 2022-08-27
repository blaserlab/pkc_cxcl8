#  zf embryo bulk rnaseq----------------------------
supplemental_table_1 <- volcano_df %>%
  write_csv(str_glue("{tables_out}/supplemental_table_1.csv"))

# gene module table---------------------------------------------
supplemental_table_2 <- 
  bind_rows(bb_rowmeta(cds_pmm_final) %>%
            mutate(related_figure = "Figure 3"),
          bb_rowmeta(cds_embryo_aligned) %>%
            mutate(related_figure = "Figure 6 embryo")) %>%
  relocate(gene_short_name) %>%
  relocate(related_figure) %>%
  arrange(related_figure,module) %>%
  select(-feature_id) %>%
  write_csv(str_glue("{tables_out}/supplemental_table_2.csv"))

# go term table--------------------------------------
supplemental_table_3 <- bind_rows(
  map2_dfr(
    .x = emb_rrvgo_thresh_0.9,
    .y = names(emb_rrvgo_thresh_0.9),
    .f = function(x, y) {
      rt <- as_tibble(x[["reducedTerms"]]) %>%
        mutate(gene_set = y)
      return(rt)
    }
  ) %>%
    mutate(related_figure = "Figure 6 embryo") %>%
    mutate(
      gene_set = recode(
        gene_set,
        "partition 22 top genes" = "sinusoidal top genes",
        "partition 2 top genes" = "lepr+ MSC top genes",
        "partition 3 top genes" = "MSC top genes",
        "partition 8 top genes" = "osteoblast top genes"
      )
    ),
  map2_dfr(
    .x = pmm_rrvgo,
    .y = names(pmm_rrvgo),
    .f = function(x, y) {
      rt <- as_tibble(x[["reducedTerms"]]) %>%
        mutate(gene_set = y)
      return(rt)
    }
  ) %>%
    mutate(related_figure = "Figure 3")
) %>%
  relocate(gene_set) %>%
  relocate(related_figure) %>%
  arrange(related_figure, gene_set) |> 
  write_csv(str_glue("{tables_out}/supplemental_table_3.csv"))


# top genes table------------------------------------------
supplemental_table_4 <- bind_rows(
  list(
    emb_louvain_assignment_top_markers %>%
      as_tibble() %>%
      mutate(related_figure = "Figure 6 embryo") %>%
      rename(cell_group = louvain_assignment_1) %>%
      relocate(cell_group) %>%
      relocate(related_figure) %>%
      select(-louvain) %>%
      filter(
        cell_group %in% c(
          "osteoblast",
          "fibroblast",
          "lepr+ MSC",
          "pericyte",
          "chondrocyte",
          "sinusoidal"
        )
      ),
    
    marker_test_res_c_pmm_anno %>%
      as_tibble() %>%
      mutate(related_figure = "Figure 3") %>%
      mutate(cell_group = revision_cluster_assignment) |> 
      relocate(cell_group) %>%
      relocate(related_figure) %>%
      select(-revision_cluster_assignment),
    cxcl8_marrow_partition_assignment_top_markers %>%
      as_tibble() %>%
      rename(cell_group = partition_assignment) %>%
      select(-partition) %>%
      mutate(related_figure = "Figure 6 marrow") %>%
      relocate(cell_group) %>%
      relocate(related_figure)
  )
) %>%
  arrange(related_figure, cell_group) %>%
  write_csv(str_glue("{tables_out}/supplemental_table_4.csv"))


# e4 deg-------------------------------------------------------------
supplemental_table_5 <- e4_res_table %>%
  select(-version) %>%
  arrange(padj) %>%
  write_csv(file = str_glue("{tables_out}/supplemental_table_5.csv"))

# e4 gsea res-----------------------------------------------------------
supplemental_table_6 <- 
  e4_gsea_res %>% 
  as_tibble() %>%
  arrange(desc(NES)) %>%
  mutate(leadingEdge = map_chr(leadingEdge,paste,collapse = ",")) %>%
  write_csv(str_glue("{tables_out}/supplemental_table_6.csv"))

