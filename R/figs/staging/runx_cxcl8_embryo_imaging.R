# runx cxcl8 prkcda mutant -----------------------------------------------------------
runx_cxcl8_prkcda_mut_plot <- 
  ggplot(data = runx_cxcl8_prkcda_mut, 
         mapping = aes(x = time_h, fill = label, color = label)) + 
  geom_density(weight = 0.5) +
  scale_fill_manual(name = NULL, labels = c("cxcl8", "control"), values = alpha(experimental_group_palette,0.4), limits = c("cxcl8", "control")) +
  scale_color_manual(name = NULL, labels = c("cxcl8", "control"), values = experimental_group_palette, limits = c("cxcl8", "control")) + 
  geom_vline(
    data = as.data.frame(runx_cxcl8_prkcda_mut %>% 
                           group_by(label) %>% 
                           summarise(median = median(time_h))),
    aes(xintercept = median, color = label),
    show.legend = F
  ) +
  xlim(0, max(runx_cxcl8_restime$time) * 1.2) +
  labs(x = "CHT Residency Time (h)",
       y = "Density",
       fill = NULL,
       subtitle = "*prkcda<sup>-/-</sup>*") +
  bb_annotate_npc(label = "ns", x = 0.5, y = 0.9, gp = gpar(fontsize = 11)) +
  theme(legend.position = c(0.4, 0.6)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(plot.subtitle = ggtext::element_markdown(face = "italic"))

# genotyping figure-------------------------------------------------------------------

txdb <- AnnotationDbi::loadDb(file = system.file("extdata", "GRCz11.97_txdb.sqlite", package = "pkc.cxcl8.datapkg"))

# plot the variants
crispr_variants_plot <- plotVariants(crispr_set,
                  txdb = txdb,
                  col.wdth.ratio = c(1,1),
                  plotAlignments.args = list(pam.start = 7,
                                             target.loc = 12,
                                             guide.loc = IRanges::IRanges(7,29),
                                             min.count = 500,
                                             tile.height = 0.55,
                                             top.n = 3),
                  plotFreqHeatmap.args = list(min.count = 500,
                                              top.n = 3,
                                              plot.text.size = 3,
                                              x.size = 8, #group = rep(group_desig,length(bam_temp_files)),
                                              legend.text.size = 8,
                                              # type = "proportions",
                                              legend.position = "bottom",
                                              legend.key.height = grid::unit(0.5, "lines"),
                                              axis.text.x = element_text(hjust = 1)
                                              ))



# residency time in transients####------------------------------------------------------------------------------------
runx_cxcl8_restime_plot <-
  ggplot(data = runx_cxcl8_restime, 
         mapping = aes(x = time, fill = label, color = label)) +
  geom_density(weight = 0.5) +
  scale_fill_manual(name = NULL, labels = c("cxcl8", "control"), values = alpha(experimental_group_palette,0.4), limits = c("cxcl8", "control")) +
  scale_color_manual(name = NULL, labels = c("cxcl8", "control"), values = experimental_group_palette, limits = c("cxcl8", "control")) + 
  geom_vline(
    data = as.data.frame(runx_cxcl8_restime %>% 
                           group_by(label) %>% 
                           summarise(median = median(time))),
    aes(xintercept = median, color = label),
    show.legend = F
  ) +
  xlim(0, max(runx_cxcl8_restime$time) * 1.2) +
  labs(x = "CHT Residency Time (h)",
       y = "Density",
       fill = NULL,
       title = NULL,
       subtitle = "*casper*") +
  bb_annotate_npc(label = "**", x = 0.5, y = 0.9) +
  theme(legend.position = c(0.4, 0.6)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(plot.subtitle = ggtext::element_markdown())

# cuddling time in transients####----------------------------------------------------------
# runx_cxcl8_cuddletime_plot <-
#   ggplot(runx_cxcl8_cuddletime,
#          aes(x = label,
#              y = percent_cuddling_time,
#              fill = label,
#              color = label)) +
#   geom_violin(scale = "area") +
#   scale_color_manual(values = experimental_group_palette, guide = F) +
#   scale_fill_manual(values = alpha(experimental_group_palette, 0.4), guide = F) +
#   scale_linetype_manual(values = c("solid", "solid"), guide = F) +
#   stat_compare_means(
#     method = "wilcox",
#     label = "p.signif",
#     label.x.npc = "center"
#   ) +
#   labs(y = "% Cuddling Time", x = NULL, title = NULL) +
#   scale_y_continuous(expand = expansion(mult = c(0.1))) 

runx_cxcl8_cuddletime_plot <- ggplot(
  runx_cxcl8_cuddletime,
  aes(
    x = end_movie_status,
    y = percent_cuddling_time,
    fill = label,
    color = label
  )
) +
  geom_split_violin()+
  scale_color_manual(name = NULL, values = experimental_group_palette[c("control", "cxcl8")], limits = c("cxcl8", "control")) +
  scale_fill_manual(name = NULL, values = alpha(experimental_group_palette[c("control", "cxcl8")], 0.4), limits = c("cxcl8", "control")) +
  labs(y = "% Cuddling Time", x = NULL, title = NULL, fill = NULL) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.text.x = element_blank()) +
  theme(legend.position = c(0.75, 0.25)) +
  bb_annotate_npc(label = "**", x = 0.5, y = 0.95) +
  scale_y_continuous(expand = expansion(mult = c(0.05,0.1))) +
  theme(legend.position = "top") +
  theme(legend.justification = "center") +
  theme(legend.direction = "vertical") +
  theme(axis.line.x = element_blank())
  



# runx stables jitter plot ####-------------------------------------------------------------------------

runx_cxcl8_stables_plot <-
  ggplot(runx_cxcl8_stables %>% 
           mutate(transgene = "Runx1:\ncxcl8") %>%
           mutate(label_new = recode(label, "+" = "TG")) %>% 
           mutate(label_new = factor(label_new, levels = c("-", "TG"))), 
         aes(x = label_new, y = runx_count, fill = label_new, color = label_new)) +
  geom_jitter(
    shape = jitter_shape,
    width = jitter_width,
    size = jitter_size,
    stroke = jitter_stroke,
    height = 0
  ) +
  scale_fill_manual(values = alpha(experimental_group_palette, 0.4)) +
  scale_color_manual(values = experimental_group_palette) +
  stat_summary(
    fun.data = data_summary_mean_se,
    color = summarybox_color,
    size = summarybox_size,
    width = summarybox_width,
    alpha = summarybox_alpha,
    geom = summarybox_geom
  ) +
  stat_compare_means(method = "wilcox",
                     label = "p.signif",
                     label.x.npc = "center") +
  theme(legend.position = "none") +
  labs(x = NULL,
       y  = "HSPCs/CHT",
       fill = NULL,
       title = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0.1))) +
  facet_wrap(facets = vars(transgene)) +
  theme(strip.background = element_blank()) +
  theme(strip.text = element_text(face = "italic"))


# competition density plot ####-------------------------------------------------------------------------
runx_cxcl8_competition_plot <-
  ggplot(runx_cxcl8_competition, 
         mapping = aes(x = time_h, fill = class, color = class)) +
  geom_density(weight = 0.5) +
  scale_color_manual(name = NULL, values = experimental_group_palette, limits = c("cxcl8", "competitor")) + 
  scale_fill_manual(name = NULL, values = alpha(experimental_group_palette,0.4), limits = c("cxcl8", "competitor")) +
  geom_vline(
    data = as.data.frame(
      runx_cxcl8_competition %>%
        group_by(class) %>%
        summarise(median = median(time_h))
    ),
    aes(xintercept = median, color = class),
    show.legend = F
  ) +
  xlim(0, max(runx_cxcl8_competition$time_h) * 1.2) +
  labs(x = "CHT Residency Time (h)",
       y = "Density",
       fill = NULL,
       title = NULL,
       subtitle = "*casper*: competition") +
  bb_annotate_npc(label = "****", x = 0.5, y = 0.9) +
  theme(legend.position = c(0.4, 0.6)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(plot.subtitle = ggtext::element_markdown())

# elrcxc density plot ####------------------------------------------------------------------

elrcxc_plot <-
  ggplot(data = elrcxc, 
         mapping = aes(x = time_h, fill = label, color = label)) +
  geom_density(weight = 0.5) +
  scale_fill_manual(
    name = NULL,
    values = alpha(experimental_group_palette, 0.4),
    labels = list("cxcl8", bquote(Delta * ELRCXC)),
    limits = c("cxcl8", "ELRCXC")
  ) +
  scale_color_manual(
    name = NULL,
    values = experimental_group_palette,
    labels = list("cxcl8", bquote(Delta * ELRCXC)),
    limits = c("cxcl8", "ELRCXC")
  ) +
  geom_vline(
    data = as.data.frame(elrcxc %>%
                           group_by(label) %>%
                           summarise(median = median(time_h))),
    aes(xintercept = median, color = label),
    show.legend = F
  ) +
  xlim(0, max(elrcxc$time_h) * 1.2) +
  labs(x = "CHT Residency Time (h)", 
       y = "Density", 
       fill = NULL,
       subtitle = "*casper*") +
  bb_annotate_npc(label = "**", x = 0.5, y = 0.9) +
  theme(legend.position = c(0.4, 0.6)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(plot.subtitle = ggtext::element_markdown())

# drug studies ####--------------------------------------------------------------------------

runx_drug_plot <- 
  ggplot(
  data = drug_data %>%
    mutate(label_new = recode(runx_cxcl8, "+" = "TG")) %>%
    mutate(label_new = factor(label_new, levels = c("-", "TG"))),
  mapping = aes(x = label_new, y = fold_change_cell_count, fill = treated)
) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point(
    position = position_jitterdodge(jitter.width = jitter_width,
                                    jitter.height = 0,
                                    dodge.width = 0.75),
    color = "black",
    shape = jitter_shape,
    size = jitter_size,
    stroke = jitter_stroke,
  ) +
  scale_fill_manual(values = alpha(experimental_group_palette, 0.6),
                    limits = c("vehicle", "drug")) +
  stat_summary(
    fun.data = data_summary_mean_se,
    color = "black",
    size = 0.5,
    width = 0.3,
    alpha = 0.3,
    geom = "crossbar",
    show.legend = F,
    position = position_dodge(width = 0.75)
  ) + #adds box for mean plus minus SE
  stat_compare_means(
    method = "t.test",
    label = "p.signif",
    label.x.npc = "middle",
    hide.ns = F
  ) + #ggpubr function
  labs(x = "Runx1:cxcl8", y = "Fold Change in HSPCs", title = NULL, fill = NULL) +
  theme(axis.title.x = element_text(face = "italic")) +
  facet_wrap(
    facets = vars(drug_name),
    nrow = 1,
    strip.position = "top"
  ) +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  theme(legend.position = "top") +
  theme(legend.justification = "center") +
  scale_y_continuous(expand = expansion(mult = c(0.1)))



