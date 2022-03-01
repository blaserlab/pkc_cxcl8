# volcano plot ####-----------------------------------------------------------
# select genes to label on left and right
label_df_left <- filter(volcano_df, gene %in% c("prkcda"))
label_df_right <-
  filter(volcano_df, gene %in% c("flt4", "egfl7", "kdr"))

# make the plot####
x_limits_left <- c(NA, -10)
x_limits_right <- c(10, NA)

volcanoplot <-
  ggplot(volcano_df, aes(x = ALFC, y = neglog10p, color = significant, fill = significant)) +
  geom_point(
    stat = "identity",
    position = "identity",
    size = 0.25,
    shape = 21
  ) +
  scale_color_manual(values = c("grey80", "#DC0000")) +
  scale_fill_manual(values = c("transparent", "#DC0000"))+
  theme(legend.position = "none") +
  labs(y = expression(-log[10] * "p"),
       x = "ALFC"
       ) +
  geom_text_repel(
    data = label_df_left,
    size = 3,
    segment.size = 0.25,
    segment.alpha = 0.5,
    min.segment.length = 0.1,
    aes(
      label = label_df_left$gene,
      x = label_df_left$ALFC,
      y = label_df_left$neglog10p
    ),
    color = "black",
    xlim = x_limits_left
  ) +
  geom_text_repel(
    data = label_df_right,
    size = 3,
    segment.size = 0.25,
    segment.alpha = 0.5,
    aes(
      label = label_df_right$gene,
      x = label_df_right$ALFC,
      y = label_df_right$neglog10p
    ),
    xlim = x_limits_right,
    direction = "y",
    box.padding = grid::unit(0.5, "lines"),
    color = "black",
    max.overlaps = 100,
    force = 2,
    seed = 1234,
    segment.curvature = -0.1,
    segment.square = TRUE,
    segment.inflect = TRUE,
    min.segment.length = 0
  ) 

# bar plot with pkc family####---------------------------------------------------------------
pkc_family_plot <-
  ggplot(pkc_data_to_plot, aes(x = gene, y = value, fill = alt_name1)) +
  geom_bar(position = "dodge",
           stat = "identity",
           color = "black") +
  scale_fill_manual(values = c("Not CHT-Derived" = "#00FF00", "CHT-Derived" = "#FFFF00")) +
  scale_y_continuous(limits = c(0, 24), breaks = c(0, 5, 10, 15, 20)) +
  geom_segment(aes(
    x = 3.6,
    xend = 4.4,
    y = 23,
    yend = 23
  ),
  size = 0.25,
  colour = "black") +
  ggplot2::annotate("text",
           x = 4,
           y = 23.5,
           label = "***") +
  theme(legend.position = c(0.4, 0.8),legend.text = element_text(size = 8)) +
  labs(y = "Expression",
       x = NULL,
       title = NULL,
       fill = NULL) +
  guides(fill = guide_legend(keywidth = 0.5, keyheight = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

