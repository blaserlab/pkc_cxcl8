# prkcda longitudnal clone jitterplot ####---------------------------------------------------------------------------------------------------------------------------------------------
prkcda_clone_plot <-
  ggplot(data = gestalt_20180900_named_1 %>%
           mutate(label_new = recode(class, "mcs" = "control")),
         mapping = aes(
           x = label_new,
           y = value,
           fill = label_new,
           color = label_new
         )) +
  geom_jitter(
    shape = jitter_shape,
    width = jitter_width,
    height = 0,
    size = jitter_size
  ) +
  scale_fill_manual(values = alpha(c(experimental_group_palette), jitter_alpha_fill), limits = c("control", "prkcda")) +
  scale_color_manual(values = experimental_group_palette, limits = c("control", "prkcda")) +
  stat_summary(
    fun.data = data_summary_mean_se,
    color = summarybox_color,
    size = summarybox_size,
    width = summarybox_width,
    alpha = summarybox_alpha,
    geom = summarybox_geom,
    position = position_dodge(width = 0.75),
    show.legend = FALSE
  ) +
  stat_compare_means(
    mapping = aes(group = label_new),
    method = "t.test",
    label = "p.signif",
    label.y.npc = 0.95,
    label.x.npc = "center",
    comparisons = list(c(1,2))) +
  facet_wrap(facets = vars(timepoint), nrow = 1,strip.position = "bottom") +
  theme(strip.placement = "outside") + 
  theme(strip.background = element_blank()) +
  theme(legend.position = "top") +
  theme(legend.justification = "center") +
  scale_y_continuous(expand = expansion(mult = c(0.1,0.1)),breaks = c(2,4,6,8,10,12)) +
  theme(axis.text.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  labs(x = NULL,
       y = "HSC Clones",
       color = NULL,
       fill = NULL
       ) 
# prkcda_clone_plot
# summary gestalt correlation plot-------------------------------------------------

gestalt_cor_summary_plot <- gestalt_cor_data %>%
  filter(name == "baseline_cor") %>%
  group_by(first_name) %>%
  mutate(rn = row_number()) %>%
  filter(rn != 1) %>%
  select(-rn,-sum_cor,-mean_cor,-name) %>%
  mutate(label_new = recode(class, "mcs" = "control")) %>%
  ggplot(mapping = aes(
    x = label_new,
    y = value,
    fill = label_new,
    color = label_new
  )) +
  geom_jitter(
    width = jitter_width,
    shape = 22,
    size = jitter_size,
    stroke = jitter_stroke
  ) +
  scale_fill_manual(values = alpha(c(experimental_group_palette), jitter_alpha_fill), limits = c("control", "prkcda")) +
  scale_color_manual(values = experimental_group_palette, limits = c("control", "prkcda")) +
  stat_summary(
    fun.data = data_summary_mean_se,
    color = summarybox_color,
    size = summarybox_size,
    width = summarybox_width,
    alpha = summarybox_alpha,
    geom = summarybox_geom,
    position = position_dodge(width = 0.75),
    show.legend = FALSE
  ) +
  stat_compare_means(
    mapping = aes(group = label_new),
    method = "wilcox",
    label = "p.signif",
    label.y.npc = 0.95,
    label.x.npc = "center",
    comparisons = list(c(1,2))) +
  facet_wrap(facets = vars(timepoint), nrow = 1,strip.position = "bottom") +
  theme(strip.placement = "outside") + 
  theme(strip.background = element_blank()) +
  theme(legend.position = "top") +
  theme(legend.justification = "center") +
  theme(axis.text.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  labs(x = NULL,
       y = "Barcode Correlation",
       color = NULL,
       fill = NULL
       ) + 
  scale_y_continuous(expand = expansion(mult = c(0.1,0.1)),breaks = c(0.25,0.5,0.75,1.00))
# gestalt_cor_summary_plot

# pacman plot####-------------------------------------------------------------------------
mcs_pacman_filler <- tibble(first_name = c("filler1","filler1","filler2","filler2","filler3","filler3"),
       class = rep("mcs", times = 6),
       timepoint = factor(rep("3 mpf", times = 6)),
       name = c("baseline_cor","anti_cor","baseline_cor","anti_cor","baseline_cor","anti_cor"),
       value = rep(NA, times = 6),
       sum_cor = rep(0, times = 6),
       mean_cor = rep(0, times = 6),
       label = c("",""," "," ", "  ","  "),
       mpf = factor(rep(3, times = 6)),
       )


mcs_pacman_data <- 
  gestalt_cor_data %>% 
  filter(class == "mcs") %>% 
  bind_rows(mcs_pacman_filler) %>%
  group_by(first_name) %>%
  mutate(encounter = row_number()) %>%
  mutate(name = ifelse(encounter == 1,"first_encounter",name))


pacman_plot_mcs <- ggplot(data = mcs_pacman_data,
       mapping = aes(x = "", y = value, fill = name)) +
  geom_col(width = 1,color = "black") +
  scale_fill_manual(values = c("red", "yellow","grey80")) +
  coord_polar("y", start = pi / 3) +
  labs(x = NULL, y = NULL, title = "control", caption = "mpf") +
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()
        ) +
  facet_grid(rows = vars(fct_reorder(label, -sum_cor)), cols = vars(mpf), switch = "both") + 
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0)) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.caption = element_text(hjust = 0.5, size = 10))
pacman_plot_mcs

prkcda_pacman_data <- 
  gestalt_cor_data %>% 
  filter(class == "prkcda") %>% 
  group_by(first_name) %>%
  mutate(encounter = row_number()) %>%
  mutate(name = ifelse(encounter == 1,"first_encounter",name))

pacman_plot_prkcda <- ggplot(data = prkcda_pacman_data %>% mutate(name = recode(name, "anti_cor" = "Not correlated", "baseline_cor" = "Correlated", "first_encounter" = "Baseline" )),
       mapping = aes(x = "", y = value, fill = name)) +
  geom_col(width = 1,color = "black") +
  scale_fill_manual(values = c("grey80", "yellow", "red")) +
  coord_polar("y", start = pi / 3) +
  labs(x = NULL, y = NULL, title = "prkcda", caption = "mpf", fill = NULL) +
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()
        ) +
  facet_grid(rows = vars(fct_reorder(label, -sum_cor)), cols = vars(mpf), switch = "both") + 
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0)) +
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.caption = element_text(hjust = 0.5, size = 10))
pacman_plot_prkcda
pacman_legend <- get_legend(
  # create some space to the left of the legend
  pacman_plot_prkcda
)
pacman_legend_centered <- plot_grid(NULL, pacman_legend, NULL, ncol = 3)
# gestalt areaplot ####-----------------------------------------------------------------------------
gestalt_areaplot <-
  map(
    .x = gestalt_areaplot_data,
    .f = function(data) {
      p <- ggplot(data = data, aes(x = mpf, y = norm_vaf, fill = class)) +
        facet_wrap( ~ fac_label, nrow = 1) +
        geom_area(color = "black", aes(alpha = reorder(variant, vaf_t1))) +
        scale_fill_manual(values = experimental_group_palette) +
        theme(legend.position = "none") +
        labs(y = "Frequency", x = "mpf", title = NULL) +
        scale_x_continuous(breaks = c(3,6,9,12,22)) +
        theme(strip.background = element_blank(),strip.text = element_blank())
      return(p)
    }
  )
gestalt_areaplot[[1]]
gestalt_areaplot[[17]]

