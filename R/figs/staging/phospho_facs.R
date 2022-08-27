# phospho facs-------------------------------------------------------------------

my_logicle_trans <- function(w = 0.5,
                             t = 262144,
                             m = 4.5) {
  
  logicleTransform <- flowCore::logicleTransform(w = w,
                                                t = t,
                                                m = m,
                                                a = 0)
  transform <- function(x) {
    logicleTransform(x)
  }
  inverse = function(x) {
    flowCore::inverseLogicleTransform(transformationId = "inverseLogicle", trans = logicleTransform)(x)
  }
  breaks = function(x) {
    lim = 10 ** (1 + w)
    linear = scales::pretty_breaks(n = 3, min.n = 3)(c(x[1], lim))
    logs = scales::log_breaks(10)(c(lim, x[2]))
    unique(c(linear[linear <= lim], logs[logs > lim]))
  }
  scales::trans_new("logicle", transform, inverse, breaks, )
}

# mybreaks <- c(-100, -50, 0, 50, 100, 300, 500, 1000, 3000, 5000, 10000, 30000, 50000, 100000)
# mylabels <- c("-100", "","0", "","100", "", "", "10<sup>3</sup>", "", "", "10<sup>4</sup>", "", "", "10<sup>5</sup>")
mybreaks <- c(-100, -50, 0, 50, 100, 300, 500, 1000, 3000, 5000, 10000)
mylabels <- c("-100", "","0", "","100", "", "", "10<sup>3</sup>", "", "", "10<sup>4</sup>")


runx_perk_cut <- 262 
flk_perk_cut <- 125
plotmin <- -135
plotmax <- 10000
textpos <- 1000

segment_data <- tribble(
  ~xstart, ~xend, ~ystart, ~textx, ~celltype,
  # runx_perk_cut, 262144, 1.0, textpos, "Runx1<sup>+</sup>",
  # flk_perk_cut, 262144, 1.0, textpos, "kdrl<sup>+</sup>",
  runx_perk_cut, plotmax, 1.0, textpos, "Runx1<sup>+</sup>",
  flk_perk_cut, plotmax, 1.0, textpos, "kdrl<sup>+</sup>",
  
)

range(pfacs_data$`PE-A`)
# make the plot
pfacs_density_plot <- ggplot(pfacs_data |> mutate(celltype = recode(celltype, "flk" = "kdrl<sup>+</sup>", "runx" = "Runx1<sup>+</sup>"))) +
  geom_density(mapping = aes(x = `PE-A`, color = condition), adjust = 1/2)+
  scale_x_continuous(trans = my_logicle_trans(), breaks = mybreaks, labels = mylabels, limits = c(plotmin, plotmax)) +
  scale_color_manual(values = experimental_group_palette[c("control", "DDG")]) +
  theme(axis.text.x = ggtext::element_markdown()) +
  geom_segment(data = segment_data, mapping = aes(x = xstart, y = ystart, xend = xend, yend = ystart)) +
  geom_segment(data = segment_data, mapping = aes(x = xstart, xend = xstart, y = ystart-0.05, yend = ystart + 0.05)) +
  geom_richtext(data = segment_data, mapping = aes(x = textx, y = ystart), label = "p-ERK<sup>+</sup>", nudge_y = 0.3, fill = NA, label.color = NA, size = 3) +
  facet_wrap(~celltype, ncol = 1, scales = "fixed", strip.position = "right") +
  theme(strip.background = element_blank()) +
  theme(strip.text.y = element_markdown()) +
  labs(y = "Density", color = NULL) +
  theme(legend.position = "top") +
  theme(legend.direction = "horizontal") +
  theme(legend.justification = "center") +
  theme(strip.background.y = ggh4x::element_part_rect("l", color = "black"))
pfacs_density_plot
# get the summary data
pfacs_data |> 
  mutate(perk = 
           case_when(celltype == "flk" & `PE-A` > flk_perk_cut ~ "pos",
                     celltype == "runx" & `PE-A` > runx_perk_cut ~ "pos",
                     TRUE ~ "neg")) |> 
  count(sample, perk) |> 
  pivot_wider(names_from = "perk", values_from = "n") |> 
  mutate(percent_pos = pos/(neg+pos)*100)




