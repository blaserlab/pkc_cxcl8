# graphical parameters####
main_fontsize = 10 
theme_set(theme_cowplot(font_size = main_fontsize))
options(Biostrings.coloring = F)

# show_col(pal_npg("nrc")(10))
experimental_group_palette <- c(
  "mcs" = "#3C5488",#blue
  "NC" = "#3C5488",#blue
  "VC" = "#3C5488",#blue
  "prkcda" = "#DC0000",#red
  "control" = "#3C5488",
  "competitor" = "#3C5488",
  "ELRCXC" = "#3C5488",
  "HA100" = "#F39B7F",
  "prkcaa" = "#DC0000",
  "prkcba" = "#DC0000",
  "prkcdb" = "#DC0000",
  "prkchb" = "#DC0000",
  "prkcsh" = "#DC0000",
  "prkcz" = "#DC0000",
  "PKC" = "#DC0000",
  "+" = "#DC0000",
  "-" = "#3C5488",
  "TG" = "#DC0000",
  "WT" = "#3C5488",
  "vehicle" = "transparent",
  "veh" = "transparent",
  "drug" = "darkorchid3",
  "ibr" = "darkorchid3",
  "other" = brewer.pal(n = 8, name = "Set2")[1],
  "not" = brewer.pal(n = 8, name = "Set2")[1],
  "niche" = brewer.pal(n = 8, name = "Set2")[2],
  "sinusoidal" = brewer.pal(n = 8, name = "Set2")[3],
  "km sinusoidal" = brewer.pal(n = 8, name = "Set2")[3],
  "MSC" = brewer.pal(n = 8, name = "Set2")[4],
  "osteoblast" = brewer.pal(n = 8, name = "Set2")[5],
  "km osteoblast" = brewer.pal(n = 8, name = "Set2")[5],
  "lepr+ MSC" = brewer.pal(n = 8, name = "Set2")[6],
  "proliferative" = brewer.pal(n = 8, name = "Set2")[7],
  "non-proliferative" = brewer.pal(n = 8, name = "Set2")[8],
  "progenitor" = brewer.pal(n = 5, name = "Set2")[3],
  "myeloid 1" = brewer.pal(n = 5, name = "Set2")[4],
  "myeloid 2" = brewer.pal(n = 5, name = "Set2")[5],
  "cxcl8" = "#DC0000",
  "tubule" = brewer.pal(n = 5, name = "Set2")[1],
  "TPA" = "#DC0000",
  "PKCi" = "#00A087",
  "TPA+PKCi" = "#F39B7F",
  "MEKi" = "#4DBBD5",
  "TPA+MEKi" = "#7E6148",
  "DDG" = "#DC0000",
  "chondrocyte" = brewer.pal(n = 8, name = "Accent")[6],
  "km chondrocyte" = brewer.pal(n = 8, name = "Accent")[6],
  "pericyte" = brewer.pal(n = 8, name = "Accent")[2],
  "km pericyte" = brewer.pal(n = 8, name = "Accent")[2],
  "fibroblast" = brewer.pal(n = 8, name = "Accent")[3],
  "km fibroblast" = brewer.pal(n = 8, name = "Accent")[3],
  "angio-\nhematopoietic" = brewer.pal(n = 12, name = "Set3")[10],
  "angiohematopoietic" = brewer.pal(n = 12, name = "Set3")[10],
  "tubule 1" = brewer.pal(n = 8, name = "Dark2")[1],
  "tubule 2" = brewer.pal(n = 8, name = "Dark2")[2],
  "neuronal" = brewer.pal(n = 8, name = "Dark2")[3],
  "germ cell" = brewer.pal(n = 8, name = "Dark2")[4],
  "niche" = brewer.pal(n = 8, name = "Dark2")[5],
  "heme" = brewer.pal(n = 8, name = "Dark2")[6],
  "hepatocyte" = brewer.pal(n = 8, name = "Dark2")[7]
  
)


experimental_group_palette_2 <- c(
  "Progenitor 1" = "#83a344",
  "Progenitor 2" = "#9171c8",
  "Neutrophil" = "#c8723e",
  "Erythroid" = "#4cab98",
  "Renal" = "#ca547e"

  )

# for the scatac cell assignment bar chart
revision_palette_1 <- c(
 brewer.pal(n = 12, name = "Paired"),
 "grey80"
)


jitter_alpha_fill <- 0.2
jitter_shape <- 21
jitter_size <- 2
jitter_stroke <- 0.5
jitter_width <- 0.2
jitter_alpha_color <- 1
jitter_height <- 0.2

summarybox_color <- "black"
summarybox_size <- 0.5
summarybox_width <- 0.3
summarybox_alpha <- 0.3
summarybox_geom <- "crossbar"

# 3 color heatmap
heatmap_3_colors <- c("#313695","white","#A50026")

# unmask
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("count", "dplyr")
conflict_prefer("get_legend", "cowplot")

# output
# output directories
figs_out <- "~/network/P/blaser_lab_p/writing/cxcl8_pkc_manuscript_2020/figures/r_pdfs"
tables_out <- "~/network/P/blaser_lab_P/writing/cxcl8_pkc_manuscript_2020/tables"

# set the figure device
device <- "pdf"

# set to true to re-source all panels before composing figures. Exception is facs plots and mutant typing plot which will not be restaged. 
stage_with_compose <- TRUE
# stage_with_compose <- FALSE


