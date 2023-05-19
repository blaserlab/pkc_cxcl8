
suppressPackageStartupMessages(library("conflicted"))
suppressPackageStartupMessages(library("blaseRtools"))
suppressPackageStartupMessages(library("blaseRdata"))
suppressPackageStartupMessages(library("tidyverse"))
suppressPackageStartupMessages(library("monocle3"))
suppressPackageStartupMessages(library("circlize"))
suppressPackageStartupMessages(library("ComplexHeatmap"))
suppressPackageStartupMessages(library("lazyData"))
suppressPackageStartupMessages(library("cowplot"))
suppressPackageStartupMessages(library("RColorBrewer"))
suppressPackageStartupMessages(library("ggrepel"))
suppressPackageStartupMessages(library("ggpubr"))
suppressPackageStartupMessages(library("rstatix"))
suppressPackageStartupMessages(library("Signac"))
suppressPackageStartupMessages(library("patchwork"))
suppressPackageStartupMessages(library("SeuratWrappers"))
suppressPackageStartupMessages(library("Seurat"))
suppressPackageStartupMessages(library("ggtext"))
suppressPackageStartupMessages(library("CrispRVariants"))
suppressPackageStartupMessages(library("msa"))
suppressPackageStartupMessages(library("GenomicFeatures"))
suppressPackageStartupMessages(library("AnnotationDbi"))
suppressPackageStartupMessages(library("eulerr"))
suppressPackageStartupMessages(library("pander"))
suppressPackageStartupMessages(library("Cairo"))


# load, install, and/or update the project data -----------------------------
# this requires that you are using a blaseRtemplates installation, see https://blaserlab.github.io/blaseRtemplates/articles/establish.html
# you would have to edit the path variable to point to the directory holding the pkc.cxcl8.datapkg tar.gz file
blaseRtemplates::project_data("/home/OSUMC.EDU/blas02/network/X/Labs/Blaser/Brad/projects/pkc_cxcl8_data/datapkg")

# alternatively, use the data function to load the data objects after installing pkc.cxcl8.datapkg
# d <- data(package = "pkc.cxcl8.datapkg")
# data(list = d$results[, "Item"], package = "pkc.cxcl8.datapkg")
# rm(d)
