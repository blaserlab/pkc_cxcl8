# uncomment and run to restore package library from lock file
# renv::restore()

# uncomment and run to rebuild the lock file
# renv::init()

# renv::install("blaserlab/blaseRtools")

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
suppressPackageStartupMessages(library("CytoExploreR"))
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
suppressPackageStartupMessages(library("conflicted"))


bb_renv_datapkg("/home/OSUMC.EDU/blas02/network/X/Labs/Blaser/Brad/projects/pkc_cxcl8_data/datapkg")

lazyData::requireData("pkc.cxcl8.datapkg")