# Microenvironmental Control of Hematopoietic Stem Cell Fate via CXCL8 and Protein Kinase C

## Description
Altered hematopoietic stem cell (HSC) fate underlies primary blood disorders but microenvironmental factors controlling this are poorly understood.  Genetically barcoded GESTALT zebrafish were used to screen for factors expressed by the sinusoidal vascular niche that alter the phylogenetic distribution of the HSC pool under native conditions.  Dysregulated expression of protein kinase C delta (PKC-delta, encoded by prkcda) increased the number of HSC clones by up to 80% and expanded polyclonal populations of immature neutrophil and erythroid precursors.  PKC agonists such as cxcl8 augmented HSC competition for residency within the niche and expanded defined niche populations.  CXCL8 induced association of PKC-delta with the focal adhesion complex, activating ERK signaling and expression of niche factors in human endothelial cells.  Our findings demonstrate the existence of reserve capacity within the niche which is controlled by CXCL8 and PKC and has significant impact on HSC phylogenetic and phenotypic fate.
	
## Steps to Reproduce

1. System Requirements
  - R v4.2
  - Rstudio
  - This software has been tested on Linux Ubuntu 18.04.6 and Windows 10
  - Loading the complete dataset occupies approximately 7 GB memory.

2.  Installation
  - go to DOI: 10.17632/6s7vy929dc.1
  - download pkc.cxcl8.datapkg and unzip in a convenient location on your system.  This contains the processed data required for this analysis project to function.
  - clone this analysis project to your computer using git clone https://github.com/blaserlab/pkc_cxcl8.git
  - open the R project by double-clicking on the pkc_cxcl8.Rproj file
  - a list of the packages required for the project can be found in library_catalogs/blas02_pkc_cxcl8.tsv.  Filter for packages with status == "active".   Install these packages.
  - install custom packages from our R Universe repository using these commands:
    -  install.packages('blaseRtools', repos = c('https://blaserlab.r-universe.dev', 'https://cloud.r-project.org'))
    -  install.packages('blaseRtemplates', repos = c('https://blaserlab.r-universe.dev', 'https://cloud.r-project.org'))
    -  install.packages('blaseRdata', repos = c('https://blaserlab.r-universe.dev', 'https://cloud.r-project.org'))
  - edit R/dependencies.R
    - unless you are using a blaseRtemplates installation, comment out the last active line starting blaseRtemplates::project_data....
    - uncomment/activate the last 3 lines to load the data into your workspace
  - edit R/configs.R 
    - the file paths for output should be customized for your system
  - typical time required for the first installation and data loading is approximately 15 minutes. This excludes the time required to download the data package.

3.  Instructions for use after installing and configuring
  - source R/dependencies.R
  - source R/configs.R
  - source R/make_all_figs.R. This will generate all computationally-derived figures in the manuscript.
  - source R/supplemental_tables.R. This will generate all supplementary tables in the manuscript.
  - open Rmd/stats.Rmd. Click on the knit dropdown menu and ensure knit directory is set to "Project Directory". Click "knit" to generate a detailed pdf statistics report to accompany the figures.
  - If properly configured, these scripts should run to completion in 1-2 minutes.

4.  Each computationally-generated figure panel is associated with processed data and code for visualization.  Each processed data object has its own help manual and associated processing code within the data package.  To access these resources do the following:
  - find the variable name for the panel you wish to review in the appropriate figure composition file in R/figs/composition.
  - search for that variable name in R/figs/staging
  - find the original data object used to generate that panel in the code
  - type ?data_object_name to get the help manual
  - to review processing code, go to the installed location of pkc.cxcl8.datapkg on your system, enter the data-raw directory and run grep --include=\*.R -rnw '.' -e "data_object_name"
