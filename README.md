# Microenvironmental Control of Hematopoietic Stem Cell Fate via CXCL8 and Protein Kinase C

This is an analysis project. It requires the processed R data package which is available at the Dryad URL provided in the manuscript. The data package is currently private for peer review. When downloaded from Dryad it will be compressed as a zip file. After unzipping, there should be a single file with the extension .tar.gz. This can be installed on your system like any other R package. However the recommended way to install it is in concert with this analysis project (see below). Together, this analysis project and this data package will reproduce all R-generated figures from the manuscript (excluding images such as western blots and micrographs).

After cloning this project, open R/dependencies.R. The first thing you should do is make sure you have renv installed. This is the package-management software used in the project. Then run renv::restore(). This will attempt to install all required packages listed in renv.lock into a local environment within your copy of the analysis project. Packages may be missed by this process; these should be installed manually using renv::install(<package name>). The blaseRtools and blaseRdata packages can be installed using renv::install("blaserlab/blaseRtools") and renv::install("blaserlab/blaseRdata") if necessary. These are custom tools used to make the figures. The rest should be well known and freely available.

Please note the last two lines of code in R/dependencies.R. The argument for the function bb_renv_datapkg() should be edited to point to the directory holding the unzipped data package for the analysis. This function will go to that directory and install the latest available version of the data into your local project. You can also specify the exact version if necessary by supplying the full file name ending in .tar.gz. The last line will load the data into a hidden environment. This is meant to emulate the lazyloading function of typical R packages: the data objects are loaded as pointers to files on disk and do not occupy memory until called. This minimizes the memory footprint and bypasses the normal size restriction on lazyloading package data in R.

Once everything is properly installed, you should restart the R session and source R/dependencies.R. If it sources without errors, then source R/configs.R. This has required aesthetic, output and other information for the files. You should edit the output section at the bottom.

* Individual figure panels can be recreated running the code in R/figs/staging. 
* Full figures can be generated by running the code in R/figs/composition. 
* All figures can be made by sourcing R/make_all_figs.R with the option stage_with_compose on configs.R set to TRUE. 
* Tables and summary stats can be generated by sourcing R/supplemental_tables.R and knitting Rmd/stats.Rmd, respectively.

If you want to inspect the code used to generate the data package, it will be installed in your package library in the directory, pkc.cxcl8.datapkg/data-raw.  Each data object has an associated manual page which can be accessed by clicking on "pkc.cxcl8.datapkg" in the packages panel of RStudio.  

## System Requirements

-   [R v4.1.2](https://www.r-project.org/)
-   [Bioconductor version 3.14](https://bioconductor.org/packages/release/bioc/html/BiocVersion.html)
-   This software has been tested on Linux Ubuntu 18.04.6 and Windows 10
-   Loading the complete dataset occupies close to 8 GB memory.

## Installation

-   clone the analysis repository to your computer using `git clone https://github.com/blaserlab/pkc_cxcl8.git`
-   open the R project by double-clicking on the pkc_cxcl8.Rproj file
-   open the file R/dependencies.R
-   uncomment and run the command, `renv::restore()`. This will attempt to install all required software dependencies.  
-   install any packages that cannot be found using `renv::install("<package name>")`.
    -   for bioconductor packages: `renv::install("bioc::<package name>")`
    -   for packages on github: `renv::install("<githubowner>/<repository name>")`
-   download the data package to your computer from the link provided in the manuscript and unzip
-   edit the line in R/dependencies.R starting `bb_renv_datapkg...` to point to the directory containing the unzipped data package. This should be a fully-qualified absolute path, meaning it should start with "/" on linux or mac and "C:/" or another drive identifier on Windows.
-   source R/dependencies.R
-   typical time required for the first installation and data loading is approximately 15 minutes.  This excludes the time required to download the data package.
-   open R/configs.R. Edit the lines beginning `figs_out <-` and `tables_out <-` to point to existing directories on your system
-   source R/configs.R

## Demo

Recreate Figure 1E from the paper using the Rstudio ide:

* source R/dependencies.R
* source R/configs.R
* source R/figs/staging/sele_pkc_runx.R
* run ```all_pkc_runx``` in the console
* Expected output:  a jitter plot showing Fold Change in HSPCs in zebrafish overexpressing PKC isoforms.
* Expected runtime:  seconds

## Instructions for use

After installing and configuring:

-   source R/dependencies.R
-   source R/configs.R
-   source R/make_all_figs.R. This will generate all computationally-derived figures in the manuscript.
-   source R/supplemental_tables.R. This will generate all supplementary tables in the manuscript.
-   open Rmd/stats.Rmd. Click on the knit dropdown menu and ensure knit directory is set to "Project Directory". Click "knit" to generate a detailed pdf statistics report to accompany the figures.

If properly configured, these scripts should run to completion in 1-2 minutes.
