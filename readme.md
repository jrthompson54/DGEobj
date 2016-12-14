# DGEresult: An S3 data object for Differential Gene Expression analysis

Much like a RangedSummarizedExperiment (RSE), the DGEresult has data slots for row (gene), col (samples), assays (anything with row X sample dimentions) and meta (anything that can't be keyed to row, col or assay).  Unlike the RSE, the DGEresult object can hold any number of row and col data.  While the RSE provides a nice package for raw data (counts and associated annotation), the DGEresult more useful for capturing downstream analysis.  For example, fit objects and topTable results are accommodated by the row base type object.  

When used in conjunction with DGE.Tools2, a workflow dataframe is also maintained within the DGEresult object.  The workflow dataframe records the functions and arguments that create each data element stored in the DGEresult.

There are accessor functions for retrieving data elements, adding data elements and querying what's in the data structure.  Supported functions include:  

* addItem   
* rmItem 
* getItem
* getType 
* dim
* print

## Installation

CRAN and Bioconductor package dependencies should be installed automatically.  However, we use a few 
It is best to run the install from a fresh R session before loading any packages as loaded packages cannot be updated.

```r
    #if you don't have the devtools package already
    install.packages("devtools") 

    #next line required so that missing Bioconductor packages will install
    source("https://bioconductor.org/biocLite.R")

    #you may also need to set proxy information to install from external github accounts
    httr::set_config(httr::use_proxy(url="http://proxy-server.bms.com:8080"))

 
    devtools::install_git("http://biogit.pri.bms.com/thompj27/DGEresult", repos=BiocInstaller::biocinstallRepos()) 
  
```   
