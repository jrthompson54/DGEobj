# DGEobj: An S3 data object to capture results from Differential Gene Expression analysis

Much like a RangedSummarizedExperiment (RSE), the DGEresult has data slots for row (gene), col (samples), assays (anything with row X sample dimentions) and meta (anything that can't be keyed to row, col or assay).  Unlike the RSE, the DGEresult object can hold any number of row and col data.  Thus, the DGEobj data structure is suitable for capturing downstream analysis.  For example, fit objects and topTable results are accommodated by the row base type object.  Some types are defined as unique.  A DGEobj can only hold one instance of a unique type.  counts, design, geneDat, isoformDat and exonDat are all unique types.  

Each data object deposited in a DGEobj is accompanied by attributes including a Type, a basetype, a dataCreated and funArgs text field.  funArgs is a text field that to hold details creating the object.  Passing funArgs = match.call() is a convenient way to capture the arguments of the current function.

There are accessor functions to build, add/remove items and retrieve individual items or groups of items.  
Supported functions include:  


* initDGEobj: Initialize a new DGEobj (requires counts, design and geneDat (or isoformDat or exonDat)
* addItem.R:  Add a data item to a DGEobj
* rmItem:  Remove a data item from a DGEobj
* dim.R: Return the dimensions of the DGEobj
* dimnames:  Return the row and column names
* itemNames:  Return a character vector of data item names
* getItem:  Return one or more data items by itemname
* getType:  Return one or more data items by itemType
* getBaseType:  Return one or more data items by baseType
* newType:  Define a new data type
* print:  Print a summary of the contents of a DGEobj, date created and optionally the funArg history
* showTypes:  Show the type definitions of a DGEobj

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

    devtools::install_git("http://biogit.pri.bms.com/thompj27/DGEobj", repos=BiocInstaller::biocinstallRepos()) 
  
```   
