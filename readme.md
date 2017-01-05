# DGEobj: An S3 data object to capture results from Differential Gene Expression analysis

**Note: DGEobj is in early beta phase.  Most of all of the implemented tools work.  Some tools, notably a subsetting function, are not in place yet.  Check
back often.  I'll increment the version number each time I make changes.**

DGEobj is an S3 data class that provides a flexible container for DGE analysis results.  
The DGEobj is designed to be extensible allowing definition of new data types as needed. 
A set of accessory functions to deposit, query and retrieve subsets of a data workflow has been provided.  Attributes are used to capture metadata such as species and genemodel and including reproducibility information such that a 3rd party can access a DGEobj history to see how each data object was created. 

Structurally, much like a RangedSummarizedExperiment (RSE), the DGEobj has data slots for row (gene), col (samples), assays (anything with row X sample dimensions) and meta (anything that can't be keyed to row, col or assay).  There is also a rowvector and colvector slot for 1 dimensional objects keyed on row or col data.  One of the motivations for creating the DGEobj data structure is that the RSE only allows one data item in the row and col slots.  The DGEresult object can hold any number of row and col data objects.  Thus, the DGEobj data structure is suitable for capturing multiple steps of a downstream analysis.  For example, fit objects and topTable results are accommodated by the row type object.  

To maintain a clear thread of the relationships between data object, many of the object types are defined as unique which means only one instance of that type may be added to the DGEobj.  In this way it is clear that contrasts deposited in a DGEobj are derived from the single fit object stored in the same DGEobj.  This prevents ambiguity about how the data was generated. But it also limits a DGEobj to one analysis thread.  If, for example, you want to fit two different models to a dataset, you would duplicate the DGEobj containing the normalized data and create separate DGEobj for each fit.  Based on feedback from Don, we have a feature request to support multiple fits and associate downstream topTable results with its parent fit object via assigned attributes.   But for the time being, a DGEobj should represent a single thread of analysis.  

## Structure of a DGEobj

A DGE obj is fundamentally a simple list of data objects. Each data object deposited in a DGEobj is accompanied by attributes including a Type, a Basetype, a dataCreated and funArgs text field.  Example data objects that are useful to capture as documentation of an analysis include, design matrices, DGElists, fit objects, topTable output etc. 

There are 4 fundamental and immutable basetypes (row, col, assay, meta) and two derivative basetypes (rowvector, colvector).  These are used under the hood to define how to subset each data element.  

To provide flexibility there are number of predefined types (showTypes()) and a newType function provides extensibility to create new data types as needed.  Each type must be associated with a basetype. Except for the unique fields described above, you can have multiple instances of any Type as long as each instance is given a unique "itemName".  A data structure defining a DGEobj's customized structure is stored as the "objDef" attribute on the DGEobj (myObjDef <- attr(dgeObj, "objDef")).  

funArgs is a text field intended to hold details of creating the object.  Passing funArgs = match.call() is a convenient way to automate capture of the calling arguments of the current function when that best describes how an object was created.  The user can also supply a custom user authored line for this purpose.
  
## Supporting functions include:  

### Manipulate DGEObj  

* initDGEobj: Initialize a new DGEobj (requires counts, design and geneDat (or isoformDat or exonDat)
* addItem:  Add a data item to a DGEobj  
* rmItem:  Remove a data item from a DGEobj  
* newType:  Define a new data type  
* setAttributes: Allows setting multiple attributes (facilitates attribute templates)

### Query Functions  

* dim: Return the dimensions of the DGEobj (the assay dimensions) 
* length: Returns the number of stored data items 
* dimnames:  Return the row (gene) and column (sample) names  
* itemNames:  Return a character vector of data item names  
* showTypes:  Show the type definitions of a DGEobj (all currently defined  types)  
* print:  Print a summary of the contents of a DGEobj, date created and optionally the funArg history  

### Data retrieval  

* getItem:  Return a data item(s) by itemname
* getItems:  Return data items by  a list of itemnames
* getType:  Return data item(s) by itemType
* getBaseType:  Return data item(s) by baseType

### Conversion

as.list:  unclass a DGEobj to simple list
as.RSE:  (pending) convert basedata from DGEobj to RangedSummarizedExperiment format
as.ES:   (pending) convert basedata from DGEobj to ExpressionSet format
as.DGEO  (pending) convert basedata from an RSE object to a DGEobject

### GRanges Data

If the geneData object (row annotatioon) contains chromosome position information (chr, start, end, strand), a GRanges object will also be created.  

## Installation

CRAN and Bioconductor package dependencies should be installed automatically.  However, we use a few 
It is best to run the install from a fresh R session before loading any packages as loaded packages cannot be updated.

```r
    #if you don't have the devtools package already
    install.packages("devtools") 

    #next line required so that missing Bioconductor packages will install
    source("https://bioconductor.org/biocLite.R")

    devtools::install_git("http://biogit.pri.bms.com/thompj27/DGEobj", repos=BiocInstaller::biocinstallRepos()) 
  
```   
