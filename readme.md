# DGEobj: An S3 data object to capture results from Differential Gene Expression analysis

DGEobj is an S3 data class that provides a flexible container for DGE analysis results.  
The DGEobj is designed to be extensible allowing definition of new data types as needed. 
A set of accessory functions to deposit, query and retrieve subsets of a data workflow has been provided.  Attributes are used to capture metadata such as species and genemodel and including reproducibility information such that a 3rd party can access a DGEobj history to see how each data object was created. 

Operationally, the DGEobj is styled after the RangedSummarizedExperiment (RSE).  The DGEobj has data slots for row (gene), col (samples), assays (anything with n rows X m samples dimensions) and meta data (anything that can't be keyed to row, col or assay).  The key motivations for creating the DGEobj data structure is that the RSE only allows one data item each in the row and col slots and thus is unsuitable for capturing the plethora of data objects created during a typical DGE workflow.   The DGEobj data structure then can hold any number of row and col data objects.  Thus, the DGEobj data structure is engineered for capturing multiple steps of a downstream analysis.  For example, fit objects and topTable results are accommodated by the row type object. 

Certain object types, primarily the count matrix and associated row and column info, are defined as unique which means only one instance of that type may be added to the DGEobj.  

When multiple objects of one type are included in a DGEobj (e.g. two different fits), the concept of parent attributes is used to associate downstream data objects (e.g. contrasts) with the appropriate data object they are derived from.  

## Structure of a DGEobj

A DGE obj is fundamentally a list of data objects. Each data object deposited in a DGEobj is accompanied by attributes including a Type, a Basetype, a dateCreated and funArgs text field.  Example data objects that are useful to capture as documentation of an analysis include, design matrices, DGElists, fit objects, topTable output etc. 

There are 4 fundamental and immutable basetypes (row, col, assay, meta).  These are used under the hood to define how to subset each data element.  

To provide flexibility there are number of predefined types (showTypes() to list) and a newType function provides extensibility to create new data types as needed.  Each type must be associated with a basetype. Except for the unique fields described above, you can have multiple instances of any Type as long as each instance is given a unique "itemName".  A data structure defining a DGEobj's customized structure is stored as the "objDef" attribute on the DGEobj (myObjDef <- attr(dgeObj, "objDef")).  

funArgs is a text field intended to hold details of creating the object.  Passing funArgs = match.call() is a convenient way to automate capture of the calling arguments of the current function when that best describes how an object was created.  The user can also supply a custom user-authored text comment for this purpose.
  
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
* inventory:  Print a summary of the contents of a DGEobj, date created and optionally the funArg history  

### Data retrieval  

* getItem:  Return a data item(s) by itemname
* getItems:  Return data items by  a list of itemnames
* getType:  Return data item(s) by itemType
* getBaseType:  Return data item(s) by baseType

### Conversion

* as.list:  unclass a DGEobj to simple list
* convertDGEobj:  convert base data (counts, gene and sample annotation) 
from DGEobj to RangedSummarizedExperiment or ExpressionSet
* convertRSE:    convert base data (counts, gene and sample annotation) 
from RSE to DGEobj or ExpressionSet

### GRanges Data

If the geneData object (row annotatioon) contains chromosome position information (chr, start, end, strand), a GRanges object will also be created. 

### Original Data 

During initialization, a copy of the counts, gene annotation and sample annotation is duplicated and stored in the meta slot with an "_orig" suffix on the itemName.  This preserves the original data after you have subsetted the original data. 

## Installation 

It is best to run the install from a fresh R session before loading any
packages because loaded packages cannot be updated.

Charles Tilford has created a BMS repository, BRAN.  Packages from BRAN
can now be installed simply with install.packages (after a little setup).

One time setup for BRAN :

```
   # Sourcing this file modifies your .Rprofile file to add BRAN to your list of repositories
   source('http://bran.pri.bms.com/resources/configureRepo.R')
```

Install or update DGEobj:  

```
    install.packages("DGEobj", repos="http://BRAN.pri.bms.com")
```

## Installation from Biogit (backup method)

You can also install directly from GIT.  Versions released to BRAN are tagged version numbers.  

```r
    require(devtools)
    devtools::install_git("http://biogit.pri.bms.com/thompj27/DGEobj", repos=BiocInstaller::biocinstallRepos()) 
  
```   
