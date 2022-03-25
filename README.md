# DGEobj: An S3 data object to capture results from Differential Gene Expression analysis

<!-- badges: start -->
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/DGEobj?color=9bc2cf)](https://cran.r-project.org/package=DGEobj) 
[![CRAN_Downloads_Badge](https://cranlogs.r-pkg.org/badges/grand-total/DGEobj?color=9bc2cf)](https://cran.r-project.org/package=DGEobj)
[![CircleCI](https://circleci.com/gh/cb4ds/DGEobj/tree/master.svg?style=svg)](https://app.circleci.com/pipelines/github/cb4ds/DGEobj?branch=master)
[![Codecov test coverage](https://codecov.io/gh/cb4ds/DGEobj/branch/master/graph/badge.svg)](https://app.codecov.io/gh/cb4ds/DGEobj?branch=master)
<!-- badges: end -->

DGEobj is an S3 data class that provides a flexible container for Differential Gene Expression (DGE) analysis results.  The DGEobj class is designed to be extensible. Thus, while designed with RNA-Seq analysis workflows in mind, The DGE object data structure is suitable for  allowing definition of new data types as needed. A set of accessory functions to deposit, query and retrieve subsets of a data workflow has been provided.  Attributes are used to capture metadata such as species and gene model, including reproducibility information such that a 3rd party can access a DGEobj history to see how each data object was created or modified. 

Operationally, the DGEobj is styled after the RangedSummarizedExperiment (RSE).  The DGEobj has data slots for row (gene), col (samples), assays (anything with n-rows by m-samples dimensions) and metadata (anything that can't be keyed to row, col or assay).  The key motivations for creating the DGEobj data structure is that the RSE only allows one data item each in the row and col slots and thus is unsuitable for capturing the plethora of data objects created during a typical DGE workflow.   The DGEobj data structure can hold any number of row and col data objects and thus is engineered for capturing the multiple steps of a downstream analysis.

Certain object types, primarily the count matrix and associated row and column info, are defined as unique which means only one instance of that type may be added to the DGEobj.  

When multiple objects of one type are included in a DGEobj (e.g. two different fits), the concept of parent attributes is used to associate downstream data objects (e.g. contrasts) with the appropriate data object they are derived from.  

### Structure of a DGEobj

A DGE obj is fundamentally a list of data objects. Each data object deposited in a DGEobj is accompanied by attributes including a Type, a baseType, a dateCreated and funArgs text field.  Data objects that are useful to capture as documentation of an analysis include design matrices, fit objects, topTable output etc. 

There are four fundamental and immutable baseType: *row, col, assay, meta*.  These are used under the hood to define how to subset each data element.  

To provide flexibility there are number of predefined types (use *showTypes()*) and the newType function provides extensibility to create new data types as needed.  Each type is associated with a baseType and except for the unique fields described above, you can have multiple instances of any type as long as each instance is given a unique name.  A data structure defining the DGEobj customized structure is stored as the "objDef" attribute on the DGEobj.

The funArgs text field intended to hold details from creating the object.  Passing funArgs = match.call() is a convenient way to automate capture of the calling arguments of the current function when that best describes how an object was created.  The user can also supply a custom user-authored text comment for this purpose.
  
### Supporting functions include:  

#### Manipulate DGEObj  

* **initDGEobj**: Initialize a new DGEobj - requires counts, design, and gene (or isoform or exon) data
* **addItem/s**:  Add a data item to a DGEobj  
* **rmItem/s**:  Remove a data item from a DGEobj  
* **newType**:  Define a new data type  
* **setAttribute/s**: Allows setting attributes (facilitates attribute templates)
* **resetDGEobj**: Produces a new DGEobj with the original unfiltered data

#### Query Functions  

* **dim**: Return the dimensions of the DGEobj (the assay dimensions) 
* **dimnames**:  Return the row (gene) and column (sample) names  
* **showTypes**:  Show the type definitions of a DGEobj (all currently defined  types)  
* **inventory**:  Print a summary of the contents of a DGEobj, date created and optionally the funArgs history  

#### Data retrieval  

* **getItem/s**:  Return a data item(s) by item name
* **getType**:  Return data item(s) by item type
* **getBaseType**:  Return data item(s) by baseType
* **getAttribute/s**: Return attributes

#### Conversion

* **as.list**:  unclass a DGEobj to simple list

#### GRanges Data

If the gene data object (row annotation) contains chromosome position information (name, start, end, strand), a GRanges object will also be created. 

#### Original Data 

During initialization, a copy of the counts, gene annotation and sample annotation is duplicated and stored in the meta slot with an "_orig" suffix on the itemName.  This preserves the original data if you subset the original data. To restore select attributes including primary parts of the originally initialized DGEobj, you can reset the object.
