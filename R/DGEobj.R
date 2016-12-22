
### DGEobj object definition

### Define an extensible datatype
### There are 4 immutable base type: row, col, assay, meta
### Each type must be one of these four basetypes
### Extensibility: Additional types can be added to .DGEobj$type as long as they are
### assigned to one of the 4 basetypes.
###
### Data Definitions
.DGEobjDef <- list(
    basetype = c(row="row",
                   col="col",
                   assay="assay",
                   meta="meta"),
    #the value of .type is a basetype
    type = c(row="row",
             col="col",
             assay ="assay",
             meta ="meta",

             geneData ="row",
             isoformData = "row",
             exonData = "row",
             topTable = "row",
             fit = "row",

             design ="col",
             designMatrix = "col",

             counts ="assay",
             Log2CPM = "assay",
             TPM = "assay",
             FPKM = "assay",
             zFPKM = "assay",

             topTreat ="meta",
             geneList = "meta",
             pathway = "meta",
             URL = "meta"

              ),

# These Types can only have one instance in a DGEobj
    uniqueType = c("counts",
                "design",
                "geneData",
                "isoformData",
                "exonData"),

    allowedLevels = c("gene", "isoform", "exon")
)
# Uncomment this block when you need to update the ./data/DGEobj.rda file
# x = getwd()
# setwd ("~/R/lib/pkgsrc/DGEobj/")
# save(.DGEobj, file="./data/DGEobj.rda")
# setwd(x)


### Function initDGEobj ###
#' Function initDGEobj
#'
#' Initializes DGEobj with first data item.
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq
#'
#' @param counts A count matrix or dataframe with row and colnames.
#' @param rowData  Gene, isoform or exon level annotation. rownames must match
#'    rownames in count matrix.
#' @param colData A dataframe describing the experiment design.  Rownames much match
#'  colnames(counts).
#' @param DGEobjDef An object definition. Defaults to the global DGE object definition
#'     (.DGEobjDef) and you usually shouldn't change this.
#' @param customAttr An optional (but highly recommended) named list of attributes
#'     to assign to the DGEobj.
#'
#' @return A DGEobj object
#'
#' @examples
#'    # initialize a DGEobj
#'    myDGEresult <- DGEresult(counts = MyCounts,
#'                             rowData = MyGeneAnnotation,
#'                             colData = MyDesign,
#'                             level = "gene",
#'                             customAttr = list (PID = "20161025-0001",
#'                                                XpressID = "12345")
#'                            )
#'
#' @import assertthat magrittr
#'
#' @export
### # Constructor function for the class
initDGEobj <- function(counts, colData, rowData, #required
                       level,   #one of gene, isoform or exon
                       DGEobjDef=.DGEobjDef,
                       customAttr #optional list of named Attr/Value pairs
                       ) {
    assert_that(!missing(counts),
                !missing(colData),
                !missing(rowData),
                !missing(level))

    assert_that((is.matrix(counts) | is.data.frame(counts)),
                level %in% DGEobjDef$allowedLevels,
                !is.null(rownames(counts)),
                !is.null(colnames(counts)),
                !is.null(rownames(rowData))
                )

    assert_that(level %in% DGEobjDef$allowedLevels)

    #some reality checking before we build the DGEobj
    #rownames annotation = rownames on counts
    #   if not and same length, sort to try and align the data
    # similarly rownames in colData must match colnames in counts
    assert_that(nrow(counts) == nrow(rowData),
                ncol(counts) == nrow(colData))
    #if we're here, lengths match up, now check names
    if (!all(rownames(counts) == rownames(rowData))){
        #sort both by rowname
        counts <- counts[order(rownames(counts)),]
        rowData <- rowData[order(rownames(rowData)),]
    }
    if (!all(colnames(counts) == rownames(colData))){
        #sort both as appropriate
        counts <- counts[,order(colnames(counts))]
        colData <- colData[order(rownames(colData)),]
    }
    #now everything should be sorted properly check one last time
    assert_that(
        all(rownames(counts) == rownames(rowData)),
        all(colnames(counts) == rownames(colData))
        )

    #all our data are properly aligned; build the DGEobj

    #initialize an empty DGEobj
    dgeObj <- list()
    dgeObj$data <- list()
    class(dgeObj) <- "DGEobj"

    attr(dgeObj, "type") <- list()
    attr(dgeObj, "basetype") <- list()
    attr(dgeObj, "dateCreated") <- list()
    attr(dgeObj, "funArgs") <- list()
    attr(dgeObj, "objDef") <- DGEobjDef

    # dgeObj$type <- list()
    # dgeObj$basetype <- list()
    # dgeObj$dateCreated <- list()
    # dgeObj$funArgs <- list()
    # dgeObj$objDef <- DGEobjDef

    #load required items
    dgeObj <- addItem(dgeObj, counts, "counts", "counts",
                      funArgs = match.call())
    dgeObj <- addItem(dgeObj, colData, "Design", "design",
                      funArgs = match.call())
    switch(level,
           "gene" = {
               dgeObj <- addItem(dgeObj, rowData, "geneData", "geneData",
                                 funArgs = match.call())
               attr(dgeObj, "level") <- "gene"
               },
           "isoform" = {
               dgeObj <- addItem(dgeObj, rowData, "isoformData", "isoformData",
                                 funArgs = match.call())
               attr(dgeObj, "level") <- "isoform"
               },
           "exon" = {
               dgeObj <- addItem(dgeObj, rowData, "exonData", "exonData",
                                 funArgs = match.call())
               attr(dgeObj, "level") <- "exon"
               }
    )

    attr(dgeObj, "assayDim") <- c(nrow(counts), ncol(counts))
    attr(dgeObj, "assayDimnames") <- list(rownames=rownames(counts),
                                     colnames=colnames(counts))

    if (!missing(customAttr))
        for (i in 1:length(customAttr))
            attr(dgeObj, names(customAttr[i])) <- customAttr[[i]]

    return (dgeObj)
}


# DGEobj <- function(data, dataName, dataType) {
#
#     #initialize an empty DGEresult and optionally add the first item
#     dgeObj <- list()
#     dgeObj$data <- list()
#     dgeObj$type <- list()
#     dgeObj$basetype <- list()
#     dgeObj$dateCreated <- list()
#     dgeObj$funArgs <- list()
#     dgeObj$objDef <- .DGEobj
#     class(dgeObj) <- "DGEobj"
#
#     #optionally load the first item into dgeObj
#     if (!missing(data) & !missing(dataName) & !missing(dataType))
#         dgeObj <- addItem.DGEresult(dgeObj, data, dataName, dataType)
#
#     return (dgeObj)
# }
    #x is a list ot POSIXct datetimes
    #names(unclass(y))
    # [1] "sec"    "min"    "hour"   "mday"   "mon"    "year"   "wday"   "yday"   "isdst"  "zone"   "gmtoff"
    # > y[["hour"]]


##  To Do
##
##      subsetting
##      as.RSE
##      as.ES
##
##
##      FunctionListlo


