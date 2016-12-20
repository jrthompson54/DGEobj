
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
              counts ="assay",
              design ="col",
              geneDat ="row",
              isoformDat = "row",
              exonDat = "row",
              topTable = "row",
              topTreat ="meta",
              fit = "row",
              designMatrix = "col",
              geneList = "meta",
              pathway = "meta"
              ),

# These Types can only have one instance in a DGEresult
    uniqueType = c("counts",
                "design",
                "geneDat",
                "isoformDat",
                "exonDat")
)
# Uncomment this block when you need to update the .rda files
# x = getwd()
# setwd ("~/R/lib/pkgsrc/DGEobj/")
# save(.DGEobj, file="./data/DGEobj.rda")
# setwd(x)


### Function DGEobj ###
#' Function DGEobj
#'
#' Initializes an empty DGEobj.  Optionally, add
#' the first data element.
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq
#'
#' @param
#' @param counts A count matrix or dataframe with row and colnames.
#' @param rowDat  Gene, isoform or exon level annotation. rownames must match
#'    rownames in count matrix.
#' @param colDat A dataframe describing the experiment design.  Rownames much match
#'  colnames(counts).
#' @param DGEobjDef An object definition. Defaults to the global DGE object definition
#'     (.DGEobjDef).
#'
#' @return A DGEresult object
#'
#' @examples
#'    # init an empty DGEresult object
#'    myDGEresult <- initDGEobj()
#'    # init and add the first data object.
#'    myDGEresult <- DGEresult(data = mycounts,
#'                    dataName = "counts",
#'                    dataType = "assay")
#'
#' @export
### # Constructor function for the class
initDGEobj <- function(counts, colDat, rowDat,
                       rowDatType="geneDat",
                       DGEobjDef=.DGEobjDef) {
    assert_that(!missing(counts),
                !missing(colDat),
                !missing(rowDat),
                !missing(rowDatType))
    assert_that(is.matrix(counts) | is.data.frame(counts))
    assert_that (rowDatType %in% names(DGEobjDef$type))
    assert_that (!is.null(rownames(counts)), !is.null(rownames(rowDat)))

    #initialize an empty DGEobj and load require items
    dgeObj <- list()
    dgeObj$data <- list()
    dgeObj$type <- list()
    dgeObj$basetype <- list()
    dgeObj$dateCreated <- list()
    dgeObj$funArgs <- list()
    dgeObj$objDef <- DGEobjDef
    class(dgeObj) <- "DGEobj"

    dgeObj <- addItem(dgeObj, counts, "counts", "counts")
    dgeObj <- addItem(dgeObj, colDat, "Design", "design")
    switch(rowDatType,
           "geneDat" = {
               dgeObj <- addItem(dgeObj, rowDat, "geneDat", "geneDat")},
           "isoformDat" = {
               dgeObj <- addItem(dgeObj, rowDat, "isoformDat", "isoformDat")},
           "exonDat" = {
               dgeObj <- addItem(dgeObj, rowDat, "exonDat", "exonDat")}
    )

    attr(dgeObj, "Dim") <- c(nrow(counts), ncol(counts))

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


