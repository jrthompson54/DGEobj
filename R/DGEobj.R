
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
              geneAnno ="row",
              transcriptAnno = "row",
              topTable = "row",
              topTreat ="meta",
              fit = "row",
              DGEList = "row",
              designMatrix = "col"
              ),

# These Types can only have one instance in a DGEresult
    uniqueType = c("counts",
                "design",
                "geneAnno",
                "transcriptAnno")
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
#' @param data A parcel of data to be added to the DGEresult object.
#' @param dataName  A unique name for the data parcel
#' @param dataType A data type identifier. See .DGEobj$type
#'
#' @return A DGEresult object
#'
#' @examples
#'    # init an empty DGEresult object
#'    myDGEresult <- DGEresult()
#'    # init and add the first data object.
#'    myDGEresult <- DGEresult(data = mycounts,
#'                    dataName = "counts",
#'                    dataType = "assay")
#'
#' @export
### # Constructor function for the class
DGEobj <- function(data, dataName, dataType) {

    #initialize an empty DGEresult and optionally add the first item
    dgeObj <- list()
    dgeObj$data <- list()
    dgeObj$type <- list()
    dgeObj$basetype <- list()
    dgeObj$dateCreated <- list()
    dgeObj$funArgs <- list()
    dgeObj$objDef <- .DGEobj
    class(dgeObj) <- "DGEobj"

    #optionally load the first item into dgeObj
    if (!missing(data) & !missing(dataName) & !missing(dataType))
        dgeObj <- addItem.DGEresult(dgeObj, data, dataName, dataType)

    return (dgeObj)
}

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


