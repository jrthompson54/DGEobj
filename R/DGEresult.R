
#' ### DGEresult object definition

### Define an extensible datatype
### There are 4 immutable base type: row, col, assay, meta
### Each type must be one of these four basetypes
### Additional types can be added to .type as long as they are
### assigned to one of the 4 basetypes.
###
### Data Definitions
.basetype <- c(row="row",
               col="col",
               assay="assay",
               meta="meta")
#the value of .type is a basetype
.type <- c(row="row",
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
              )

# These Types can only have one instance in a DGEresult
.uniqueType <- c("counts",
                "design",
                "geneAnno",
                "transcriptAnno")
# Uncomment this block when you need to update the .rda files
# x = getwd()
# setwd ("~/R/lib/pkgsrc/DGE.Tools2/")
# save(.type, file="./data/type.rda")
# save(.basetype, file="./data/basetype.rda")
# save(.uniqueType,  file="./data/uniqueType.rda")
# setwd(x)


### Function DGEresult ###
#' Function DGEresult
#'
#' Initializes an empty DGEresult object.  Optionally, add
#' the first data element.
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq
#'
#' @param data A parcel of data to be added to the DGEresult object.
#' @param dataName  A unique name for the data parcel
#' @param dataType A data type identifier. See .
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
DGEresult <- function(data, dataName, dataType) {

    #initialize an empty DGEresult and optionally add the first item
    dgeResult <- list()
    dgeResult$data <- list()
    dgeResult$type <- list()
    dgeResult$basetype <- list()
    dgeResult$dateCreated <- list()
    dgeResult$funArgs <- list()
    class(dgeResult) <- "DGEresult"

    #optionally load the first item into dgeResult
    if (!missing(data) & !missing(dataName) & !missing(dataType))
        dgeResult <- addItem.DGEresult(dgeResult, data, dataName, dataType)

    return (dgeResult)
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


