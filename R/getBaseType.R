### Function getBaseType ###
#' Function getBaseType
#'
#' Accessor function for DGEobj class objects.  Retrieves all data items of a
#' given basetype or list of basetypes.
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class dgeObj created by function initDGEobj
#' @param baseType One of: ["row", "col", "assay", "meta"]
#'
#' @return A simple list of data items
#'
#' @examples
#'    Assays <- getBaseType(dgeObj, baseType="assay")
#'    AssaysAndGeneAnnotation <- getBaseType(degObj, c("assay", "row"))
#'
#' @export
#Return all items of a specified type as a list
getBaseType <- function(dgeObj, baseType){

    if (missing(baseType))
        stop("baseType argument is required")

    if (!baseType %in% attr(dgeObj, "objDef")$basetype)
        stop(paste("baseType must be one of: ",
                paste(attr(dgeObj, "objDef")$basetype, collapse=", "),
                sep=""))

    idx <- attr(dgeObj, "basetype") %in% baseType

    if (sum(idx) < length(baseType))
        warning("Some baseTypes were not found")

    if (sum(idx) == 1)
        result <- dgeObj$data[idx][[1]]
    else
        result <- dgeObj$data[idx]
    return(result)
}
