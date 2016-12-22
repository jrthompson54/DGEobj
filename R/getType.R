### Function getType ###
#' Function getType
#'
#' Retrieve one of more data items by type.
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class dgeObj created by function initDGEobj
#' @param type A single type of list of types to retreives.  Enter
#'    showTypes(MyDgeObj) to see a list of allowed types.  See F. addType
#'    to define new types.
#' @return A data item or list of data items
#'
#' @examples
#'    MyContrastList <- getType(dgeObj, type="topTable")
#'    MyRawData <- getType(dgeObj, type=list("counts", "design", "geneData"))
#'
#' @export
#Return all items of a specified type as a list
getType <- function(dgeObj, type){
    #type can be a single named type or a vector or list of types
    idx <- attr(dgeObj, "type") %in% type
    if (sum(idx) == 1)
        result <- dgeObj$data[idx][[1]]
    else
        result <- dgeObj$data[idx]

    if (sum(idx) < length(type))
        warning("Some types were not found")

    return(result)
}
