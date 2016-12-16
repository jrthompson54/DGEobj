#Return all items of a specified type as a list
getBaseType <- function(x, ...) UseMethod("getBaseType")
getBaseType.default <- function(dgeResult, ...){
    warning(paste("getBaseType does not know how to handle object of class ",
                  class(dgeResult),
                  "and can only be used on class DGEresult"))
}
getBaseType.DGEresult <- function(dgeResult, baseType){
    baseTypes <- unlist(dgeResult$type)
    idx <- baseType %in% baseTypes
    itemList <- dgeResult$data[idx]
}
