#Return all items of a specified type as a list
getType <- function(x, ...) UseMethod("getType")
getType.default <- function(dgeResult, ...){
    warning(paste("dim does not know how to handle object of class ",
                  class(dgeResult),
                  "and can only be used on class DGEresult"))
}
getType.DGEresult <- function(dgeResult, type){
    idx <- dgeResult$type %in% type
    itemList <- dgeResult$data[idx]
}
