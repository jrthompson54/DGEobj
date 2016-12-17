#Return all items of a specified type as a list
getType <- function(x, ...) UseMethod("getType")
getType.default <- function(dgeResult, ...){
    warning(paste("dim does not know how to handle object of class ",
                  class(dgeResult),
                  "and can only be used on class DGEresult"))
}
getType.DGEresult <- function(dgeResult, type){
    #ype can be a single named type or a vector or list of types
    idx <- dgeResult$type %in% type
    if (sum(idx) == 1)
        result <- dgeResult$data[idx][[1]]
    else
        result <- dgeResult$data[idx]

    if (sum(idx) < length(type))
        warning("Some types were not found")

    return(result)
}
