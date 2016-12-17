#Return all items of a specified type as a list
getBaseType <- function(x, ...) UseMethod("getBaseType")
getBaseType.default <- function(dgeResult, ...){
    warning(paste("getBaseType does not know how to handle object of class ",
                  class(dgeResult),
                  "and can only be used on class DGEresult"))
}
getBaseType.DGEresult <- function(dgeResult, baseType){

    if (missing(baseType))
        stop("baseType argument is required")

    if (!baseType %in% .basetype)
        stop("baseType must be one of: row, col, assay, meta")

    idx <- dgeResult$basetype %in% baseType

    if (sum(idx) < length(baseType))
        warning("Some baseTypes were not found")

    if (sum(idx) == 1)
        result <- dgeResult$data[idx][[1]]
    else
        result <- dgeResult$data[idx]
    return(result)
}
