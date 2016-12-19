#' @export
#Return all items of a specified type as a list
getBaseType <- function(dgeObj, baseType){

    if (missing(baseType))
        stop("baseType argument is required")

    if (!baseType %in% .DGEobjDef$basetype)
        stop(paste("baseType must be one of: ",
                paste(.DGEobjDef$basetype, collapse=", "),
                sep=""))

    idx <- dgeObj$basetype %in% baseType

    if (sum(idx) < length(baseType))
        warning("Some baseTypes were not found")

    if (sum(idx) == 1)
        result <- dgeObj$data[idx][[1]]
    else
        result <- dgeObj$data[idx]
    return(result)
}
