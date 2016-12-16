### rmItem
rmItem <- function(x, ...) UseMethod("rmItem")
rmItem.default <- function(dgeResult, ...)
{
    warning(paste("rmItem does not know how to handle object of class ",
                  class(dgeResult),
                  "and can only be used on class DGEresult"))
}
rmItem.DGEresult <- function(dgeResult, itemName){
    if (missing(dgeResult) | missing(itemName))
        stop ("Both dgeResult and itemName are required")
    if (!class(dgeResult) == "DGEresult")
        stop("dgeResult must be class DGEresult")
    if (!class(itemName) == "character")
        stop("itemName should be class character")
    #item not found
    if (!itemName %in% names(dgeResult$data))
        stop(paste(itemName, " does not exist within DGEresult.", sep=""))

    dgeResult$data[itemName] <- NULL
    dgeResult$type[itemName] <- NULL
    dgeResult$basetype[itemName] <- NULL
    dgeResult$dateCreated[itemName] <- NULL
    dgeResult$funArgs[itemName] <- NULL

    return(dgeResult)
}
