#' @export
### rmItem
rmItem <- function(dgeObj, itemName){
    if (missing(itemName))
        stop ("itemName is required")
    if (class(itemName) != "character")
        stop("itemName should be class character")
    #item not found
    if (!itemName %in% names(dgeObj$data))
        stop(paste(itemName, " does not exist within DGEresult.", sep=""))

    dgeObj$data[itemName] <- NULL
    dgeObj$type[itemName] <- NULL
    dgeObj$basetype[itemName] <- NULL
    dgeObj$dateCreated[itemName] <- NULL
    dgeObj$funArgs[itemName] <- NULL

    return(dgeObj)
}
