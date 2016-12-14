### addItem
addItem <- function(x, ...) UseMethod("addItem")
addItem.default <- function(dgeResult, ...){
    warning(paste("addItem does not know how to handle object of class ",
                  class(dgeResult),
                  "and can only be used on class DGEresult"))
}
addItem.DGEresult <- function(dgeResult, item, itemName, itemType,
                              overwrite=FALSE, funArgs=match.call()){
    #enforce itemType
    if (!itemType %in% names(.type))
        stop(paste("itemType must be one of: ",
                   paste(names(.type), collapse=", "), sep=""))

    #add item and assign it's type
    #refuse to add if it exists already unless overwrite = T
    if (overwrite==FALSE & with(dgeResult$data, exists(itemName)))
        stop('itemName already exists in DGEresult!')
    #confirm dimensions consistent before adding
    else if (.dimensionMatch(dgeResult, item, itemType) == TRUE){
        dgeResult$data[[itemName]] <- item
        dgeResult$type[[itemName]] <- itemType
        dgeResult$basetype[[itemName]] <- .type[[itemType]]
        dgeResult$dateCreated[[itemName]] <- lubridate::now()
        dgeResult$funArgs[[itemName]] <- match.call()
    }

    return(dgeResult)
} #addItem
