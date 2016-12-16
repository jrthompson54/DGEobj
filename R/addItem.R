### addItem
addItem <- function(x, ...) UseMethod("addItem")
addItem.default <- function(dgeResult, ...){
    warning(paste("addItem does not know how to handle object of class ",
                  class(dgeResult),
                  "and can only be used on class DGEresult"))
}
addItem.DGEresult <- function(dgeResult, item, itemName, itemType,
                              overwrite=FALSE, funArgs=match.call()){

    if (missing(item) | missing(itemName) | missing(itemType))
        stop("All Three of item, itemName, itemType are required")

    #enforce itemType
    if (!itemType %in% names(.type))
        stop(paste("itemType must be one of: ",
                   paste(names(.type), collapse=", "), sep=""))

    #check for disallowed second instance of uniqueTypes (unless overwrite mode)
    if(itemType %in% .uniqueType  &
       itemType %in% dgeResult$type &
       overwrite==FALSE)
        stop (paste( "Only one instance of type ", itemType, " allowed.",
                     " Use a base type instead (row, col, assay, meta)",
                     sep=""))

    #convert call objects to text
    if (class(funArgs) == "call")
        funArgs <- paste(funArgs, collapse=" ")

    #refuse to add if itemName exists already unless overwrite = T
    if (overwrite==FALSE & itemName %in% names(dgeResult$data))
        stop('itemName already exists in DGEresult!')
    #confirm dimensions consistent before adding
    else if (.dimensionMatch(dgeResult, item, itemType) == TRUE){

            print("Adding Item")
            dgeResult$data[[itemName]] <- item
            dgeResult$type[[itemName]] <- itemType
            dgeResult$basetype[[itemName]] <- .type[[itemType]]
            dgeResult$dateCreated[[itemName]] <- lubridate::now()
            dgeResult$funArgs[[itemName]] <- funArgs
    }

    return(dgeResult)
}
#addItem
