### addItem
### items must have row and column names
###
#' @export
addItem <- function(dgeObj, item, itemName, itemType,
                              overwrite=FALSE, funArgs=match.call()
                              ){

    if (missing(item) | missing(itemName) | missing(itemType))
        stop("All Three of item, itemName, itemType are required")

    #item must have rownames
    if (is.null(rownames(item)))
        stop ("item is missing rownames!")

    #enforce itemType
    if (!itemType %in% names(dgeObj$objDef$type))
        stop(paste("itemType must be one of: ",
                   paste(names(dgeObj$objDef$type), collapse=", "), sep=""))

    #check for disallowed second instance of uniqueTypes (unless overwrite mode)
    if(itemType %in% dgeObj$objDef$uniqueType  &
       itemType %in% dgeObj$type &
       overwrite==FALSE)
        stop (paste( "Only one instance of type ", itemType, " allowed.",
                     " Use a base type instead (row, col, assay, meta)",
                     sep=""))

    #convert call objects to text
    if (class(funArgs) == "call")
        funArgs <- paste(funArgs, collapse=" ")

    #refuse to add if itemName exists already unless overwrite = T
    if (overwrite==FALSE & itemName %in% names(dgeObj$data))
        stop('itemName already exists in DGEobj!')
    #confirm dimensions consistent before adding
    else if (.dimensionMatch(dgeObj, item, itemType) == TRUE){

            # print("Adding Item")
            dgeObj$data[[itemName]] <- item
            dgeObj$type[[itemName]] <- itemType
            dgeObj$basetype[[itemName]] <- dgeObj$objDef$type[[itemType]]
            dgeObj$dateCreated[[itemName]] <- lubridate::now()
            dgeObj$funArgs[[itemName]] <- funArgs
    }

    if (is.null(rownames(item)))
        warning ("No rownames assigned")

    return(dgeObj)
}
#addItem
