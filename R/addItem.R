### addItem
### items must have row and column names
###
#' @export
addItem <- function(dgeObj, item, itemName, itemType,
                              overwrite=FALSE, funArgs=match.call()
                              ){
    assert_that(!missing(dgeObj),
                !missing(item),
                !missing(itemName),
                !missing(itemType),
                !is.null(rownames(item))
    )

    #enforce itemType
    types <- names(attr(dgeObj, "objDef")$type)
    if (!itemType %in% types)
        stop(paste("itemType must be one of: ",
                   paste(types, collapse=", "), sep=""))

    #refuse to add if itemName exists already unless overwrite = T
    if (overwrite==FALSE & itemName %in% names(dgeObj$data))
        stop('itemName already exists in DGEobj!')

    #check for disallowed second instance of uniqueTypes (unless overwrite mode)
    uniqueTypes <- attr(dgeObj, "objDef")$uniqueType
    if(itemType %in% uniqueTypes  &
       itemType %in% attr(dgeObj, "type") &
       overwrite==FALSE)
        stop (paste( "Only one instance of type ", itemType, " allowed.",
                     " Use a base type instead (row, col, assay, meta),",
                     " or define a new type.", sep=""))

    #convert call objects to text
    if (class(funArgs) == "call")
        # funArgs <- paste(funArgs, collapse="; ")
        funArgs <- paste(funArgs[[1]], "(",
                        paste(funArgs[2:length(funArgs)], collapse=", "),
                        ")", sep="")

    #confirm dimensions consistent before adding
    if (.dimensionMatch(dgeObj, item, itemType) == FALSE)
        stop("item doesn't match dimension of dgeObj")

    #ready to add the item
    # print("Adding Item")
    dgeObj$data[[itemName]] <- item
    # dgeObj$type[[itemName]] <- itemType
    #
    # add the type attribute to a named list
    attr(dgeObj, "type")[[itemName]] <- itemType

    #dgeObj$basetype[[itemName]] <- dgeObj$objDef$type[[itemType]]
    attr(dgeObj, "basetype")[[itemName]] <- attr(dgeObj, "objDef")$type[[itemType]]

    #dgeObj$dateCreated[[itemName]] <- lubridate::now()
    attr(dgeObj, "dateCreated")[[itemName]] <- lubridate::now()

    #dgeObj$funArgs[[itemName]] <- funArgs
    attr(dgeObj, "funArgs")[[itemName]] <- funArgs

    return(dgeObj)
} #addItem
