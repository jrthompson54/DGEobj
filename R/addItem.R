### Function addItem ###
#' Function addItem (DGEobj)
#'
#' Add a data item to a class DGEobj
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class dgeObj created by function initDGEobj
#' @param item  Required. The data item to be deposited in the DGEobj
#' @param itemName Required.  A user assigned name for this data item
#' @param itemType Required.  A type attribute.  See showTypes to see the
#'     pre-defined types. Types are extensible with the newType function.
#' @param overwrite Default = FALSE.  Set to TRUE to overwrite the data object
#'     stored in the itemName slot
#' @param funArgs A text field to annotate how the data object was created.
#'    If you pass the result of match.call() as this argument, it captures the
#'    name and arguments used in the current function.
#'
#' @return A DGEobj class object with a new item added.
#'
#' @examples
#'    myFunArgs <- match.call() #capture calling function and arguments
#'    showTypes()  #See what predefined types are available
#'    myDgeObj <- addItem(myDgeObj, item = MyCounts,
#'                                  itemName = "counts",
#'                                  itemType = "counts",
#'                                  funArgs = myFunArgs)
#'
#' @import lubridate assertthat
#'
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
