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
#' @param custAttr A named list of attributes to add to the item.
#'
#' @return A DGEobj class object with a new DGEList added.
#'
#' @examples
#'    myFunArgs <- match.call() #capture calling function and arguments
#'    showTypes()  #See what predefined types are available
#'    myDgeObj <- addItem(myDgeObj, item = MyCounts,
#'                                  itemName = "counts",
#'                                  itemType = "counts",
#'                                  funArgs = myFunArgs)
#'
#' @import assertthat
#'
#' @export
addItem <- function(dgeObj, item, itemName, itemType,
                              overwrite=FALSE,
                              funArgs=match.call(),
                              custAttr
                              ){

    assert_that(!missing(dgeObj),
                !missing(item),
                !missing(itemName),
                !missing(itemType))

    basetype <- attr(dgeObj, "objDef")$type[[itemType]]
    switch(basetype,
           row = {if (!itemType == "granges" & is.null(rownames(item)))
                    stop("Row basetypes must have rownames")},
           col = {if (is.null(rownames(item)))
                    stop("Col basetypes must have rownames")},
           assay = {if (is.null(rownames(item)) | is.null(colnames(item)))
                        stop("Assay basetypes must have row and column names")}
    )

    #enforce itemType
    allowedTypes <- names(attr(dgeObj, "objDef")$type)
    if (!itemType %in% allowedTypes)
        stop(paste("itemType must be one of: ",
                   paste(allowedTypes, collapse=", "), sep=""))

    #refuse to add if itemName exists already unless overwrite = T
    if (overwrite==FALSE & itemName %in% names(dgeObj))
        stop('itemName already exists in DGEobj!')

    #check for disallowed second instance of uniqueTypes (unless overwrite mode)
    uniqueTypes <- attr(dgeObj, "objDef")$uniqueType
    if(itemType %in% uniqueTypes  &
       itemType %in% getItemAttribute(dgeObj, "type") &
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

    #set type, basetype, dateCreated and funArgs attributes of item
    stdAttr <- list(
        type = itemType,
        basetype = basetype,
        dateCreated = lubridate::now(),
        funArgs = funArgs
    )
    item <- setAttributes(item, stdAttr)

    # if (exists("custAttr")){
    if (!missing("custAttr")){
        # print("Adding custom attributes")
        item <- setAttributes(item, custAttr)
    }

    #ready to add the item with all its attributes
    dgeObj[[itemName]] <- item

    return(dgeObj)
} #addItem


### Function addItems ###
#' Function addItem (DGEobj)
#'
#' Add a data item to a class DGEobj
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj A DGEobj that items will be added to.
#' @param itemList  A list of data items to add to dgeObj
#' @param overwrite Default = FALSE.  Set to TRUE to overwrite the data object
#'     stored in the itemName slot
#' @param custAttr A named list of attributes to add to each item (optional)
#'
#' @return A DGEobj class object with new items added.
#'
#' @examples
#'    #replace a set of contrasts after adding something to each
#'    myDgeObj <- addItems(myDgeObj, mycontrastList, overwrite=TRUE)
#'
#' @import assertthat
#'
#' @export
addItems <- function(dgeObj, itemList, overwrite=FALSE, custAttr){

    assert_that(!missing(dgeObj),
                !missing(itemList),
                class(dgeObj)[[1]] == "DGEobj",
                class(itemList)[[1]] == "list")

    if (!missing(custAttr)){
        attrNames <- names(custAttr)
        for (i in 1:length(itemList))
            for (j in 1:length(custAttr))
                attr(itemList[[i]], attrNames[[j]]) <- custAttr[[j]]
    }

    itemNames <- names(itemList)
    itemType <- lapply(itemList, attr("Type"))
    for (i in 1:length(itemList))
        addItem(itemList[[i]], itemNames[[i]], itemType[[i]], overwrite=overwrite)
}
