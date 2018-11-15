### Function getItems ###
#' Function getItems
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class dgeObj created by function initDGEobj
#' @param itemNames A list of itemNames to retrieve
#'
#' @return A data item or list data items
#'
#' @examples
#'    MyCounts <- getItem(dgeObj, "counts")
#'
#' @importFrom assertthat assert_that
#'
#' @export
getItems <- function(dgeObj, itemNames){

    assert_that(!missing(dgeObj),
                !missing(itemNames),
                class(dgeObj)[[1]] == "DGEobj",
                class(itemNames)[[1]] == "character"
    )

    idx <- names(dgeObj) %in% itemNames
    if (sum(idx) == 0){
        tsmsg("Warning: No matching item(s) found.")
        result = NULL
    } else {
        result <- unclass(dgeObj)[idx]  #list of elements
        if (length(result) < length (itemNames))
            warning("Some requested items were missing!")
    }
    return(result)
}

### Function getItem ###
#' Function getItem
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class dgeObj created by function initDGEobj
#' @param itemName Name of item to retrieve
#'
#' @return A data item
#'
#' @examples
#'    MyCounts <- getItem(dgeObj, "counts")
#'
#' @importFrom assertthat assert_that
#'
#' @export
getItem <- function(dgeObj, itemName){
    assert_that(!missing(dgeObj),
                !missing(itemName),
                class(dgeObj)[[1]] == "DGEobj",
                class(itemName) == "character",
                length(itemName) == 1
    )
    x <- getItems(dgeObj, itemName)
    if (length(x) > 0)
        return(x[[1]])
    else
        return(NULL)
}

### Function getType ###
#' Function getType
#'
#' Retrieve one of more data items by type.
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class dgeObj created by function initDGEobj
#' @param type A single type of list of types to retreives.  Enter
#'    showTypes(MyDgeObj) to see a list of allowed types.  See F. addType
#'    to define new types.
#' @param parent (Optional) filter return list for common parent (e.g. useful
#' to select one set of contrast results when multiple fits have been performed)
#' @return A list of data items
#'
#' @examples
#'    MyContrastList <- getType(dgeObj, type="topTable")
#'    MyRawData <- getType(dgeObj, type=list("counts", "design", "geneData"))
#'
#' @export
#Return all items of a specified type as a list
getType <- function(dgeObj, type, parent){
    #type can be a single named type or a vector or list of types
    idx <- attr(dgeObj, "type") %in% type
    if (!missing(parent)){
    	pidx <- attr(dgeObj, "parent") == parent
    	idx <- idx & pidx
    }
    result <- unclass(dgeObj)[idx]

##    filter for defined parent
#    if (!missing("parent")){
##        Get the names of items
#        itemNames <- names(lapply(result, attr, "parent"))
#        result <- result[itemNames]
#    }

    if (sum(idx) < length(type))
        warning("Some types were not found")
    if (sum(idx) == 0){
        tsmsg("Warning: no items of specified type are found.")
        return(NULL)
    } else {
        if (sum(idx) < length(type))
            warning("Some types were not found")
        return(result)
    }
}



### Function getBaseType ###
#' Function getBaseType
#'
#' Accessor function for DGEobj class objects.  Retrieves all data items of a
#' given basetype or list of basetypes.
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class dgeObj created by function initDGEobj
#' @param baseType One or more of: ["row", "col", "assay", "meta"]
#'
#' @return A simple list of data items
#'
#' @examples
#'    Assays <- getBaseType(dgeObj, baseType="assay")
#'    AssaysAndGeneAnnotation <- getBaseType(degObj, c("assay", "row"))
#'
#' @export
#Return all items of a specified type as a list
getBaseType <- function(dgeObj, baseType){

    if (missing(baseType))
        stop("baseType argument is required")

    if (!baseType %in% baseTypes(dgeObj))
        stop(paste("baseType must be one of: ",
                paste(baseTypes(dgeObj), collapse=", "),
                sep=""))

    idx <- attr(dgeObj, "basetype") %in% baseType

    if (sum(idx) < length(baseType))
        warning("Some baseTypes were not found")

    result <- unclass(dgeObj)[idx]
    return(result)
}

### Function BaseType ###
#' Function BaseType
#'
#' Return the basetype for a given type
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj A class DGEobj created by function initDGEobj
#' @param type  A type for which you want the basetype
#'
#' @return A basetype value (chracter string)
#'
#' @examples
#'    MyBaseType <- baseType(dgeObj, type="DGEList")
#'
#' @importFrom assertthat assert_that
#'
#' @export
baseType <- function(dgeObj, type){

    assert_that(!missing(dgeObj),
                !missing(type),
                class(dgeObj)[[1]] == "DGEobj",
                class(type)[[1]] == "character"
                )
    objDef <- attr(dgeObj, "objDef")
    return (objDef$type[[type]])

}

### Function baseTypes ###
#' Function baseTypes
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' Return a list of the available basetypes
#'
#' @param dgeObj  a class DGEobj object
#'
#' @return A list of  basetypes
#'
#' @examples
#'    #global definition of baseTypes
#'    mybasetypes <- baseTypes()
#'    #basetypes from a specific DGEobj
#'    mybasetypes <- baseTypes(myDgeObj)
#'
#' @export
baseTypes <- function(dgeObj, itemNames){
    if (missing(dgeObj))
        return(unique(.DGEobjDef$type))
    else
        return(unique(attr(dgeObj, "objDef")$type))
}



