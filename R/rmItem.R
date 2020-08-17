### Function rmItem ###
#' Function rmItem
#'
#' Removes a named data item from a DGEobj
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class dgeObj created by function initDGEobj
#' @param itemName Name of the item to remove from the DGEobj
#'
#' @return An updated DGEobj
#'
#' @examples
#'    MyDgeObj <- rmItem(MyDgeObj, "design")
#'
#' @importFrom assertthat assert_that
#'
#' @export
rmItem <- function(dgeObj, itemName){

    assertthat::assert_that(class(dgeObj)[[1]] == "DGEobj",
                !missing(itemName),
                class(itemName)[[1]] == "character")

    #item not found
    if (!itemName %in% names(dgeObj))
        stop(paste(itemName, " does not exist within DGEresult.", sep=""))

    dgeObj[itemName] <- NULL

    #fix the main attributes
    attr(dgeObj, "basetype")[itemName] <- NULL
    attr(dgeObj, "type")[itemName] <- NULL
    attr(dgeObj, "parent")[itemName] <- NULL
    attr(dgeObj, "funArgs")[itemName] <- NULL
    attr(dgeObj, "dateCreated")[itemName] <- NULL


    return(dgeObj)
}

### Function rmItems ###
#' Function rmItems
#'
#' Removes a vector or list of named data items from a DGEobj
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class dgeObj created by function initDGEobj
#' @param items Either a character vector of item names or a numeric index of items to
#'   remove from the dgeObj.  Use inventory(dgeobj) to view the indexes of items.
#'
#' @return An updated DGEobj
#'
#' @examples
#'    MyDgeObj <- rmItems(MyDgeObj, c("designMatrix", "designMatrix_Elist"))
#'
#' @importFrom assertthat assert_that
#'
#' @export
rmItems <- function(dgeObj, items){
    # browser()
    assertthat::assert_that(!missing(dgeObj),
                            !missing(items),
                            "DGEobj" %in% class(dgeObj))

    #let's tolerate lists too.
    if ("list" %in% class(items)) items <- unlist(items)

    if (any(c("numeric", "integer") %in% class(items)) & max(items) > length(dgeObj))
        stop("A value in items numeric index is gt items in dgeObj" )

    #convert numeric Index to Names
    if (any(c("numeric", "integer") %in% class(items))) items <- names(dgeObj)[items]

    #delete the items
    for (item in items){
        dgeObj <- rmItem(dgeObj, item)
    }
    return(dgeObj)
}
