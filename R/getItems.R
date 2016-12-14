### getItems
getItems <- function(x, ...) UseMethod("getItem")
getItems.default <- function(dgeResult, ...)
{
    warning(paste("getItem does not know how to handle object of class ",
                  class(dgeResult),
                  "and can only be used on class DGEresult"))
}
getItems.DGEresult <- function(dgeResult, itemList){
    # make sure the items exist
    inames <- names(dgeResult$data)
    itemCount <- length(itemList)
    itemList <- intersection(itemList, inames)
    if (length(itemList < itemCount))
        warning("Only some items were found.")

    ListOfItems <- list()
    for (i in itemList){
        idx <- dgeResult$type[[itemList[[i]]]]
        ListOfItems[[itemList[[i]]]] <- getItem(dgeResult, i)
    }
    if (length(ListOfItems) == 0)
        warning("No items on list were found in the DGEresult")
    if (length(ListOfItems) < length(itemList))
        warning("Not all specified items were found in DGEresult")

    return(ListOfItems)
}
