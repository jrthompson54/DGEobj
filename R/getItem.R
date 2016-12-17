### getItem
getItem <- function(x, ...) UseMethod("getItem")
getItem.default <- function(dgeResult, ...)
{
    warning(paste("getItem does not know how to handle object of class ",
                  class(dgeResult),
                  "and can only be used on class DGEresult"))
}
getItem.DGEresult <- function(dgeResult, itemName){
    #itemName can be a single element or a vector of item names.
    # Return a single element or a list if more than one element

    idx <- names(dgeResult$data) %in% itemName
    if (sum(idx) == 1)
        result <- dgeResult$data[idx][[1]] #single element
    else {
        result <- dgeResult$data[idx]  #list of elements
        if (length(result) < length (itemName))
            warning("Some requested items were missing!")
    }
    return(result)
}
