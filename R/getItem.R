### getItem
getItem <- function(x, ...) UseMethod("getItem")
getItem.default <- function(dgeResult, ...)
{
    warning(paste("getItem does not know how to handle object of class ",
                  class(dgeResult),
                  "and can only be used on class DGEresult"))
}
getItem.DGEresult <- function(dgeResult, itemName){
    return(dgeResult$data[[itemName]])
}
