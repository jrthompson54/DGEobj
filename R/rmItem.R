### rmItem
rmItem <- function(x, ...) UseMethod("rmItem")
rmItem.default <- function(dgeResult, ...)
{
    warning(paste("rmItem does not know how to handle object of class ",
                  class(dgeResult),
                  "and can only be used on class DGEresult"))
}
rmItem.DGEresult <- function(dgeResult, itemName){
    #remove the named item
    dgeResult$data[itemName] <- NULL
    dgeResult$type[itemName] <- NULL
    dgeResult$basetype[itemName] <- NULL
    if(with(dgeResult$createdDate, exists(itemName)))
        dgeResult$createdDate[itemName] <- NULL
    if(with(dgeResult$funArgs, exists(itemName)))
        dgeResult$funARgs[itemName] <- NULL
    return(dgeResult)
}
