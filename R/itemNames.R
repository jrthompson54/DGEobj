#Return the list of all item names
itemNames  <- function(x, ...) UseMethod("itemNames")
itemNames.default <- function(dgeResult, ...){
    warning(paste("dim does not know how to handle object of class ",
                  class(dgeResult),
                  "and can only be used on class DGEresult"))
}
itemNames.DGEresult <- function(dgeResult){
    names(dgeResult$data)
}
