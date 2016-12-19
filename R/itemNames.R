#' @export
#Return the list of all item names
itemNames  <- function(x, ...) UseMethod("itemNames")
itemNames.default <- function(dgeObj, ...){
    warning(paste("dim does not know how to handle object of class ",
                  class(dgeObj),
                  "and can only be used on class DGEresult"))
}
itemNames.DGEobj <- function(dgeObj){
    names(dgeObj$data)
}
