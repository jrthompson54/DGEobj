#' @export
## as.list
# as.list <- function(x, ...) UseMethod("as.list")
# as.list.default <- function(dgeResult, ...) {
#     warning(paste("as.list does not know how to handle object of class ",
#                   class(dgeResult),
#                   "and can only be used on class DGEresult"))
# }
as.list.DGEobj <- function(dgeObj){
    #Set uniqueItem to TRUE to allow only one instance of itemType
    unclass(dgeObj)
    return(dgeObj)
}
