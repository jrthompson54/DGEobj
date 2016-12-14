### addType
addType <- function(x, ...) UseMethod(".dimensionMatch")
addType.default <- function(dgeResult, ...) {
    warning(paste("addType does not know how to handle object of class ",
                  class(dgeResult),
                  "and can only be used on class DGEresult"))
}
addType.DGEresult <- function(type, basetype){
    .type[type] <- basetype
}
