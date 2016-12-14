### showTypes
showTypes <- function(x, ...) UseMethod(".dimensionMatch")
showTypes.default <- function(dgeResult, ...) {
    warning(paste("showTypes does not know how to handle object of class ",
                  class(dgeResult),
                  "and can only be used on class DGEresult"))
}
showTypes.DGEresult <- function(pretty=TRUE){
    df <- as.data.frame(unlist(.type))
    colnames(df) <- c("Type", "BaseType")
    If (pretty)
    knitr::kable(df)
}
