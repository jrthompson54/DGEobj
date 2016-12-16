### showTypes
# showTypes <- function(x, ...) UseMethod("showTypes")
# showTypes.default <- function(x, ...) {
#     warning(paste("showTypes does not know how to handle object of class ",
#                   class(dgeResult),
#                   "and can only be used on class DGEresult"))
# }
showTypes <- function(pretty=TRUE){
    df <- as.data.frame(unlist(.type), stringsAsFactors=FALSE)
    df$type <- rownames(df)
    colnames(df) <- c("BaseType", "Type")
    df %<>% dplyr::select(Type, BaseType)
    colnames(df) <- c("Type", "BaseType")
    if (pretty)
        knitr::kable(df, row.names=FALSE)
}
