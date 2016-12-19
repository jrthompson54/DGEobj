#' @export
### showTypes
showTypes <- function(DGEobjDef=.DGEobjDef, pretty=TRUE){
    df <- as.data.frame(unlist(DGEobjDef$type), stringsAsFactors=FALSE)
    df$type <- rownames(df)
    colnames(df) <- c("BaseType", "Type")
    df %<>% dplyr::select(Type, BaseType)
    colnames(df) <- c("Type", "BaseType")
    if (pretty)
        knitr::kable(df, row.names=FALSE)
}
