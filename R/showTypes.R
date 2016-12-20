#' @export
### showTypes
showTypes <- function(dgeObj, pretty=TRUE){

    assert_that(class(dgeObj) == "DGEobj")

    df <- as.data.frame(unlist(dgeObj$objDef$type), stringsAsFactors=FALSE)
    df$type <- rownames(df)
    colnames(df) <- c("BaseType", "Type")
    df %<>% dplyr::select(Type, BaseType)
    colnames(df) <- c("Type", "BaseType")
    if (pretty)
        knitr::kable(df, row.names=FALSE)
}
