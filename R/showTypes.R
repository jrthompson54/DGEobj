### Function showTypes ###
#' Function showTypes
#'
#' Show the list of all Types defined  in the DGEobj.
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class dgeObj created by function initDGEobj
#' @param pretty TRUE value (the default) invokes knitr::kable to print a
#'    nicely formatted table
#'
#' @return Prints a list of defined "types"
#'
#' @examples
#'    showTypes(MyDgeObj)
#'
#' @import magrittr assertthat dplyr knitr
#'
#' @export
showTypes <- function(dgeObj, pretty=TRUE){

    assert_that(class(dgeObj) == "DGEobj")

    df <- as.data.frame(unlist(attr(dgeObj, "objDef")$type), stringsAsFactors=FALSE)
    df$type <- rownames(df)
    colnames(df) <- c("BaseType", "Type")
    df %<>% dplyr::select(Type, BaseType)
    #colnames(df) <- c("Type", "BaseType")
    if (pretty)
        knitr::kable(df, row.names=FALSE)
}
