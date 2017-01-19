### Function print ###
#' Function print.DGEobj
#'
#' Print a DGEobj object
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq
#'
#' @param dgeObj A DGEobj object.
#' @param verbose  Add funArgs to the output
#' @param pretty prints using knitr::kable if TRUE
#'
#' @return NULL
#'
#' @examples
#'   print(myDGEobj)
#'   print(myDGEobj, verbose=TRUE)
#'
#' @import knitr
#'
#' @export
print.DGEobj <- function(dgeObj, verbose=FALSE, pretty=TRUE, ...)  {

    ItemNames <- itemNames(dgeObj)
    ItemTypes <- attr(dgeObj, "type")
    BaseTypes <- attr(dgeObj, "basetype")
    CreationDates <- lapply(attr(dgeObj, "dateCreated"), strftime)
    Parents <- lapply (dgeObj$data, function(x) attr(x, "parent"))
    FunArgs <- attr(dgeObj, "funArgs")

    df <- data.frame(cbind(ItemName=ItemNames, ItemType=ItemTypes,
                           BaseType=BaseTypes, DateCreated=CreationDates,
                           Parent=Parents),
                     row.names=NULL)
    if (verbose==TRUE)
        df$FunArgs <- unlist(FunArgs)

    cat(paste("\nDimension: [", dim(dgeObj)[1], ", ", dim(dgeObj)[2], "]", sep=""))
    cat("")
    if (pretty == TRUE)
        return(knitr::kable(df, row.names=FALSE))
    else return(df)
}
