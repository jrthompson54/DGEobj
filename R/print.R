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
#' @import magrittr
#'
#' @export
print.DGEobj <- function(dgeObj, verbose=FALSE, pretty=TRUE, ...)  {

    ItemNames <- itemNames(dgeObj)
    ItemTypes <- attr(dgeObj, "type")  #dgeObj$type
    baseTypes <- attr(dgeObj, "basetype") #dgeObj$basetype
    creationDates <- lapply(attr(dgeObj, "dateCreated"), strftime)
    funArgs <- attr(dgeObj, "funArgs")  #dgeObj$funArgs

    df <- data.frame(cbind(ItemName=ItemNames, ItemType=ItemTypes,
                           BaseType=baseTypes, DateCreated=creationDates),
                     row.names=NULL)
    if (verbose==TRUE)
        df$funArgs <- unlist(funArgs)

    cat(paste("\nDimension: [", dim(dgeObj)[1], ", ", dim(dgeObj)[2], "]", sep=""))
    cat("")
    if (pretty == TRUE)
        return(knitr::kable(df, row.names=FALSE))
    else return(df)
}
