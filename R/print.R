### Function print ###
#' Function print.DGEobj
#'
#' Print a DGEobj object
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq
#'
#' @param dgeObj A DGEobj object
#' @param verbose  Add funArgs to the output (default=FALSE)
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
print.DGEobj <- function(dgeObj, verbose=FALSE, ...)  {

    ItemNames <- names(dgeObj)
    # ItemTypes <- lapply (dgeObj, function(x) attr(x, "type"))
    # BaseTypes <- lapply (dgeObj, function(x) attr(x, "basetype"))
    # Parents <- lapply (dgeObj, function(x) attr(x, "parent"))
    ItemTypes <- getItemAttributes(dgeObj, "type")
    BaseTypes <- getItemAttributes(dgeObj, "basetype")
    Parents <- getItemAttributes(dgeObj, "parent")

   # CreationDates <- lapply(attr(dgeObj, "dateCreated"), strftime)
    CreationDates <- lapply (dgeObj, function(x) strftime(attr(x, "dateCreated")))
    #FunArgs <- attr(dgeObj, "funArgs")
    FunArgs <- getItemAttributes(dgeObj, "funArgs")

    df <- data.frame(cbind(ItemName=ItemNames,
    					   ItemType=ItemTypes,
                           BaseType=BaseTypes,
                           Parent=Parents,
                           DateCreated=CreationDates),
                           row.names=NULL)
    if (verbose==TRUE)
        df$FunArgs <- unlist(FunArgs)

    cat(paste("\nDimension: [", dim(dgeObj)[1], ", ", dim(dgeObj)[2], "]", sep=""))
    cat("")

    print(knitr::kable(df, row.names=FALSE))

    return(invisible(df))
}
