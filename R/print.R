### Function print ###
#' Function print.DGEresult
#'
#' Print a DGEresult object
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq
#'
#' @param dgeResult A DGEresult object.
#' @param verbose  Add funArgs to the output
#' @param pretty prints using knitr::kable if TRUE
#'
#' @return NULL
#'
#' @examples
#'   print(myDGEresult)
#'   print(myDGEresult, verbose=TRUE)
#'
#' @import magrittr
#'
#' @export
print.DGEresult <- function(dgeResult, verbose=FALSE, pretty=TRUE, ...)  {

    ItemNames <- itemNames(dgeResult)
    ItemTypes <- dgeResult$type
    baseTypes <- dgeResult$basetype
    creationDates <- lapply(d$dateCreated, strftime)
    funArgs <- dgeResult$funArgs

    df <- data.frame(cbind(ItemName=ItemNames, ItemType=ItemTypes,
                           BaseType=baseTypes, DateCreated=creationDates),
                     row.names=NULL)
    if (verbose==TRUE)
        df$funArgs <- unlist(funArgs)

    #print(knitr::kable(df, row.names=FALSE))

    Dim <- dim(dgeResult)
    cat(paste("\nDimension: [", Dim[1], ", ", Dim[2], "]", sep=""))
    cat("")
    if (pretty == TRUE)
        return(knitr::kable(df, row.names=FALSE))
    else return(df)
}
