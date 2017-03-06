### Function summarize ###
#' Function summarize
#'
#' Print a DGEobj object
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq
#'
#' @param dgeObj A DGEobj object
#' @param verbose  Add funArgs to the output (default=FALSE)
#'
#' @return a dataframe summarizing the data contained in the DGEobj
#'
#' @examples
#'   Mydf <- summarize(myDGEobj)
#'   knitr::kable(summarize(myDGEobj))
#'   Mydf <- summarize(myDGEobj, verbose=TRUE)
#'
#' @export
summarize <- function(dgeObj, verbose=FALSE, ...)  {

    ItemNames <- names(dgeObj)
    ItemTypes <- attr(dgeObj, "type")
    BaseTypes <- attr(dgeObj, "basetype")
    Parents <- attr(dgeObj, "parent")
    creationDates <- attr(dgeObj, "dateCreated")
    FunArgs <- attr(dgeObj, "funArgs")

    df <- data.frame(cbind(ItemName=ItemNames,
    					   ItemType=ItemTypes,
                           BaseType=BaseTypes,
                           Parent=Parents,
                           DateCreated=creationDates),
                           row.names=NULL)
    if (verbose==TRUE)
        df$FunArgs <- unlist(FunArgs)

    return(df)
}

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
print.dgeObj <- function (dgeObj, digits = NULL, quote = TRUE, na.print = NULL,
                          print.gap = NULL, right = FALSE, max = NULL,
                          useSource = TRUE,
                          verbose=FALSE,
                          ...){
    df <- summarize(dgeObj, verbose=verbose)
    print(df)
    return(invisible(dgeObj))
}
