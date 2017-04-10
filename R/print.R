### Function inventory ###
#' Function inventory
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
#'   #capture in a dataframe
#'   Mydf <- inventory(myDGEobj)
#'   #print a nicely formatted table
#'   knitr::kable(inventory(myDGEobj))
#'   #capture dataframe with verbose output
#'   Mydf <- inventory(myDGEobj, verbose=TRUE)
#'
#' @export
inventory <- function(dgeObj, verbose=FALSE, ...)  {

    ItemNames <- names(dgeObj)
    ItemTypes <- attr(dgeObj, "type")
    BaseTypes <- attr(dgeObj, "basetype")
    Parents <- attr(dgeObj, "parent")
    creationDates <- attr(dgeObj, "dateCreated")
    FunArgs <- attr(dgeObj, "funArgs")
    Class <- lapply(dgeObj, class)
    Class <- lapply(Class, `[[`, 1)

    df <- data.frame(cbind(ItemName=ItemNames,
    					   ItemType=ItemTypes,
                           BaseType=BaseTypes,
                           Parent=Parents,
    					   Class,
                           DateCreated=creationDates),
                           row.names=NULL)
    if (verbose==TRUE)
        df$FunArgs <- unlist(FunArgs)

    return(df)
}

### Function print.DGEobj ###
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
print.DGEobj <- function (dgeObj, digits = NULL, quote = TRUE, na.print = NULL,
                          print.gap = NULL, right = FALSE, max = NULL,
                          useSource = TRUE,
                          verbose=FALSE,
                          ...){
    df <- inventory(dgeObj, verbose=verbose)
    print(df)
    return(invisible(dgeObj))
}
