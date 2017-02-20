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

# print("Entering print.DGEobj")
# browser()

    ItemNames <- names(dgeObj)
    # ItemTypes <- lapply (dgeObj, function(x) attr(x, "type"))
    # BaseTypes <- lapply (dgeObj, function(x) attr(x, "basetype"))
    # Parents <- lapply (dgeObj, function(x) attr(x, "parent"))
    ItemTypes <- getItemAttribute(dgeObj, "type")
    BaseTypes <- getItemAttribute(dgeObj, "basetype")
    Parents <- getItemAttribute(dgeObj, "parent")

   # CreationDates <- lapply(attr(dgeObj, "dateCreated"), strftime)
    CreationDates <- lapply (dgeObj, function(x) strftime(attr(x, "dateCreated")))
    #FunArgs <- attr(dgeObj, "funArgs")
    FunArgs <- getItemAttribute(dgeObj, "funArgs")

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


#Same function with a distinct name instead of overloading the print function
#for debugging purposes.
#
### Function printDGEobj ###
#' Function printDGEobj
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
printDGEobj <- function(dgeObj, verbose=FALSE, ...)  {

    print("Entering printDGEobj")
    browser()

    ItemNames <- names(dgeObj)
    # ItemTypes <- lapply (dgeObj, function(x) attr(x, "type"))
    # BaseTypes <- lapply (dgeObj, function(x) attr(x, "basetype"))
    # Parents <- lapply (dgeObj, function(x) attr(x, "parent"))
    ItemTypes <- getItemAttribute(dgeObj, "type")
    BaseTypes <- getItemAttribute(dgeObj, "basetype")
    Parents <- getItemAttribute(dgeObj, "parent")

    # CreationDates <- lapply(attr(dgeObj, "dateCreated"), strftime)
    CreationDates <- lapply (dgeObj, function(x) strftime(attr(x, "dateCreated")))
    #FunArgs <- attr(dgeObj, "funArgs")
    FunArgs <- getItemAttribute(dgeObj, "funArgs")

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


