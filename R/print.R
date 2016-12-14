### Function DGEresult ###
#' Function DGEresult
#'
#' Initializes an empty DGEresult object.  Optionally, add
#' the first data element.
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq
#'
#' @param data A parcel of data to be added to the DGEresult object.
#' @param dataName  A unique name for the data parcel
#' @param dataType A data type identifier. See .
#'
#' @return A DGEresult object
#'
#' @examples
#'    # init an empty DGEresult object
#'    myDGEresult <- DGEresult()
#'    # init and add the first data object.
#'    myDGEresult <- DGEresult(data = mycounts,
#'                    dataName = "counts",
#'                    dataType = "assay")
#'
#' @import magrittr
#'
#' @export
print.DGEresult <- function(dgeResult, verbose=FALSE, ...)  {

    itemNames <- itemNames(dgeResult)
    itemTypes <- dgeResult$type
    baseTypes <- dgeResult$basetype
    creationDates <- lapply(d$dateCreated, strftime)
    funArgs <- dgeResult$funArgs %>% as.list
    df <- data.frame(cbind(ItemName=itemNames, ItemType=itemTypes,
                           BaseType=baseTypes, DateCreated=creationDates),
                           row.names=NULL)
    if (verbose==TRUE)
        df %<>%  cbind(FunArgs=funArgs)

    print(knitr::kable(df, row.names=FALSE))
    cat("")
    Dim <- dim(dgeResult)
    cat(paste("\nDimension: [", Dim[1], ", ", Dim[2], "]", sep=""))
    # return(kable(df, row.names=FALSE))
    return(df)
}
