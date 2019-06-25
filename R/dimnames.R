### Function dimnames.DGEobj ###
#' Function dimnames.DGEobj
#'
#' Returns the rownames and colnames in a list of length 2.
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class dgeObj created by function initDGEobj
#'
#' @return A list of length 2 containing rownames and colnames.
#'
#' @examples
#'    MyDims <- dimnames(dgeObj)
#'    names(MyDims)
#'    "rownames" "colnames"
#'
#' @export
dimnames.DGEobj <- function(dgeObj){
    firstAssay <- getBaseType(dgeObj, "assay")[[1]]
    return(list(rownames=rownames(firstAssay), colnames=colnames(firstAssay)))
}


