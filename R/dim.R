### Function dim ###
#' Function dim
#'
#' Reports the dimensions of the assay slot (row = genes; col = samples).
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class dgeObj created by function initDGEobj
#'
#' @return An integer vector [r,c] with a length of 2.
#'
#' @examples
#'    dim(MyDgeObj)
#'
#' @export
dim.DGEobj <- function(dgeObj){
    #check the first assay for dimensions

    dimension <- c(0,0)

    idx <- attr(dgeObj, "basetype") == "assay"
    myassays <- unclass(dgeObj)[idx]

    if (length(myassays) > 0)
        dimension <- dim(myassays[[1]])

    return(dimension)
}
