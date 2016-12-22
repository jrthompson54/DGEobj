### Function as.list.DGEobj ###
#' Function as.list.DGEobj
#'
#' Casts a DGEobj class object as a simple list.
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class dgeObj created by function initDGEobj
#'
#' @return A simple list version of the DGEobj
#'
#' @examples
#'    MyDGElist <- as.list(dgeObj)
#'
#' @export
as.list.DGEobj <- function(dgeObj){
    #Set uniqueItem to TRUE to allow only one instance of itemType
    unclass(dgeObj)
    return(dgeObj)
}
