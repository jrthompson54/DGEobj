### Function itemNames ###
#' Function itemNames (DGEobj)
#'
#' Returns the names of all data items in a DGEobj
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class dgeObj created by function initDGEobj
#'
#' @return A character vector of names
#'
#' @examples
#'    MyNames <- itemNames(dgeObj)
#'
#' @export
itemNames <- function(dgeObj){
    names(dgeObj)
}

