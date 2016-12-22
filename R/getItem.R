### Function getItem ###
#' Function getItem (DGEobj)
#'
#' Casts a DGEobj class object as a simple list.
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class dgeObj created by function initDGEobj
#' @param itemName An itemName or list of itemNames to retrieve
#'
#' @return A data item or list data items
#'
#' @examples
#'    MyCounts <- getItem(dgeObj, "counts")
#'
#' @export
getItem <- function(dgeObj, itemName){
    #itemName can be a single element or a vector of item names.
    # Return a single element or a list if more than one element

    idx <- names(dgeObj$data) %in% itemName
    if (sum(idx) == 1)
        result <- dgeObj$data[idx][[1]] #single element
    else {
        result <- dgeObj$data[idx]  #list of elements
        if (length(result) < length (itemName))
            warning("Some requested items were missing!")
    }
    return(result)
}
