### Function getItems ###
#' Function getItems
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class dgeObj created by function initDGEobj
#' @param itemNames A list of itemNames to retrieve
#'
#' @return A data item or list data items
#'
#' @examples
#'    MyCounts <- getItem(dgeObj, "counts")
#'
#' @import assertthat
#'
#' @export
getItems <- function(dgeObj, itemNames){

    assert_that(!missing(dgeObj),
                !missing(itemNames),
                class(dgeObj)[[1]] == "DGEobj",
                class(itemNames)[[1]] == "character"
    )

    idx <- names(dgeObj$data) %in% itemNames
    result <- dgeObj$data[idx]  #list of elements
    if (length(result) < length (itemNames))
        warning("Some requested items were missing!")

    return(result)
}

### Function getItem ###
#' Function getItem
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class dgeObj created by function initDGEobj
#' @param itemName Name of item to retrieve
#'
#' @return A data item
#'
#' @examples
#'    MyCounts <- getItem(dgeObj, "counts")
#'
#' @import assertthat
#'
#' @export
getItem <- function(dgeObj, itemName)
    getItems(dgeObj, itemName)[[1]]

