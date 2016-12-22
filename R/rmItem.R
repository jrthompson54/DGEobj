### Function as.list ###
#' Function as.list
#'
#' Removes a named data item from a DGEobj
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class dgeObj created by function initDGEobj
#' @param itemName
#'
#' @return An updated DGEobj
#'
#' @examples
#'    MyDgeObj <- rmItem(MyDgeObj, "design")
#'
#' @import assertthat
#'
#' @export
rmItem <- function(dgeObj, itemName){
    assert_that(class(dgeObj) == "DGEobj",
                !missing(itemName),
                class(itemName) == "character")

    #item not found
    if (!itemName %in% names(dgeObj$data))
        stop(paste(itemName, " does not exist within DGEresult.", sep=""))

    dgeObj$data[itemName] <- NULL
    attr(dgeObj, "type")[[itemName]] <- NULL
    attr(dgeObj, "basetype")[[itemName]] <- NULL
    attr(dgeObj, "dateCreated")[[itemName]] <- NULL
    attr(dgeObj, "funArgs")[[itemName]] <- NULL

    return(dgeObj)
}
