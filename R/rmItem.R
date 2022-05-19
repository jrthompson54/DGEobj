#' Removes a named data item
#'
#' @param dgeObj   A class DGEobj created by function initDGEobj()
#' @param itemName Name of the item to remove
#'
#' @return A DGEobj
#'
#' @examples
#'     # example DGEobj
#'     exObj <- readRDS(system.file("miniObj.RDS", package = "DGEobj"))
#'
#'     exObj <- rmItem(exObj, "design")
#'
#' @importFrom assertthat assert_that
#'
#' @export
rmItem <- function(dgeObj, itemName){

    assertthat::assert_that("DGEobj" %in% class(dgeObj),
                            msg = "The dgeObj parameter must be of class 'DGEobj'.")
    assertthat::assert_that(!missing(itemName),
                            length(itemName) == 1,
                            "character" %in% class(itemName),
                            msg = "Specify a singular itemName as a character string.")

    if (!itemName %in% names(dgeObj))
        stop(paste(itemName, " does not exist within dgeObj", sep = ""))

    dgeObj[itemName] <- NULL

    attr(dgeObj, "basetype")[itemName] <- NULL
    attr(dgeObj, "type")[itemName] <- NULL
    attr(dgeObj, "parent")[itemName] <- NULL
    attr(dgeObj, "funArgs")[itemName] <- NULL
    attr(dgeObj, "dateCreated")[itemName] <- NULL

    return(dgeObj)
}
