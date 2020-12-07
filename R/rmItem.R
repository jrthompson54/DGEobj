#' Removes a named data item
#'
#' @param dgeObj   A class DGEobj
#' @param itemName Name of the item to remove
#'
#' @return A DGEobj
#'
#' @examples
#'     # example DGEobj
#'     exObj <- readRDS(system.file("exampleObj.RDS", package = "DGEobj"))
#'
#'     exObj <- rmItem(exObj, "design")
#'
#' @importFrom assertthat assert_that
#'
#' @export
rmItem <- function(dgeObj, itemName){

    assertthat::assert_that(class(dgeObj)[[1]] == "DGEobj",
                            msg = "The dgeObj parameter must be of class 'DGEobj'.")
    assertthat::assert_that(!missing(itemName),
                            length(itemName) == 1,
                            class(itemName)[[1]] == "character",
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


#' Removes list of named data items

#' @param dgeObj A DGEobj
#' @param items  Either a character vector of names or numeric indexes of items to remove.  Use inventory(DGEobj) to view the indexes of items.
#'
#' @return A DGEobj
#'
#' @examples
#'     # example DGEobj
#'     exObj <- readRDS(system.file("exampleObj.RDS", package = "DGEobj"))
#'
#'     exObj <- rmItems(exObj, c("design", "design_orig"))
#'     exObj <- rmItems(exObj, c(1:2))
#'
#' @importFrom assertthat assert_that
#'
#' @export
rmItems <- function(dgeObj, items){
    assertthat::assert_that(!missing(dgeObj),
                            !missing(items),
                            msg = "Specify both a DGEobj and a character vector or list of items to remove.")
    assertthat::assert_that("DGEobj" %in% class(dgeObj),
                            msg = "The DGEobj must be of class 'DGEobj'.")

    if ("list" %in% class(items)) items <- unlist(items)

    if (any(c("numeric", "integer") %in% class(items)) & max(items) > length(dgeObj))
        stop("A value in the numeric index is larger than the number of items in dgeObj" )

    if (any(c("numeric", "integer") %in% class(items))) items <- names(dgeObj)[items]

    for (item in items) {
        dgeObj <- rmItem(dgeObj, item)
    }

    return(dgeObj)
}
