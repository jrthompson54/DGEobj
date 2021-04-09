#' Retrieve multiple data items by name
#'
#' @param dgeObj    A class DGEobj created by function initDGEobj()
#' @param itemNames A character string, character vector, or list names to retrieve
#'
#' @return A list
#'
#' @examples
#'     # example DGEobj
#'     exObj <- readRDS(system.file("exampleObj.RDS", package = "DGEobj"))
#'
#'     myList <- getItems(exObj, list("counts", "geneData"))
#'     names(myList)
#'
#' @importFrom assertthat assert_that
#' @importFrom stringr str_c
#'
#' @export
getItems <- function(dgeObj, itemNames){

    assertthat::assert_that(!missing(dgeObj),
                            !missing(itemNames),
                            msg = "Specify both a DGEobj and at least one itemName to retrieve.")
    assertthat::assert_that("DGEobj" %in% class(dgeObj),
                            msg = "The DGEobj must be of class 'DGEobj'.")
    assertthat::assert_that(any(c("character", "list") %in% class(itemNames)),
                            msg = "Pass the itemNames as a single character string, a character vector, or a list of string names to retrieve.")

    idx <- itemNames %in% names(dgeObj)
    result <- list()
    for (itemName in itemNames[idx]) {
        result[[itemName]] <- dgeObj[[itemName]]
    }

    if (length(result) == 1) result <- result[[1]]

    if (sum(idx) < length(idx)) {
        missingItems <- stringr::str_c(itemNames[!idx], sep = ", ")
        warning(stringr::str_c("These item(s) not found: [", missingItems, "]"))
    }

    return(result)
}


#' Retrieve a data item by name
#'
#' @param dgeObj   A class DGEobj created by function initDGEobj()
#' @param itemName Name of item to retrieve
#'
#' @return The requested data item
#'
#' @examples
#'     # example DGEobj
#'     exObj <- readRDS(system.file("exampleObj.RDS", package = "DGEobj"))
#'
#'     MyCounts <- getItem(exObj, "counts")
#'
#' @importFrom assertthat assert_that
#'
#' @export
getItem <- function(dgeObj, itemName){
    assertthat::assert_that(!missing(dgeObj),
                            !missing(itemName),
                            msg = "Specify both a DGEobj and an itemName to retrieve.")
    assertthat::assert_that("DGEobj" %in% class(dgeObj),
                            msg = "The DGEobj must be of class 'DGEobj'.")
    assertthat::assert_that(class(itemName) == "character",
                            length(itemName) == 1,
                            msg = "The itemName should be a character string and contain the name of only one item to retrieve. To retrieve multiple items, use the getItems() function.")
    assertthat::assert_that(itemName %in% names(dgeObj),
                            msg = "The requested itemName should be in the DGEobj. Use names(dgeObj) to see the available items.")
    return(dgeObj[[itemName]])
}


#' Retrieve data items by type
#'
#' @param dgeObj  A class DGEobj created by function initDGEobj()
#' @param type    A type or list of types to retrieve
#' @param parent  (optional) Filter return list for common parent (e.g. useful
#' to select one set of contrast results when multiple fits have been performed)
#'
#' @return A list of data items
#'
#' @examples
#'     # example DGEobj
#'     exObj <- readRDS(system.file("exampleObj.RDS", package = "DGEobj"))
#'
#'     MyRawData      <- getType(exObj, type = list("counts", "design", "geneData"))
#'
#' @export
getType <- function(dgeObj, type, parent){

    idx <- attr(dgeObj, "type") %in% type
    if (!missing(parent)) {
        pidx <- attr(dgeObj, "parent") == parent
        idx <- idx & pidx
    }
    result <- unclass(dgeObj)[idx]

    if (sum(idx) == 0) {
        warning("No items of specified type were found")
        return(NULL)
    } else {
        if (sum(idx) < length(type))
            warning("Some types were not found")
        return(result)
    }
}


#' Retrieve data items by baseType

#' @param dgeObj   A class DGEobj created by function initDGEobj()
#' @param baseType One or more of: ["row", "col", "assay", "meta"]
#'
#' @return A list of data items
#'
#' @examples
#'     # example DGEobj
#'     exObj <- readRDS(system.file("exampleObj.RDS", package = "DGEobj"))
#'
#'     Assays <- getBaseType(exObj, baseType = "assay")
#'     AssaysAndMeta <- getBaseType(exObj, c("assay", "meta"))
#'
#' @export
getBaseType <- function(dgeObj, baseType){

    if (missing(baseType))
        stop("baseType argument is required")

    if (!all(baseType %in% baseTypes(dgeObj)))
        stop(paste("baseType must be one of: ",
                   paste(baseTypes(dgeObj), collapse = ", "),
                   sep = ""))

    idx <- attr(dgeObj, "basetype") %in% baseType

    if (sum(idx) < length(baseType))
        warning("Some baseTypes were not found")

    result <- unclass(dgeObj)[idx]
    return(result)
}
