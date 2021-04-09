#' Get the baseType of an internal data item
#'
#' @param dgeObj A class DGEobj created by function initDGEobj()
#' @param type   An item type for which you want the baseType
#'
#' @return character string
#'
#' @examples
#'     # example DGEobj
#'     exObj <- readRDS(system.file("exampleObj.RDS", package = "DGEobj"))
#'
#'     baseType(exObj, type = "DGEList")
#'
#' @importFrom assertthat assert_that
#'
#' @export
baseType <- function(dgeObj, type){
    assertthat::assert_that(!missing(dgeObj),
                            !missing(type),
                            msg = "Specify both a DGEobj and a type (to check the baseType). Both are required.")
    assertthat::assert_that(class(dgeObj)[[1]] == "DGEobj",
                            msg = "The DGEobj must be of class 'DGEobj'.")
    assertthat::assert_that(class(type)[[1]] == "character",
                            msg = "The type must be of class 'character'.")

    objDef <- attr(dgeObj, "objDef")

    assertthat::assert_that(type %in% names(objDef$type),
                            msg = "The type is not defined on the DGEobj")

    return(objDef$type[[type]])
}


#' Get a list of the available baseTypes
#'
#' @param dgeObj  (optional) A class DGEobj created by function initDGEobj()
#'
#' @return A character vector of baseTypes
#'
#' @examples
#'     # Global definition of baseTypes
#'     baseTypes()
#'
#'     # example DGEobj
#'     exObj <- readRDS(system.file("exampleObj.RDS", package = "DGEobj"))
#'
#'     # Basetypes from a specific DGEobj
#'     baseTypes(exObj)
#'
#' @export
baseTypes <- function(dgeObj){
    if (missing(dgeObj))
        return(unique(.DGEobjDef$type))
    else
        return(unique(attr(dgeObj, "objDef")$type))
}


#' Returns and prints the list of all defined types
#'
#' @param dgeObj  A class DGEobj created by function initDGEobj()
#'
#' @return data.frame
#'
#' @examples
#'     # example DGEobj
#'     exObj <- readRDS(system.file("exampleObj.RDS", package = "DGEobj"))
#'
#'     showTypes(exObj)
#'
#' @import magrittr
#' @importFrom assertthat assert_that
#'
#' @export
showTypes <- function(dgeObj){

    assertthat::assert_that(class(dgeObj) == "DGEobj",
                            msg = "The DGEobj must be of class 'DGEobj'.")

    df <- as.data.frame(unlist(attr(dgeObj, "objDef")$type), stringsAsFactors = FALSE)
    df$type <- rownames(df)
    colnames(df) <- c("BaseType", "Type")
    df <- df[, c("Type", "BaseType")]

    return(df)
}


#' Add a new type definition to a DGEobj
#'
#' @param dgeObj     A class DGEobj created by function initDGEobj()
#' @param itemType   The name of the new type to create
#' @param baseType   The baseType of the new item. One of [row, col, assay, meta]
#' @param uniqueItem If set to TRUE, only one instance of the new type is allowed in a DGEobj
#'
#' @return A DGEobj
#'
#' @examples
#'     # example DGEobj
#'     exObj <- readRDS(system.file("exampleObj.RDS", package = "DGEobj"))
#'
#'     exObj <- newType(exObj,
#'                      itemType   = "AffyRMA",
#'                      baseType   = "assay",
#'                      uniqueItem = TRUE)
#'
#' @importFrom assertthat assert_that
#'
#' @export
newType <- function(dgeObj, itemType, baseType, uniqueItem = FALSE){
    result <- FALSE

    assertthat::assert_that(!missing(dgeObj),
                            !missing(itemType),
                            !missing(baseType),
                            msg = "Specify the DGEobj, itemType, and baseType. All three are required.")
    assertthat::assert_that(class(dgeObj) == "DGEobj",
                            msg = "The DGEobj must be of class 'DGEobj'.")
    assertthat::assert_that(baseType %in% baseTypes(dgeObj),
                            msg = "The baseType must be one of the baseTypes available in the DGEobj. Use baseTypes(DGEobj) to see which are available.")

    attr(dgeObj, "objDef")$type[itemType] <- baseType
    if (uniqueItem == TRUE)
        attr(dgeObj, "objDef")$uniqueType <- c(attr(dgeObj, "objDef")$uniqueType, itemType)

    return(dgeObj)
}
