### Function newType ###
#' Function newType
#'
#' Used to customiza a DGEobj definition by adding new types.  A basetype
#' is also declared and whether more than a single instance is allowed.
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class dgeObj created by function initDGEobj
#' @param itemType The name of the new type to create
#' @param baseType The basetype of the new item.  One of [row, col, assay, meta]
#' @param uniqueItem If set to TRUE, only one instance of the new type is
#'    allowed in a DGEobj
#'
#' @return A DGEobj definition object
#'
#' @examples
#'     MyDgeObj <- newType(MyDgeObj,
#'                         itemType="AffyRMA",
#'                         baseType="assay",
#'                         uniqueItem=TRUE)
#'
#' @import assertthat
#'
#' @export
newType <- function(dgeObj, itemType, baseType, uniqueItem=FALSE){
    #Set uniqueItem to TRUE to allow only one instance of itemType
    result <- FALSE
    assert_that(!missing(dgeObj), !missing(itemType),
                !missing(baseType), class(dgeObj) == "DGEobj",
                baseType %in% baseTypes(dgeObj)
    )

    #define new type
    attr(dgeObj, "objDef")$type[itemType] <- baseType
    if (uniqueItem == TRUE)
        attr(dgeObj, "objDef")$uniqueType <- c(attr(dgeObj, "objDef")$uniqueType, itemType)

    return(dgeObj)
}
