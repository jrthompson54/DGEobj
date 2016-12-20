#' @export
### newType
# newType <- function(x, ...) UseMethod("newType")
# newType.default <- function(dgeResult, ...) {
#     warning(paste("newType does not know how to handle object of class ",
#                   class(dgeResult),
#                   "and can only be used on class DGEresult"))
# }
newType <- function(dgeObj, itemType, baseType, uniqueItem=FALSE){
    #Set uniqueItem to TRUE to allow only one instance of itemType
    result <- FALSE
    assert_that(!missing(dgeObj), !missing(itemType),
                !missing(baseType), class(dgeObj) == "DGEobj",
                baseType %in% dgeObj$objDef$basetype
    )

    #define new type
    dgeObj$objDef$type[itemType] <- baseType
    if (uniqueItem == TRUE)
        dgeObj$objDef$uniqueType <- c(dgeObj$objDef$uniqueType, itemType)

    return(dgeObj)
}
