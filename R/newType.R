#' @export
### newType
# newType <- function(x, ...) UseMethod("newType")
# newType.default <- function(dgeResult, ...) {
#     warning(paste("newType does not know how to handle object of class ",
#                   class(dgeResult),
#                   "and can only be used on class DGEresult"))
# }
newType <- function(itemType, baseType, uniqueItem=FALSE,
                    DGEobjDef = .DGEobjDef){
    #Set uniqueItem to TRUE to allow only one instance of itemType
    result <- FALSE
    if (missing(itemType) | missing(baseType))
        stop("Both itemType and baseType arguments are required")
    #check basetype
    if (!baseType %in% DGEobjDef$basetype)
        stop ("Basetype must be one of: row, col, assay, meta")
    #does type already exist?
    if (itemType %in% names(DGEobjDef$type))
        stop(paste("Type [", itemType, "] already exists", sep=""))

    #define new type
    DGEobjDef$type[itemType] <- baseType
    if (uniqueItem == TRUE)
        DGEobjDef$uniqueType <- c(DGEobjDef$uniqueType, itemType)

    return(DGEobjDef)
}
