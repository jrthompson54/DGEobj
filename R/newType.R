### newType
# newType <- function(x, ...) UseMethod("newType")
# newType.default <- function(dgeResult, ...) {
#     warning(paste("newType does not know how to handle object of class ",
#                   class(dgeResult),
#                   "and can only be used on class DGEresult"))
# }
newType <- function(itemType, basetype, uniqueItem=FALSE){
    #Set uniqueItem to TRUE to allow only one instance of itemType
    result <- FALSE
    if (missing(itemType) | missing(basetype))
        stop("Both type and basetype arguments are required")
    #check basetype
    if (!basetype %in% .basetype)
        stop ("Basetype must be one of: row, col, assay, meta")
    #does type already exist?
    if (itemType %in% names(.type))
        stop(paste("Type [", itemType, "] already exists", sep=""))

    #define new type
    .type[itemType] <- basetype
    if (uniqueItem == TRUE)
        .uniqueType <- c(.uniqueType, itemType)

    result <- TRUE
    return(.type)
}
