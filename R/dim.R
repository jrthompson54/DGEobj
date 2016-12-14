### dim
# dim <- function(x, ...) UseMethod("dim")
# dim.default <- function(dgeResult, ...) {
#     warning(paste("dim does not know how to handle object of class ",
#                   class(dgeResult),
#                   "and can only be used on class DGEresult"))
# }
dim.DGEresult <- function(dgeResult){

    dimension <- c(0,0)
    rowcount <- 0
    colcount <- 0
    rowTypes <- names(.type)[.type %in% c("row", "assay")]
    colTypes <- names(.type)[.type %in% c("col", "assay")]

    #look for first row or assay or contrastTop object and get nrow
    idx <- dgeResult[["type"]] %in% rowTypes
    if (sum(idx) > 0) {
        firstItemName <- names(dgeResult$type[idx])[[1]]
        rowcount <- nrow(dgeResult$data[[firstItemName]])
    }

    #look for first col or assay element
    idx <- dgeResult[["type"]] %in% colTypes
    if (sum(idx) > 0) {
        firstItemName <- names(dgeResult$type[idx])[[1]]
        firstItemType <- dgeResult$type[[firstItemName]]
        firstItem <- dgeResult$data[[firstItemName]]
        if (firstItemType == "col")
            colcount <- nrow(firstItem)
        else colcount <- ncol(firstItem)
    }
    dimension <- c(rowcount, colcount)
    return(dimension)
} #dim
