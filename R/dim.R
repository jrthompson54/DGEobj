### Function dim ###
#' Function dim
#'
#' Reports the dimensions of the assay slot (row = genes; col = samples).
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class dgeObj created by function initDGEobj
#'
#' @return An integer vector [r,c] with a length of 2.
#'
#' @examples
#'    dim(MyDgeObj)
#'
#' @export
dim.DGEobj <- function(dgeObj)
    attr(d, "assayDim")


# dim.DGEobj <- function(dgeObj){
#
#     dimension <- c(0,0)
#     rowcount <- 0
#     colcount <- 0
#     rowTypes <- c("row", "assay")
#     colTypes <- c("col", "assay")
#
#     #look for first row or assay or contrastTop object and get nrow
#     idx <- dgeObj[["basetype"]] %in% rowTypes
#     if (sum(idx) > 0) {
#         rowcount <- nrow(dgeObj$data[idx][[1]])
#     }
#
#     #look for first col or assay or contrastTop object and get nrow
#     idx <- dgeObj[["basetype"]] %in% colTypes
#     if (sum(idx) > 0) {
#         if (dgeObj$type[idx][[1]] == "col")
#             colcount <- nrow(dgeObj$data[idx][[1]])
#         else colcount <- ncol(dgeObj$data[idx][[1]])
#     }
#
#     dimension <- c(rowcount, colcount)
#     return(dimension)
# } #dim
