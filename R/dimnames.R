### Function dimnames.DGEobj ###
#' Function dimnames.DGEobj
#'
#' Returns the rownames and colnames in a list of length 2.
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class dgeObj created by function initDGEobj
#'
#' @return A list of length 2 containing rownames and colnames.
#'
#' @examples
#'    MyDims <- dimnames(dgeObj)
#'    names(MyDims)
#'    [1] "rownames" "colnames"
#'
#' @export
dimnames.DGEobj <- function(dgeObj){
    firstAssay <- getBaseType(dgeObj, "assay")[[1]]
    return(list(rownames=rownames(firstAssay), colnames=colnames(firstAssay)))
}

#dimnames.DGEobj
# dimnames.DGEobj <- function(dgeObj){
#     #return  list(rownames=rnames, colnames=cnames)
#
#     #get row names
#     idx <- dgeObj$basetype == "assay"
#     if (sum(idx) > 0)
#         rnames <-rownames(dgeObj$data[idx][[1]])
#     else { #check row objects
#         idx <- dgeObj$basetype == "row"
#         if (sum(idx) > 0)
#             rnames <- rownames(dgeObj$data[idx][[1]])
#         else #no col data yet
#             rnames <- as.character()
#     }
#
#     #Get col names
#     idx <- dgeObj$basetype == "assay"
#     if (sum(idx) > 0)
#         cnames <- colnames(dgeObj$data[idx][[1]])
#     else { #check assays
#         idx <- dgeObj$basetype == "col"
#         if (sum(idx) > 0)
#             cnames <- rownames(dgeObj$data[idx][[1]])
#         else #no col data yet
#             cnames <- as.character()
#     }
#
#     return(list(rownames=rnames, colnames=cnames))
# } #dimnames.DGEobj

.updateRowNames <- function(dgeObj, rnames){
    #update rownames where appropriate

    #get names of row and assay asetypes
    idx <- names(dgeObj$data[dgeObj$basetype %in% c("row", "assay")])
    if (length(idx) > 0)
        for (n in idx) {
            rownames(dgeObj$data[[n]]) <- rnames
        }
    return(dgeObj)
}
.updateColNames <- function(dgeObj, cnames){
    #update rownames where appropriate

    #get names of col basetypes
    idx <- names(dgeObj$data[dgeObj$basetype == "col"])
    if (length(idx) > 0)
        for (n in idx) {
            rownames(dgeObj$data[[n]]) <- cnames
        }
    #get names of assay basetypes
    idx <- names(dgeObj$data[dgeObj$basetype == "assay"])
    if (length(idx) > 0)
        for (n in idx) {
            colnames(dgeObj$data[[n]]) <- cnames
        }
    return(dgeObj)
}


### Assignment functions
#' @export
`dimnames<-.DGEobj` <- function(dgeObj, dimNames){
    assert_that(class(dimNames) == "list" & length(dimnames) == 2)
    rnames <- dimNames[[1]]
    cnames <- dimNames[[2]]
    if (!is.null(rnames) & length(rnames) > 0) #not empty check dim and update
        if(length(rnames) == nrow(dgeObj)) {
            dgeObj <- updateRowNames(dgeObj, rnames)
        }
        else stop("length of rownames doesn't match dgeObj")

    if (!is.null(cnames) & length(cnames) > 0) #not empty check dim and update
        if(length(cnames) == ncol(dgeObj)) {
            dgeObj <- updateColNames(dgeObj, rnames)
        }
    else stop("length of rownames doesn't match dgeObj")
    return(dgeObj)
}



