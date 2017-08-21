### Function addItem ###
#' Function subset (DGEobj)
#'
#' Add a data item to a class DGEobj
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class dgeObj created by function initDGEobj
#' @param row  row index for the subset
#' @param col col index for the subset
#' @param drop Included for compatibility but has no real meaning in the context
#'    of subsetting a DGEobj.  So drop=FALSE is the default and changing this
#'    has no effect.
#'
#' @return A subsetted DGEobj class object
#'
#' @examples
#'    DgeObj <- subset(DgeObj, 1:10, 100:1000)
#'
#' @export
subset.DGEobj <- function(DgeObj, row, col, drop=FALSE){
    #check if both row and col provided.
    assertthat::assert_that(class(DgeObj)[[1]] == "DGEobj")

    #fill in missing row/col args
    if (missing(row))
        row <-1:nrow(DgeObj)
    if (missing(col))
        col <- 1:ncol(DgeObj)

    #make sure row and col in range
    if (max(row) > max(nrow(DgeObj)))
        stop ("row coordinates out of range")
    if (max(col) > max(ncol(DgeObj)))
        stop("col coordinates out of range")

    basetypes <- attr(DgeObj, "basetype")

    for (i in 1:length(DgeObj)){
        browser()
        switch(basetypes[[i]],

               row = {
                    if (is.null(dim(DgeObj[[i]]))) #not a matrix type object
                        DgeObj[[i]] <- DgeObj[[i]][row]
                    else DgeObj[[i]] <- DgeObj[[i]][row,]
                     },

               col = {
                   if (is.null(dim(DgeObj[[i]])))
                       DgeObj[[i]] <- DgeObj[[i]][col]
                    else DgeObj[[i]] <- DgeObj[[i]][col,]
                    },

               assay = {
                   DgeObj[[i]] <- DgeObj[[i]][row, col]
               })

    }

    return(DgeObj)
}

#' @export
`[.DGEobj` <- function(dgeObj, row, col, drop=FALSE){
    #drop supported for compatibility but has no effect in this context
    dgeObj <- subset(dgeObj, row, col, drop)
}



