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
#'
#' @return A subsetted DGEobj class object
#'
#' @examples
#'    DgeObj <- subset(DgeObj, 1:10, 100:1000)
#'
#' @export
subset.DGEobj <- function(DgeObj, row, col){
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

        #save the item attributes (attributes will be stripped in the subsetting)
        at <- getAttributes(DgeObj[[i]])

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
        #restore the attributes, if any
        if (length(at) > 0)
            DgeObj[[i]] <- setAttributes(DgeObj[[i]], at)
    }

    return(DgeObj)
}


