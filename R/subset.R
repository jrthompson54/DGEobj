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
subset <- function(DgeObj, row, col){
    #check if both row and col provided.
    assert_that(class(DgeObj)[[1]] == "DGEobj")

    #make sure row and col in range
    if (max(row) > max(nrow(DgeObj)))
        stop ("row coordinates out of range")
    if (max(col) > max(ncol(DgeObj)))
        stop("col coordinates out of range")

    if (missing(row))
        row <-nrow(DgeObj)
    if (missing(col))
        col <- ncol(DgeObj)

    for (i in 1:length(DgeObj)){
        switch(attr(DgeObj, "basetype")[[i]],

               row = {
                    if (is.null(dim(DgeObj$data[[i]])))
                        DgeObj$data[i] <- DgeObj$data[[i]][row]
                    else DgeObj$data[i] <- DgeObj$data[[i]][row,]
                     },

               col = {
                   if (is.null(dim(DgeObj$data[[i]])))
                       DgeObj$data[i] <- DgeObj$data[[i]][col]
                    else DgeObj$data[i] <- DgeObj$data[[i]][col,]
                    },

               assay = {
                   DgeObj$data[i] <- DgeObj$data[[i]][row, col]
               })
    }
    #update dimnames from 1st assay
    #designMatrix not subsetted correctly
    #Elist not correct
    #contrasts not correct
    #
    return(DgeObj)
}
