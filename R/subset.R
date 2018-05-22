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
#' @importFrom assertthat assert_that
#' @importFrom stringr str_c
#'
#' @export
subset.DGEobj <- function(DgeObj, row, col, drop=FALSE){

    assertthat::assert_that(class(DgeObj)[[1]] == "DGEobj")

    #fill in missing row/col args
    if (missing(row))
        row <-1:nrow(DgeObj)
    if (missing(col))
        col <- 1:ncol(DgeObj)

    #make sure row and col in range
    if (class(row)[[1]] %in% c("numeric", "integer") & max(row) > nrow(DgeObj))
        stop ("row coordinates out of range")
    if (class(col)[[1]] %in% c("numeric", "integer") & max(col) > ncol(DgeObj))
        stop("col coordinates out of range")

    #warn if named items don't exist
    if (class(row)[[1]] == "character"){
        count <- length(row)
        foundcount <- sum(row %in% rownames(DgeObj))
        if (foundcount < count)
            warning(str_c((count - foundcount), " items in row index not found in rownames(DgeObj)"))
    }
    if (class(col)[[1]] == "character"){
        count <- length(col)
        foundcount <- sum(col %in% colnames(DgeObj))
        if (foundcount < count)
            warning(str_c((count - foundcount), " items in col index not found in colnames(DgeObj)"))
    }
    #
    # Note1: subsetting a matrix with a vector of rownames doesn't work
    #
    # Note2: granges objects lack rownames.  To support named vectors as filtering indices,
    # Convert the names into a boolean index.  This also solves note1 problem as you can
    # subset a matrix with a boolean index.

    basetypes <- attr(DgeObj, "basetype")

    #if row or col is a character vector, convert to boolean index.
    if (class(row)[[1]] == "character")
        row <- rownames(DgeObj) %in% row
    if (class(col)[[1]] == "character")
        col <- colnames(DgeObj) %in% col

    for (i in 1:length(DgeObj)){

        # cat(str_c("subsetting", names(DgeObj)[i], basetypes[[i]], "\n", sep=" ")) #debug
        # cat(str_c("row length", length(row), class(row), "\n", sep=" "))
        # cat(str_c("col length", length(col), class(col), "\n", sep=" "))

        switch(basetypes[[i]],

               row = {

                   if (is.null(dim(DgeObj[[i]]))){ #not a matrix type object
                       DgeObj[[i]] <- DgeObj[[i]][row]
                   } else {
                       DgeObj[[i]] <- DgeObj[[i]][row,]
                   }
               },

               col = {
                   if (is.null(dim(DgeObj[[i]]))){
                       DgeObj[[i]] <- DgeObj[[i]][col]
                   } else {
                       DgeObj[[i]] <- DgeObj[[i]][col,]
                   }
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



