### Function subset.DGEobj ###
#' Function subset.DGEobj (DGEobj)
#'
#' This is the function bound to square brackets.
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
#' @param debug Default=FALSE.  Set to TRUE to get more information is subsetting a
#'    DGEobj fails with a dimension error.
#'
#' @return A subsetted DGEobj class object
#'
#' @examples
#'    dgeObj <- subset(dgeObj, 1:10, 100:1000)
#'
#' @importFrom assertthat assert_that
#' @importFrom stringr str_c
#'
#' @export
subset.DGEobj <- function(dgeObj, row, col, drop=FALSE, debug=FALSE){

    assertthat::assert_that(class(dgeObj)[[1]] == "DGEobj")

    #fill in missing row/col args
    if (missing(row))
        row <-1:nrow(dgeObj)
    if (missing(col))
        col <- 1:ncol(dgeObj)

    #make sure row and col in range
    if (class(row)[[1]] %in% c("numeric", "integer") & max(row) > nrow(dgeObj))
        stop ("row coordinates out of range")
    if (class(col)[[1]] %in% c("numeric", "integer") & max(col) > ncol(dgeObj))
        stop("col coordinates out of range")

    #warn if named items don't exist
    if (class(row)[[1]] == "character"){
        count <- length(row)
        foundcount <- sum(row %in% rownames(dgeObj))
        if (foundcount < count)
            warning(stringr::str_c((count - foundcount), " items in row index not found in rownames(dgeObj)"))
    }
    if (class(col)[[1]] == "character"){
        count <- length(col)
        foundcount <- sum(col %in% colnames(dgeObj))
        if (foundcount < count)
            warning(stringr::str_c((count - foundcount), " items in col index not found in colnames(dgeObj)"))
    }
    #
    # Note1: subsetting a matrix with a vector of rownames doesn't work
    #
    # Note2: granges objects lack rownames.  To support named vectors as filtering indices,
    # Convert the names into a boolean index.  This also solves note1 problem as you can
    # subset a matrix with a boolean index.

    #need basetype to define how to subset an object
    basetypes <- attr(dgeObj, "basetype")

    #classes for which the drop argument is valid.

    dropClasses <- c("data.frame", "matrix")

    #if row or col is a character vector, convert to boolean index.
    if (class(row)[[1]] == "character")
        row <- rownames(dgeObj) %in% row
    if (class(col)[[1]] == "character")
        col <- colnames(dgeObj) %in% col

    if (debug) browser()

    for (i in 1:length(dgeObj)){

        if (debug == TRUE) {
            cat(stringr::str_c("subsetting", names(dgeObj)[i], basetypes[[i]], "\n", sep=" ")) #debug
            cat(stringr::str_c("row arg length", length(row), class(row), "\n", sep=" "))
            cat(stringr::str_c("col arg length", length(col), class(col), "\n", sep=" "))
            cat(stringr::str_c("object dim: ", nrow(dgeObj[[i]]), ":", ncol(dgeObj[[i]])))
        }

        objectClass <- class(dgeObj[[i]])[[1]]

        #get user attributes before subsetting
        userAttribs <- getAttributes(dgeObj[[i]])

        #select subset action dependent on basetype.  Do nothing for basetype = meta
        switch(basetypes[[i]],

               row = {

                   if (is.null(dim(dgeObj[[i]]))){ #not a matrix type object
                       dgeObj[[i]] <- dgeObj[[i]][row]
                   } else if (objectClass %in% dropClasses) {
                       dgeObj[[i]] <- dgeObj[[i]][row, , drop=drop]
                   } else {
                       dgeObj[[i]] <- dgeObj[[i]][row,]
                   }
               },

               col = {
                   if (is.null(dim(dgeObj[[i]]))){
                       dgeObj[[i]] <- dgeObj[[i]][col]
                   } else if (objectClass %in% dropClasses){
                       dgeObj[[i]] <- dgeObj[[i]][col, , drop=drop]
                   } else {
                       dgeObj[[i]] <- dgeObj[[i]][col,]
                   }
               },

               assay = {
                   if (objectClass %in% dropClasses){
                       dgeObj[[i]] <- dgeObj[[i]][row, col, drop=drop]
                   } else {
                       dgeObj[[i]] <- dgeObj[[i]][row, col]
                   }
               })

        #put user attributes back in place except for granges objects
        if (!"GRanges" %in% class(dgeObj[[i]]) & length(userAttribs) > 0)
            dgeObj[[i]] <- setAttributes(dgeObj[[i]], userAttribs)
    }

    return(dgeObj)
}

#' @export
`[.DGEobj` <- function(dgeObj, row, col, drop=FALSE, debug=FALSE){
    #drop supported for compatibility but has no effect in this context
    dgeObj <- subset(dgeObj, row, col, drop, debug)
}



