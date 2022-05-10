
#' Subset internal row or column data
#'
#' @param x     A class DGEobj created by function initDGEobj()
#' @param ...   Additional parameters
#' @param row   Row index for the subset
#' @param col   Col index for the subset
#' @param drop  Included for compatibility only
#' @param debug (default = FALSE) Set to TRUE to get additional information on the console if subsetting a DGEobj fails with a dimension error.
#'
#' @return A DGEobj
#'
#' @examples
#' \dontrun{
#'     # example DGEobj
#'     exObj <- readRDS(system.file("exampleObj.RDS", package = "DGEobj"))
#'
#'     exObj <- subset(exObj, 1:10, 5:50)
#'}
#'
#' @importFrom assertthat assert_that
#' @importFrom stringr str_c
#'
#' @export
subset.DGEobj <- function(x, ..., row, col, drop = FALSE, debug = FALSE){

    assertthat::assert_that("DGEobj" %in% class(x),
                            msg = "x must be of class 'DGEobj'.")

    # Fill in missing row/col args
    if (missing(row))
        row <- 1:nrow(x)
    if (missing(col))
        col <- 1:ncol(x)

    # Make sure row and col in range
    if (class(row)[[1]] %in% c("numeric", "integer") & max(row) > nrow(x))
        stop("row coordinates out of range")
    if (class(col)[[1]] %in% c("numeric", "integer") & max(col) > ncol(x))
        stop("col coordinates out of range")

    # Warn if named items don't exist
    if ("character" %in% class(row)) {
        count <- length(row)
        foundcount <- sum(row %in% rownames(x))
        if (foundcount < count)
            warning(stringr::str_c((count - foundcount), " items in row index not found in rownames(x)"))
    }
    if ("character" %in% class(col)) {
        count <- length(col)
        foundcount <- sum(col %in% colnames(x))
        if (foundcount < count)
            warning(stringr::str_c((count - foundcount), " items in col index not found in colnames(x)"))
    }

    basetypes <- attr(x, "basetype")
    dropClasses <- c("data.frame", "matrix")

    if ("character" %in% class(row))
        row <- rownames(x) %in% row
    if ("character" %in% class(col))
        col <- colnames(x) %in% col

    for (i in 1:length(x)) {
        if (debug == TRUE) {
            cat(stringr::str_c("subsetting", names(x)[i], basetypes[[i]], "\n", sep = " "))
            cat(stringr::str_c("row arg length", length(row), class(row), "\n", sep = " "))
            cat(stringr::str_c("col arg length", length(col), class(col), "\n", sep = " "))
            cat(stringr::str_c("object dim: ", nrow(x[[i]]), ":", ncol(x[[i]])))
        }

        objectClass <- class(x[[i]])[[1]]
        userAttribs <- getAttributes(x[[i]])

        switch(basetypes[[i]],
               row = {
                   if (is.null(dim(x[[i]]))) {
                       x[[i]] <- x[[i]][row]
                   } else if (objectClass %in% dropClasses) {
                       x[[i]] <- x[[i]][row, , drop = drop]
                   } else {
                       x[[i]] <- x[[i]][row,]
                   }
               },

               col = {
                   if (is.null(dim(x[[i]]))) {
                       x[[i]] <- x[[i]][col]
                   } else if (objectClass %in% dropClasses) {
                       x[[i]] <- x[[i]][col, , drop = drop]
                   } else {
                       x[[i]] <- x[[i]][col,]
                   }
               },

               assay = {
                   if (objectClass %in% dropClasses) {
                       x[[i]] <- x[[i]][row, col, drop = drop]
                   } else {
                       x[[i]] <- x[[i]][row, col]
                   }
               })

        if (!"GRanges" %in% class(x[[i]]) & length(userAttribs) > 0)
            x[[i]] <- setAttributes(x[[i]], userAttribs)
    }

    return(x)
}


#' Subset with square brackets
#'
#' @param x    A DGEobj
#' @param ...  Additional parameters
#'
#' @return A DGEobj
#'
#' @export
`[.DGEobj` <- function(x, ...){
    row <- NULL
    col <- NULL

    dot.args <- as.list(substitute(list(...)))

    if (("row" %in% names(dot.args)) || ("col" %in% names(dot.args))) {
        # using named arguments, ignore unnamed arguments
        if ("row" %in% names(dot.args)) {
            row <- eval(dot.args$row, parent.frame())
        }
        if ("col" %in% names(dot.args)) {
            col <- eval(dot.args$col, parent.frame())
        }
    } else {
        if (length(dot.args) >= 2 && (dot.args[[2]] != "") && is.null(names(dot.args[[2]]))) {
            row <- eval(dot.args[[2]], parent.frame())
        }
        if (length(dot.args) >= 3 && (dot.args[[3]] != "") && is.null(names(dot.args[[3]]))) {
            col <- eval(dot.args[[3]], parent.frame())
        }
    }

    if (!is.null(row)) {
        if (!is.null(col)) {
            subset(x, row = row, col = col)
        } else {
            subset(x, row = row)
        }
    } else {
        if (!is.null(col)) {
            subset(x, col = col)
        } else {
            subset(x)
        }
    }
}
