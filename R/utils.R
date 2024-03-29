#' Get the "assay" dimensions (row/genes by col/samples)
#'
#' Returns the dimensions of the assay data (baseType)
#'
#' @param x  A class DGEobj created by function initDGEobj()
#'
#' @return An integer vector [r,c] with a length of 2.
#'
#' @export
dim.DGEobj <- function(x) {
    dimension <- c(0, 0)

    idx <- attr(x, "basetype") == "assay"
    myassays <- unclass(x)[idx]

    if (length(myassays) > 0)
        dimension <- dim(myassays[[1]])

    return(dimension)
}


#' Get the "assay" names (row/genes by col/samples)
#'
#' Returns a list of length 2 containing the the assay data names (baseType)
#'
#' @param x  A class DGEobj created by function initDGEobj()
#'
#' @return A list of length 2 containing rownames and colnames of the DGEobj
#'
#' @export
dimnames.DGEobj <- function(x){
    firstAssay <- getBaseType(x, "assay")[[1]]
    return(list(rownames = rownames(firstAssay), colnames = colnames(firstAssay)))
}


#' Print the Inventory
#'
#' @param x       A class DGEobj created by function initDGEobj()
#' @param ...     Additional parameters
#' @param verbose Include funArgs column in the output (default = FALSE)
#'
#' @return NULL
#'
#' @examples
#'     # example DGEobj
#'     exObj <- readRDS(system.file("miniObj.RDS", package = "DGEobj"))
#'
#'     print(exObj)
#'
#' @export
print.DGEobj <- function(x, ..., verbose = FALSE) {
    df <- inventory(x, verbose = verbose)
    print(df)
    return(invisible(x))
}


#' Retrieve the object inventory
#'
#' @param dgeObj A class DGEobj created by function initDGEobj()
#' @param verbose Include funArgs column in the output (default = FALSE)
#'
#' @return A data.frame summarizing the data contained in the DGEobj
#'
#' @examples
#'     # example DGEobj
#'     exObj <- readRDS(system.file("miniObj.RDS", package = "DGEobj"))
#'
#'     inventory(exObj)
#'
#' @export
inventory <- function(dgeObj, verbose = FALSE)  {
    ItemNames <- names(dgeObj)
    ItemTypes <- attr(dgeObj, "type")
    BaseTypes <- attr(dgeObj, "basetype")
    Parents <- attr(dgeObj, "parent")
    creationDates <- attr(dgeObj, "dateCreated")
    FunArgs <- attr(dgeObj, "funArgs")
    Class <- lapply(dgeObj, class)
    Class <- lapply(Class, `[[`, 1)
    Row <- rep(NA, length(dgeObj))
    Col <- rep(NA, length(dgeObj))

    # Get length/dimensions
    for (i in 1:length(dgeObj)) {
        if (is.null(dim(dgeObj[[i]]))) {
            Row[i] <- length(dgeObj[[i]])
        } else {
            Dim <- dim(dgeObj[[i]])
            Row[i] <- Dim[1]
            Col[i] <- Dim[2]
        }
    }

    df <- data.frame(cbind(ItemName = ItemNames,
                           ItemType = ItemTypes,
                           BaseType = BaseTypes,
                           Parent = Parents,
                           Class = Class,
                           Row = Row,
                           Col = Col,
                           DateCreated = creationDates),
                     row.names = NULL)
    if (verbose == TRUE) {
        df$FunArgs <- unlist(lapply(FunArgs, paste0, collapse = "; "))
    }
    return(df)
}

#' Cast as a simple list
#'
#' @param x    A DGEobj
#' @param ...  Additional parameters
#'
#' @return A named list
#'
#' @examples
#'     # example DGEobj
#'     exObj <- readRDS(system.file("miniObj.RDS", package = "DGEobj"))
#'
#'     mylist <- as.list(exObj)
#'
#' @export
as.list.DGEobj <- function(x, ...){
    x2 <- unclass(x)
    return(x2)
}
