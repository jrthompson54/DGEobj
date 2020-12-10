#' Print attributes
#'
#' This function prints all attributes regardless of the class of the attribute value.
#'
#' *Note* Use showMeta() to only retrieve attributes that are key/value pairs.
#'
#' @param dgeObj    A DGEobj
#' @param skipList  A character vector of attributes to skip. Use this to avoid printing certain lengthy attributes like rownames.  Defaults to c("dim", "dimnames", "rownames", "colnames", "listData", "objDef")
#'
#' @examples
#'     # example DGEobj
#'     exObj <- readRDS(system.file("exampleObj.RDS", package = "DGEobj"))
#'
#'    showAttributes(exObj)
#'
#' @export
showAttributes <- function(dgeObj, skipList = c("dim", "dimnames", "rownames", "colnames", "listData", "objDef")) {

    at <- attributes(dgeObj)
    if (length(at) > 0) {
        print(names(at))
    }

    for (i in 1:length(dgeObj)) {
        dataName <- names(dgeObj)[i]
        print(paste("dataName", ":", sep = ""))

        atnames <- names(attributes(dgeObj[[i]]))
        atnames <- atnames[!(atnames %in% skipList)]
        print(paste("atnames:", paste(atnames, collapse = ", "), sep = " "))

        for (j in atnames) {
            cat(paste("[", j, "] = "))
            print(attr(dgeObj[[i]], j))
        }
    }
    invisible(NULL)
}


#' Set attributes
#'
#' Set one or more attributes on a DGEobj or on a specific item within a DGEobj.
#'
#' This function adds attributes without deleting the attributes that are already
#' present. Any named attribute that already exists in the object will be updated.
#' To remove an attribute from an object pass NULL as the attribute value.
#'
#' @param dgeObj  A DGEobj
#' @param attribs A named list of attribute/value pairs
#'
#' @return A DGEobj
#'
#' @examples
#'     # example DGEobj
#'     exObj <- readRDS(system.file("exampleObj.RDS", package = "DGEobj"))
#'
#'     # Assign attributes to a DGEobj
#'     MyAttributes <- list(Platform       = "RNA-Seq",
#'                          Instrument     = "HiSeq",
#'                          Vendor         = "Unknown",
#'                          readType       = "PE",
#'                          readLength     = 75,
#'                          strandSpecific = TRUE)
#'     exObj <- setAttributes(exObj, MyAttributes)
#'
#'     # Set attributes on an item inside a DGEobj
#'     MyAttributes <- list(normalized   = FALSE,
#'                          LowIntFilter = "FPK >5 in >= 1 group")
#'     exObj[["counts"]] <- setAttributes(exObj[["counts"]], MyAttributes)
#'
#' @import magrittr
#' @importFrom assertthat assert_that
#'
#' @export
setAttributes <- function(dgeObj, attribs){

    assertthat::assert_that(!missing(dgeObj),
                            !missing(attribs),
                            msg = "Specify both a DGEobj and the attributes (attribs).")
    assertthat::assert_that(class(attribs)[[1]] == "list",
                            msg = "attribs must be of class 'list'.")
    assertthat::assert_that(!is.null(names(attribs)),
                            msg = "The attribs list should be a named list, specifying the attribute/value pairs. It must have names specified.")

    attribNames <- as.list(names(attribs))
    for (i in 1:length(attribs))
        dgeObj <- setAttribute(dgeObj, attribs[[i]], attribNames[[i]])
    return(dgeObj)
}


#' Set an attribute
#'
#' Set an attribute on a DGEobj or on a specific item within a DGEobj.
#'
#' The function adds or updates the attribute passed to it.  To remove an attribute,
#' pass NULL as the attribute value.
#'
#' @param dgeObj     A DGEobj
#' @param attrib     An attribute value to add
#' @param attribName A name for the attribute
#'
#' @return A DGEobj
#'
#' @examples
#'     # example DGEobj
#'     exObj <- readRDS(system.file("exampleObj.RDS", package = "DGEobj"))
#'
#'     # Assign attribute to a DGEobj
#'     exObj <- setAttribute(exObj, "RNA-SEQ", "Platform")
#'
#'     # Set attributes on an item inside a DGEobj
#'     exObj[["counts"]] <- setAttribute(exObj[["counts"]], FALSE, "normalized")
#'
#' @importFrom assertthat assert_that
#'
#' @export
setAttribute <- function(dgeObj, attrib, attribName) {

    assertthat::assert_that(!missing(dgeObj),
                            !missing(attrib),
                            !missing(attribName),
                            msg = "Specify a DGEobj, the attribute, and the attribute name")
    assertthat::assert_that(class(attribName) == "character",
                            msg = "attribName must be of class 'character'.")

    attr(dgeObj, attribName) <- attrib
    return(dgeObj)
}

#' Get all attributes
#'
#' Get all user-defined attributes from a DGEobj except for any listed in the
#' excludeList argument.
#'
#' @param dgeObj      A DGEobj
#' @param excludeList A list of attribute names to exclude from the output (default = list("dim", "dimnames", "names", "row.names"))
#'
#' @return A named list
#'
#' @examples
#'     # example DGEobj
#'     exObj <- readRDS(system.file("exampleObj.RDS", package = "DGEobj"))
#'
#'     getAttributes(exObj)
#'
#'     # Get the formula attribute from the design (if set)
#'     attr(exObj$design, "formula")
#'
#' @export
getAttributes <- function(dgeObj,
                          excludeList = list("dim", "dimnames",
                                             "names", "row.names", "class")){
    at <- attributes(dgeObj)
    idx <- !names(at) %in% excludeList
    return(at[idx])
}


#' Get a specified attribute
#'
#' @param dgeObj   A DGEobj
#' @param attrName Name of the attribute to retrieve
#'
#' @return The specified attribute value or NULL if the attribute doesn't exist
#'
#' @examples
#'     # example DGEobj
#'     exObj <- readRDS(system.file("exampleObj.RDS", package = "DGEobj"))
#'
#'     # Get an attribute from a DGEobj
#'     getAttribute(exObj, "type")
#'
#'     # Get an attribute from a DGEobj item
#'     getAttribute(exObj$designMatrix, "formula")
#'
#' @importFrom assertthat assert_that
#'
#' @export
getAttribute <- function(dgeObj, attrName){
    assertthat::assert_that(!missing(dgeObj),
                            !missing(attrName),
                            msg = "A DGEobj and an attribute name (attrName) are required.")

    x <- attr(dgeObj, attrName)
    return(x)
}


#' Retrieve the Key/Value metadata attributes
#'
#' @param dgeObj   A DGEobj with attributes
#' @param printed  Whether to print the list (default = TRUE)
#'
#' @return A data.frame with "Attribute" and "Value" columns
#'
#' @examples
#'     # example DGEobj
#'     exObj <- readRDS(system.file("exampleObj.RDS", package = "DGEobj"))
#'
#'     showMeta(exObj)
#'
#' @importFrom utils stack
#'
#' @export
showMeta <- function(dgeObj, printed = TRUE) {
    alist <- attributes(dgeObj)

    idx <- lapply(alist, length) == 1

    if (sum(idx) > 0) {
        suppressWarnings(
            df <- utils::stack(alist[idx])
        )
        colnames(df) <- c("Value", "Attribute")
        df <- df[, c("Attribute", "Value")]
        df$Attribute <- as.character(df$Attribute)
        if (printed) {
            print(df)
        }
        return(df)
    } else {
        return(NULL)
    }
}
