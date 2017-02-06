### Function setAttributes ###
#' Function setAttributes
#'
#' Set one or more attributes on an object
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param item  Anything you want to attach attrbutes to
#' @param attribs A named list of attribute/value pairs
#'
#' @return the item with new attributes
#'
#' @examples
#'    #assign attributes to a DGEobj
#'    MyAttributes <- list(Platform = "RNA-Seq",
#'            Instrument = "HiSeq",
#'            Vendor = "BMS",
#'            readType = "PE",
#'            readLength = 75,
#'            strandSpecific = TRUE)
#'    MyDGEobj <- setAttributes(MyDGEObj, MyAttributes)
#'
#'    #set attributes on an item inside a DGEobj
#'    MyAttributes <- list(normalized = FALSE,
#'                         LowIntFilter = "FPK >5 in >= 1 group"),
#'    MyDGEObj[["counts"]] <- setAttributes(MyDGEObj[["counts"]], MyAttributes)
#'
#' @import magrittr assertthat dplyr knitr
#'
#' @export
setAttributes <- function(item, attribs){

    assert_that(!missing(item),
                !missing(attribs),
                class(attribs)[[1]] == "list",
                !is.null(names(attribs))
    )

    for (i in 1:length(attribs))
        attr(item, names(attribs[i])) <- attribs[[i]]
    return(item)
}

### Function getItemAttributes ###
#' Function getItemAttributes
#'
#' get a named attribute from all items
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  a DGEobj structre
#' @param attrName The name of the item attribute to retrieve
#'
#' @return a list of attribute values for the items
#'
#' @examples
#'    #assign attributes to a DGEobj
#'    MyTypes <- getItemAttributes(dgeObj, "type")
#'    MyBaseTypes <- getItemAttributes(dgeObj, "basetype")
#'    MyCreationDates <- getItemAttributes(dgeObj, "dateCreated")
#'
#' @import magrittr assertthat dplyr knitr
#'
#' @export
getItemAttributes <- function(dgeObj, attrName){
    assert_that(
        !missing(dgeObj),
        !missing(attrName),
        class(dgeObj)[[1]] == "DGEobj",
        class(attrName)[[1]] == "character"
    )

    myattributes <- lapply (dgeObj, function(x) attr(x, attrName))
    return(myattributes)
}



