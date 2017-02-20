### Function showAttributes ###
#' Function showAttributes
#'
#' Prints the attributes associated with a DEGobj.
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A DGEobj created by function initDGEobj
#' @param skip  A character vector of attributes to skip. Use this to avoid
#'   printing certain lengthy attributes like rownames.  Defaults to c("dim",
#'   dimnames", "rownames", "colnames", "listData", "objDef")
#'
#' @return Prints a list of attributes and values.
#'
#' @examples
#'    showAttributes(MydgeObj)
#'
#' @export
### attributes.DGEobj
showAttributes <- function(dgeObj, skip) {

    #first show the main DGEobj attributes (omitting assayDimnames):
    # at <- attributes(unclass(dgeObj))C
    at <- attributes(dgeObj)
    if (length(at) >0){
        print(names(at))
    }

    #Now print attributes from each data item except dimnnames
    for (i in 1:length(dgeObj)) {
        dataName <- names(dgeObj)[i]
        print(paste("dataName", ":", sep=""))

        atnames <- names(attributes(dgeObj[[i]]))
        #drop dimnames. we're interested in other custom attributes here
        atnames <- dplyr::setdiff(atnames, c("dim", "dimnames", "rownames",
                                             "colnames", "listData", "objDef"))
        print(paste("atnames:", paste(atnames, collapse=", "),sep=" "))

        for (j in atnames) #print the name then the attribute value
            print(paste("[", j, "] = ", attr(dgeObj[[i]], j)))
    }
}

### Function setAttributes ###
#' Function setAttributes
#'
#' Set one or more attributes on an object
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param item  Anything you want to attach attributes to
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

### Function getItemAttribute ###
#' Function getItemAttribute
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
#'    MyTypes <- getItemAttribute(dgeObj, "type")
#'    MyBaseTypes <- getItemAttribute(dgeObj, "basetype")
#'    MyCreationDates <- getItemAttribute(dgeObj, "dateCreated")
#'
#' @import magrittr assertthat dplyr knitr
#'
#' @export
getItemAttribute <- function(dgeObj, attrName){
    assert_that(
        !missing(dgeObj),
        !missing(attrName),
        class(dgeObj)[[1]] == "DGEobj",
        class(attrName)[[1]] == "character"
    )

    myattributes <- lapply (dgeObj, function(x) attr(x, attrName))
    return(myattributes)
}

### Function getAttributes ###
#' Function getAttributes
#'
#' get all attributes from an item
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param item  a DGEobj structre
#' @param excludeList A list of attribute names to exclude from the output
#'     (default = list("dim", "dimnames"))
#'
#' @return a list of attribute values for the items
#'
#' @examples
#'    #assign attributes to a DGEobj
#'    MyAttr <- getAttributes(dgeObj$counts)
#'
#' @import magrittr assertthat dplyr knitr
#'
#' @export
getAttributes <- function(item, excludeList=list("dim", "dimnames")){
      at <- attributes(item)
      idx <- !names(at) %in% excludeList
      return(at[idx])
}


#'
