### Function showAttributes ###
#' Function showAttributes
#'
#' Prints the attributes associated with a DEGobj.  This prints
#' all attributes regardless of the class of the attribute value.
#' Use showMeta if you are only interested in attributes that are
#' key/value pairs.
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
#' @importFrom dplyr setdiff
#' @export
### attributes.DGEobj
showAttributes <- function(dgeObj, skip=c("dim", "dimnames", "rownames",
                                          "colnames", "listData", "objDef")) {

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
        atnames <- dplyr::setdiff(atnames, skip)
        print(paste("atnames:", paste(atnames, collapse=", "),sep=" "))

        for (j in atnames) #print the name then the attribute value
            print(paste("[", j, "] = ", attr(dgeObj[[i]], j)))
    }
}

### Function setAttributes ###
#' Function setAttributes
#'
#' Set one or more attributes on an object.  You can use this to add attribute
#' annotation(s) to a DGEobj or to a specific item within a DGEobj.  Notably,
#' using the base attributes function to add attributes strips existing
#' attributes.  The setAttributes function adds the attributes passed to it in
#' the attribs argument without deleting attributes already present.   However,
#' A names attribute on the attribs argument list that already exists in the
#' object will be updated.  The function is generic in that it should work on
#' other datatypes/classes, not just the DGEobj.
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
#' @import magrittr
#' @importFrom assertthat assert_that
#'
#'
#' @export
setAttributes <- function(item, attribs){

    assert_that(!missing(item),
                !missing(attribs),
                class(attribs)[[1]] == "list",
                !is.null(names(attribs))
    )
    attribNames <- as.list(names(attribs))
    for (i in 1:length(attribs))
        attr(item, attribNames[[i]]) <- attribs[[i]]
    return(item)
}

### Function getItemAttribute ###
#' Function getItemAttribute
#'
#' Get a named attribute (key) from all items on a list.
#'
#' At present, there are no attributes common to all items in a
#' DGEobj.  This will work on generic lists also where each list
#' has a common set of attribute keys.
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
#' @import magrittr
#' @importFrom assertthat assert_that
#'
# Don't export.  User just needs to learn base attribute functions.
getItemAttribute <- function(dgeObj, attrName){
    assert_that(
        !missing(dgeObj),
        !missing(attrName),
        class(dgeObj)[[1]] %in% c("DGEobj", "list"),
        class(attrName)[[1]] == "character"
    )

    # myattributes <- lapply (dgeObj, function(x) attr(x, attrName))
    return(myattributes)
}

### Function getAttributes ###
#' Function getAttributes
#'
#' get all attributes from an item except for ones listed on the excludeList.
#' This is intended to capture user-defined attributes by excluding a few standard
#' attributes like class and dim.
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param item  a DGEobj structure (or any object with attributes)
#' @param excludeList A list of attribute names to exclude from the output
#'     (default = list("dim", "dimnames", "names", "row.names"))
#'
#' @return a named list of attribute values for the items
#'
#' @examples
#'    #get attributes from a DGEobj
#'    MyAttr <- getAttributes(dgeObj)
#'
#'    #get the formula attribute from the designMatrix
#'    MyAttr <- attr(dgeObj$designMatrixName, "formula")
#'
#' @export
getAttributes <- function(item, excludeList=list("dim", "dimnames",
                                                 "names", "row.names", "class")){
      at <- attributes(item)
      idx <- !names(at) %in% excludeList
      return(at[idx])
}


### Function getAttribute ###
#' Function getAttribute
#'
#' get a specified attribute from an item
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param item  a DGEobj or item
#' @param attrName Name of the attribute to retrieve.
#'
#' @return the specified attribute value (data type depends on the datatype
#' stored in the attribute) or NULL if attribute doesn't exist
#'
#' @examples
#'    #get an attribute from a DGEobj
#'    MyAttr <- getAttribute(dgeObj, "type")
#'
#'    #get an attribute from a DGEobj item
#'    MyAttr <- getAttribute(dgeObj$designMatrix, "formula")
#'
#' @importFrom assertthat assert_that
#'
# Don't export.  User just needs to learn base attribute functions.
getAttribute <- function(item, attrName){
    assert_that(!missing(item),
                !missing(attrName))

    x <- attr(item, attrName)
    return(x)
}




### Function showMeta ###
#' Function showMeta
#'
#' Prints the attributes associated with a object with a limit on the length of
#' the values stored in the attributes.  Use this to examine the key=value meta
#' data associated with a DGEobj.  Written for use with DGEobjs but
#' should function generically on any object with key/value pair attributes.
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param obj  A object with attributes to examine created by function initDGEobj
#'
#' @return A data frame with the key value pairs from the object's attributes.
#'
#' @examples
#'    df <- showMeta(MydgeObj)
#'
#' @importFrom utils stack
#' @importFrom dplyr select
#'
#' @export
showMeta <- function(obj) {
     #print length==1 attributes in a table
    alist <- attributes(obj)
    #filter for attributes that are simple key/value pairsinventory
    idx <- lapply(alist, length) == 1
    if(sum(idx) > 0){
      suppressWarnings(
        df <- stack(alist[idx])  #issues warning
      )
      colnames(df) <- c("Value", "Attribute")
      df <- dplyr::select(df, Attribute, Value)
      df$Attribute <- as.character(df$Attribute)
      return(df)
    } else {
      return(NULL)
    }
}
