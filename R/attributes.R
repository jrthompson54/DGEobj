### Function showAttributes ###
#' Function showAttributes
#'
#' Prints the attributes associated with a DEGobj.
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class dgeObj created by function initDGEobj
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


