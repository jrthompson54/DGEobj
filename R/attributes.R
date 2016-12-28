### Function showAttributes ###
#' Function showAttributes
#'
#' Prints the attributes associated with a DEGobj.
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class dgeObj created by function initDGEobj
#'
#' @return Prints a list of attributes and values.
#'
#' @examples
#'    showAttributes(MydgeObj)
#'
#' @export
### attributes.DGEobj
showAttributes <- function(dgeObj) {

    #first show the main DGEobj attributes (omitting assayDimnames):
    # at <- attributes(unclass(dgeObj))C
    at <- attributes(dgeObj)
    if (length(at) >0){
        print(names(at))
    }

    #Now print attributes from each data item except dimnnames
    for (i in 1:length(dgeObj)) {
        dataName <- names(dgeObj$data)[i]
        print(paste("dataName", ":", sep=""))

        atnames <- names(attributes(dgeObj$data[[i]]))
        #drop dimnames. we're interested in other custom attributes here
        atnames <- dplyr::setdiff(atnames, c("dim", "dimnames", "rownames",
                                             "colnames", "listData"))
        print(paste("atnames:", paste(atnames, collapse=", "),sep=" "))

        for (j in atnames) #print the name then the attribute value
            print(paste("[", j, "] = ", attr(dgeObj$data[[i]], j)))
    }
}


