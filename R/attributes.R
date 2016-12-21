#' @export
### attributes.DGEobj
showAttributes <- function(dgeObj) {
    #Traverse the datachain and report any attributes.
    #first show the main DGEobj attributes (omitting assayDimnames):
    at <- attributes(unclass(dgeObj))
    if (length(at) >0){
        idx <- setdiff(names(at), "assayDimnames")
        print(names(at[idx]))
    }

    #Now print attributes from each data item except dimnnames
    for (i in 1:length(dgeObj)) {
        print("F. attributes.DGEobj")
        dataName <- names(dgeObj$data)[i]
        print(paste("dataName", ":", sep=""))

        atnames <- names(attributes(dgeObj$data[[i]]))
        #drop dimnames. we're interested in other attributes here
        atnames <- setdiff(atnames, c("dim", "dimnames", "rownames", "colnames", "assayDimnames", "listData"))
        print(paste("atnames:", paste(atnames, collapse=", "),sep=" "))

        for (j in atnames) #print the name then the attribute value
            print(paste("[", j, "] = ", attr(dgeObj$data[[i]], j)))
    }
}


