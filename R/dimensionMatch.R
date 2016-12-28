### .dimensionMatch
.dimensionMatch <- function(dgeObj, item, itemType){
    #check the dimensions of item against the DGEresult object dim
    #set the DGEresult dimension if not set (i.e. case of empty DGEresult)
    #stop with an error if item is mismatched with DGEresult dim
    #to work with this an object must return a result with the dim command
    #
    testrow <- function(dgeObj, item){
        #stop if dimensions don't match
        if (is.null(dim(item))) #item is 1D use length
            if (nrow(dgeObj) > 0 & nrow(dgeObj) != length(item))
                stop('New row object does not match row dimension of DGEobj object')
        else if (nrow(dgeObj) > 0 & nrow(dgeObj) != nrow(item))
            stop('New row object does not match row dimension of DGEobj object')
    }

    testcol <- function(dgeObj, item){
        #stop if dimensions don't match
        if (is.null(dim(item))) #item is 1D use length
            if (ncol(dgeObj) > 0 & ncol(dgeObj) != length(item))
                stop('Rows in new col object must match col dimension of DGEobj object')
        else if (ncol(dgeObj) > 0 & ncol(dgeObj) != nrow(item))
            stop('Rows in new col object must match col dimension of DGEobj object')
    }


    result <- FALSE
    switch(#get the basetype
           attr(dgeObj, "objDef")$type[[itemType]],
           "row" = testrow(dgeObj, item),
           "col" = testcol(dgsObj, item),
           "assay" = {
               testrow(dgeObj, item)
               testcol(dgeObj, item)
           }
    ) #end switch
    result <- TRUE
    return(result)
}
