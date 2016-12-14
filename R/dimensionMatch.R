### .dimensionMatch
.dimensionMatch <- function(x, ...) UseMethod(".dimensionMatch")
.dimensionMatch.default <- function(dgeResult, ...) {
    warning(paste(".dimensionMatch does not know how to handle object of class ",
                  class(dgeResult),
                  "and can only be used on class DGEresult"))
}
.dimensionMatch.DGEresult <- function(dgeResult, item, itemType){
    #check the dimensions of item against the DGEresult object dim
    #set the DGEresult dimension if not set (i.e. case of empty DGEresult)
    #stop with an error if item is mismatched with DGEresult dim
    result <- FALSE
    switch(.type[[itemType]],
           "row" = {
               if (dim(dgeResult)[1] > 0 & dim(dgeResult)[1] != nrow(item))
                   stop('New row object does not match row dimension of DGEresult object')
           },

           "col" = {
               if (dim(dgeResult)[2] > 0 & dim(dgeResult)[2] != nrow(item))
                   stop('Rows in new col object does not match col dimension of DGEresult object')
           },

           "assay" = {
               if (nrow(dgeResult) > 0 & nrow(dgeResult) != nrow(item))
                   stop('New assay object does not match row dimension of DGEresult object')
               if (ncol(dgeResult) >0 & ncol(dgeResult) != ncol(item))
                   stop('New assay object does not match col dimension of DGEresult object')
           }

    ) #end switch
    result <- TRUE
    return(result)
}
