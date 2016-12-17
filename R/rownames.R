# rownames <- function(x, ...) UseMethod("rownames")
# rownames.default <- function(dgeResult, ...){
#     warning(paste("rownames does not know how to handle object of class ",
#                   class(dgeResult),
#                   "and can only be used on class DGEresult"))
# }
rownames.DGEresult <- function(dgeResult){
    #return gene names
    print("F. rownames.DGEresult")
    idx <- dgeResult$basetype == "row"
    if (sum(idx) >0)
        rnames <- rownames(dgeResult$data[idx][[1]])
    else { #check assays
        idx <- dgeResult$basetype =="assay"
        if (sum(idx) > 0)
            rnames <- rownames(dgeResult$data[idx][[1]])
        else
            rnames <- as.character()
    }
    return(rnames)
}
