rownames.DGEresult <- function(dgeResult){
    #get from first col object, then try assay
    idx <- dgeResult$type == "row"
    if (sum(idx) >0)
        rnames <- rownames(dgeResult$data[[idx[1]]])
    else { #check assays
        idx <- dgeResult$type =="assay"
        if (sum(idx) > 0)
            rnames <- rownames(dgeResult$data[[idx[1]]])
        else
            rnames <- as.character()
    }
    return(rnames)
}
