#colnames, rownames: point these functions to the row and col annotation of assays
colnames.DGEresult <- function(dgeResult){
    #return  SampleNames (colnames)

    idx <- dgeResult$basetype == "col"
    if (sum(idx) > 0)
        cnames <- rownames(dgeResult$data[idx][[1]])
    else { #check assays
        idx <- dgeResult$type == "assay"
        if (sum(idx) > 0)
            cnames <- colnames(dgeResult$data[idx][[1]])
        else #no col data yet
            cnames <- as.character()
    }
    return(cnames)
}
