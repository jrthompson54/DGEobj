### Function as.DGEO ###
#' Function as.DGEO
#'
#' Casts an RSE class object as a DGEobj.  Only works with a gene level RSE.
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class dgeObj created by function initDGEobj
#'
#' @return A DGEobj
#'
#' @examples
#'    MyDGEobj <- as.DGEO(MyRSE)
#'
#' @import magrittr SummarizedExperiment
#'
#' @export
as.DGEO <- function(RSE){
    assert_that(!missing(RSE),
                class(RSE)[[1]] == "RangedSummarizedExperiment")

    counts <- assay(RSE, "Counts")
    design <- colData(RSE) %>% as.data.frame
    # geneAnnotation = mcols(RSE) %>% as.data.frame
    # rownames(geneAnnotation) <- geneAnnotation$ID
    geneAnnotation = rowRanges(RSE) %>% as.data.frame

    level <-  metadata(RSE)[["Level"]] %>% tolower

    dgeObj <- initDGEobj(counts=counts,
                         rowData=geneAnnotation,
                         colData=design,
                         level=level)

    return(dgeObj)
}
