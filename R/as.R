### Function convertDGEobj ###
#' Function convertDGEobj
#'
#' Casts a DGEobj class object as the specified data class.
#' Supports conversion to RangedSummarizedExperiment, Expression Set
#' or simple list.
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj, RangedSummarizedExperiment, ExpressionSet
#'
#' @param dgeObj  A class dgeObj created by function initDGEobj
#' @param Class  one of "RangedSummarizedExperiment", "RSE", "ExpressionSet", "ES"
#'   (case insensitive)
#'
#' @return A object of the specified class
#'
#' @examples
#'    MyRSE <- convertDGEobj(dgeObj, "RSE")
#'    MyES <- convertDGEobj(dgeObj, "ES")
#'    MyList <- convertDGEobj(dgeObj, "list")
#'
#' @import assertthat SummarizedExperiment
#'
#' @export
convertDGEobj <- function(dgeObj, Class){

    .toRSE <- function(counts, rowData, colData, meta){
        result <- try ({gr <- as(rowData, "GRanges")}, silent=TRUE)
        if (class(result) == "try-error")
            RSE = SummarizedExperiment(assays = list(counts=counts),
                                       rowData = rowData,
                                       colData = S4Vectors::DataFrame(colData),
                                       metadata = S4Vectors::SimpleList(meta))
        else
            RSE = SummarizedExperiment(assays = list(counts=counts),
                                       rowRanges = gr,
                                       colData = S4Vectors::DataFrame(colData),
                                       metadata = S4Vectors::SimpleList(meta))
    }

    .toES <- function(counts, rowData, colData, meta){
        RSE <- .toRSE(counts, rowData, colData, meta)
        ES <- as(RSE, "ExpressionSet")
    }

    #main
browser()
    supportedClasses <- list("RANGEDSUMMARIZEDEXPERIMENT", "RSE",
                        "EXPRESSIONSET", "ES", "LIST")
    assert_that(!missing(dgeObj),
                !missing(Class),
                class(Class) == "character",
                toupper(Class) %in% supportedClasses)

    allowedLevels <- attr(dgeObj, "objDef")$allowedLevels
    if (!attr(dgeObj, "level") %in% allowedLevels)
        stop(paste("Not a supported level. Supported levels = [",
                   paste(allowedLevels, collapse=", "),
                   "]", sep=""))

    #convert DGEobj to RSE
    counts <- getItem(dgeObj, "counts")
    colData <- getItem(dgeObj, "design")
    levelName <- paste(attr(dgeObj, "level"), "Data", sep="")
    rowData <- S4Vectors::DataFrame(getItem(dgeObj, levelName))
    meta <- getBaseType(dgeObj, "meta")

    if (toupper(Class) %in% supportedClasses)
        result <- switch(toupper(Class),
                "RSE" = .toRSE(counts, rowData, colData, meta),
                "RANGEDSUMMARIZEDEXPERIMENT" = .toRSE(counts, rowData, colData, meta),
                "ES" = .toES(counts, rowData, colData, meta),
                "EXPRESSIONSET" = .toES(counts, rowData, colData, meta),
                "LIST" = unclass(dgeObj))
    else
        stop(paste("DGEobj to ", Class, " not supported", sep=""))

    return(result)
}


### Function convertRSE ###
#' Function convertRSE
#'
#' Casts an RSE class object as a DGEobj.  Only captures count
#' data and the associated row and col annotation
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj, SummarizedExperiment, ExpressionSet
#'
#' @param RSE  A class RangedSummarizedExperiment
#' @param Class Destination class (one of "DGEobj", "ES", "ExpressionSet")
#'
#' @return A obejct of the specified class
#'
#' @examples
#'    MyDGEobj <- convertRSE(MyRSE, "DGEobj")
#'    MyES <- convertRSE(MyRSE, "ES")
#'
#' @import magrittr SummarizedExperiment
#'
#' @export
convertRSE <- function(RSE, Class){

    .toDGEobj <- function(RSE){
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

    assert_that(!missing(RSE),
                !missing(Class),
                class(RSE)[[1]] == "RangedSummarizedExperiment")

    result <- switch(toupper(Class),
                     "DGEOBJ" = .toDGEobj (RSE),
                     "ES" = as(RSE, "ExpressionSet"),
                     "ExpressionSet" = as(RSE, "ExpressionSet")
    )

    return(result)

}



#'
#' ### Function as.list.DGEobj ###
#' Function as.list.DGEobj
#'
#' Casts a DGEobj class object as a simple list.
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj
#'
#' @param dgeObj  A class dgeObj created by function initDGEobj
#'
#' @return A simple list version of the DGEobj
#'
#' @examples
#'    MyDGElist <- as.list(dgeObj)
#'
#' @export
as.list.DGEobj <- function(dgeObj){
    unclass(dgeObj)
    return(dgeObj)
}
