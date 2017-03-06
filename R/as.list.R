### Function as.DGEobj ###
#' Function as.DGEobj
#'
#' Casts a DGEobj class object as the specified data class.
#' Supports conversion to RangedSummarizedExperiment, Expression Set
#' or simple list.
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq, DGEobj, RangedSummarizedExperiment, ExpressionSet
#'
#' @param dgeObj  A class dgeObj created by function initDGEobj
#'
#' @return A object of the specified class
#'
#' @examples
#'    MyDGElist <- as.list(dgeObj)
#'
#' @import assertthat SummarizedExperiment
#'
#' @export
as.DGEobj <- function(dgeObj, newClass){

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
    assert_that(!missing(dgeObj),
                !missing(newClass),
                class(newClass) == "character")

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

    supportedClasses <- list("RSE", "ES", "LIST")
    if (toupper(newClass) %in% supportedClasses)
        x <- switch(toupper(newClass),
                        "RSE" = .toRSE(counts, rowData, colData, meta),
                        "ES" = .toES(counts, rowData, colData, meta),
                        "LIST" = unclass(dgeObj))
    else
        stop(paste("DGEobj to ", newClass, " not supported", sep=""))

    return(x)
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
