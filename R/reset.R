#' Reset to original data
#'
#' During a workflow, a DGEobj typically gets filtered down to remove samples
#' that fail QC or non-expressed genes.  The resetDGEobj() function produces a new DGEobj with
#' the original unfiltered data.
#'
#' @param dgeObj A DGEobj
#'
#' @return A DGEobj
#'
#' @examples
#'     #example object
#'     exObj <- readRDS(system.file("exampleObj.RDS", package = "DGEobj"))
#'
#'     # subset to first 10 rows to show reset functionality
#'     exObj <- exObj[c(1:10), ]
#'
#'     exObj <- resetDGEobj(exObj)
#'
#' @importFrom assertthat assert_that
#'
#' @export
resetDGEobj <- function(dgeObj){
    platform.types <- c("rna-seq", "rnaseq", "affymetrix")

    assertthat::assert_that("DGEobj" %in% class(dgeObj),
                            msg = "The DGEobj must be of class 'DGEobj'.")
    assertthat::assert_that(!is.null(attr(dgeObj, "level")),
                            msg = "The DGEobj must have a 'level' attribute specified.")
    assertthat::assert_that(!is.null(attr(dgeObj, "PlatformType")),
                            msg = "Required attribute \"PlatformType\" is missing.")

    platformType <- tolower(attr(dgeObj, "PlatformType"))
    counts       <- getItem(dgeObj, "counts_orig")
    design       <- getItem(dgeObj, "design_orig")

    if ("geneData_orig" %in% names(dgeObj)) {
        rowData <- getItem(dgeObj, "geneData_orig")
    } else if ("isoformData_orig" %in% names(dgeObj)) {
        rowData <- getItem(dgeObj, "isoformData_orig")
    } else if ("exonData_orig" %in% names(dgeObj)) {
        rowData <- getItem(dgeObj, "exonData_orig")
    } else if ("proteinData_orig" %in% names(dgeObj)) {
        rowData <- getItem(dgeObj, "proteinData_orig")
    } else {
        stop("Gene/isoform/exon/protein data not found")
    }

    if (tolower(platformType) %in% platform.types) {
        newObj <- initDGEobj(counts    = counts,
                             rowData   = rowData,
                             colData   = design,
                             level     = attr(dgeObj, "level"),
                             DGEobjDef = attr(dgeObj, "objDef"))
    } else {
        stop("The PlatformType attribute value was not recognized!")
    }

    if ("effectiveLength_orig" %in% names(dgeObj)) {
        newObj <- addItem(newObj,
                          item = dgeObj$effectiveLength_orig,
                          itemName = "effectiveLength_orig",
                          itemType = "effectiveLength_orig")

        newObj <- addItem(newObj,
                          item = dgeObj$effectiveLength_orig,
                          itemName = "effectiveLength",
                          itemType = "effectiveLength",
                          parent = "effectiveLength_orig")
    }

    excludeList <- list("names",
                        "class",
                        "row.names",
                        "dim",
                        "dimnames",
                        "objDef",
                        "type",
                        "itemName",
                        "itemType",
                        "basetype",
                        "parent",
                        "funArgs",
                        "level",
                        "dateCreated")
    attributes.dgeObj <- getAttributes(dgeObj, excludeList = excludeList)
    for (at in names(attributes.dgeObj)) {
        attr(newObj, at) <- attributes.dgeObj[[at]]
    }

    return(newObj)
}
