#' Reset to original data
#'
#' During a workflow, a DGEobj typically gets filtered down to remove samples
#' that fail QC or non-expressed genes. The resetDGEobj() function produces a new DGEobj with
#' the original unfiltered data. Resetting an object does not restore changes to attributes or class,
#' but does revert changes made with addItem() and rmItem(). Reset requires that *_orig data is still
#' in the DGEobj.
#'
#' @param dgeObj A class DGEobj created by function initDGEobj()
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
#'     dim(exObj)
#'
#' @importFrom assertthat assert_that
#'
#' @export

resetDGEobj <- function(dgeObj){
    assertthat::assert_that("DGEobj" %in% class(dgeObj),
                            msg = "The DGEobj must be of class 'DGEobj'.")
    assertthat::assert_that(!is.null(attr(dgeObj, "level")),
                            msg = "The DGEobj must have a 'level' attribute specified.")

    #TODO:: check that we have a design and primary assay data on the object, protect the following.

    level     <- attr(dgeObj, "level")
    levelData <- getItem(dgeObj, paste(level, "Data_orig", sep = ""))

    design    <- getItem(dgeObj, "design_orig")
    def       <- attr(dgeObj, "objDef")

    paData    <- getItem(dgeObj, paste(def$primaryAssayNames[[level]], "_orig", sep = ""))

    newObj  <- initDGEobj(primaryAssayData = paData,
                          rowData          = levelData,
                          colData          = design,
                          level            = level,
                          DGEobjDef        = def)

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
