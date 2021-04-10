#' Initialize with base data (primaryAssayData, row annotations, col annotations)
#'
#' @param primaryAssayData               A numeric matrix or dataframe with row and
#'   colnames. Each column represents a sample.  Each row represents and assay.
#'   This is typically the counts matrix in a DGE RNA-Seq experiment.
#' @param rowData              Gene, isoform or exon level annotation. Rownames
#'   must match the rownames in primaryAssayData
#' @param colData              A dataframe describing the experiment design.
#'   Rownames much match the colnames(primaryAssayData)
#' @param level                One of "gene", "isoform", or "exon"
#' @param customAttr           (optional) Named list of attributes
#' @param allowShortSampleIDs  Using sequential integer rownames (even if typed
#'   as character) is discouraged and by default will abort the DGEobj creation.
#'   If you have a legitimate need to have short sample names composed of
#'   numeric characters, you can set this argument to TRUE (default = FALSE)
#' @param DGEobjDef            An object definition. Defaults to the global
#'   DGEobj definition (initDGEobjDef()) and you usually shouldn't change this unless
#'   you're customizing the object for new data types.
#'
#' @return A DGEobj
#'
#' @examples
#'
#'    dgeObj <- readRDS(system.file("exampleObj.RDS", package = "DGEobj"))
#'    MyCounts <- dgeObj$counts
#'    geneinfo <- dgeObj$geneData
#'    sampinfo <- dgeObj$design
#'
#'   myDgeObj <- initDGEobj(primaryAssayData = MyCounts,
#'                          rowData = geneinfo,
#'                          colData = sampinfo,
#'                          level = "gene",
#'                          customAttr = list (Genome = "Rat.B6.0",
#'                                             GeneModel = "Ensembl.R89"))
#'
#' @import magrittr
#' @importFrom GenomicRanges GRanges
#' @importFrom assertthat assert_that
#'
#' @export
initDGEobj <- function(primaryAssayData,
                       rowData,
                       colData,
                       level,
                       customAttr,
                       allowShortSampleIDs = FALSE,
                       DGEobjDef = initDGEobjDef()
) {

    assertthat::assert_that(!missing(primaryAssayData),
                            !missing(colData),
                            !missing(rowData),
                            !missing(level),
                            msg = "Specify the primaryAssayData, colData, rowData, and level. All are required to initialize a DGEobj.")
    assertthat::assert_that(is.matrix(primaryAssayData) | is.data.frame(primaryAssayData),
                            msg = "primaryAssayData must be specified as a matrix or a data.frame.")
    assertthat::assert_that(level %in% DGEobjDef$allowedLevels,
                            msg = paste('The specified level must be one of: ', paste(DGEobjDef$allowedLevels, collapse=", ")))
    assertthat::assert_that(!is.null(rownames(primaryAssayData)),
                            !is.null(colnames(primaryAssayData)),
                            !is.null(rownames(rowData)),
                            !is.null(rownames(colData)),
                            msg = "primaryAssayData must have row and column names specified. rowData and colData must have rownames specified.")
    assertthat::assert_that(nrow(primaryAssayData) == nrow(rowData),
                            ncol(primaryAssayData) == nrow(colData),
                            msg = "The number of rows in primaryAssayData must match the number of rows in rowData. Similarly, the number of columns in primaryAssayData must match the number of columns in colData.")

    # Rows
    if (!all(rownames(primaryAssayData) == rownames(rowData))) {
        primaryAssayData <- primaryAssayData[order(rownames(primaryAssayData)),]
        rowData <- rowData[order(rownames(rowData)),]
    }
    # Columns
    if (!all(colnames(primaryAssayData) == rownames(colData))) {
        primaryAssayData <- primaryAssayData[,order(colnames(primaryAssayData))]
        colData <- colData[order(rownames(colData)),]
    }

    assertthat::assert_that(
        all(rownames(primaryAssayData) == rownames(rowData)),
        all(colnames(primaryAssayData) == rownames(colData)),
        msg = "The rownames for primaryAssayData must match the rownames of rowData. Similarly, the colnames of primaryAssayData must match the rownames of colData."
    )

    if (!allowShortSampleIDs == TRUE) {
        suppressWarnings(
            test <- as.numeric(rownames(colData))
        )

        if (all(is.na(test)) == FALSE) {
            sampleCount <- nrow(colData)
            minchar <- nchar(as.character(sampleCount))
            maxchar <- max(sapply(rownames(colData), nchar))
            assertthat::assert_that(maxchar > minchar,
                                    msg = paste("It looks like you have numeric sample IDs (design rownames).",
                                                "Please supply a more specific sample identifier. ",
                                                "Use allowShortSampleIDs = TRUE to explicitly override this restriction",
                                                sep = "\n"))
        }
    }

    funArgs <- match.call()

    result <- try(primaryAssayData <- as.matrix(primaryAssayData), silent = TRUE)
    if (any(class(result) == "try-error"))
        stop("Couldn't coerce primaryAssayData to a numeric matrix!")

    # Initialize an empty DGEobj
    dgeObj <- list()
    class(dgeObj) <- "DGEobj"
    attr(dgeObj, "objDef") <- DGEobjDef

    # Add empty attributes
    attr(dgeObj, "type") <- list()
    attr(dgeObj, "basetype") <- list()
    attr(dgeObj, "parent") <- list()
    attr(dgeObj, "funArgs") <- list()
    attr(dgeObj, "dateCreated") <- list()

    # Add DGEobj version (not the package version)
    # This will only change when there is a substantive change
    # to the base DGEobj structure
    attr(dgeObj, "DGEobjDef.version") <- "1.1"

    # Load required items
    # primaryAssayData (counts for RNA-Seq)
    primaryAssayName = DGEobjDef$primaryAssayNames[[level]]
    dgeObj <- addItem(dgeObj,
                      item = primaryAssayData,
                      itemName = paste(primaryAssayName, "_orig", sep=""),
                      itemType = paste(primaryAssayName, "_orig", sep=""),
                      funArgs = funArgs,
                      parent = "",
                      init = TRUE
    )

    dgeObj <- addItem(dgeObj,
                      item = primaryAssayData,
                      itemName = DGEobjDef$primaryAssayNames[[level]],
                      itemType = DGEobjDef$primaryAssayNames[[level]],
                      funArgs = funArgs,
                      parent = paste(primaryAssayName, "_orig", sep=""),
                      init = TRUE
    )

    # colData
    dgeObj <- addItem(dgeObj,
                      item = colData,
                      itemName = "design_orig",
                      itemType = "design_orig",
                      funArgs = funArgs,
                      parent = "")

    dgeObj <- addItem(dgeObj,
                      item = colData,
                      itemName = "design",
                      itemType = "design",
                      funArgs = funArgs,
                      parent = "design_orig")

    # rowData
    level    <- tolower(level)
    itemName <- paste(level, "Data", sep = "")

    itemType <- itemName
    parent   <- paste(itemName, "_orig", sep = "")
    grparent <- itemName

    dgeObj <- addItem(dgeObj,
                      item = rowData,
                      itemName = parent,
                      itemType = parent,
                      funArgs = funArgs,
                      parent = "")

    dgeObj <- addItem(dgeObj,
                      item = rowData,
                      itemName = itemName,
                      itemType = itemType,
                      funArgs = funArgs,
                      parent = parent)

    # Annotate the level
    dgeObj %<>% setAttributes(list(level = level))

    result <- try({gr <- GenomicRanges::GRanges(rowData)}, silent = TRUE)

    if (class(result) == "try-error") {
        warning("Couldn't build a GRanges object!")
    } else {
        dgeObj <- addItem(dgeObj,
                          item = gr,
                          itemName = "granges_orig",
                          itemType = "granges_orig",
                          funArgs = funArgs,
                          parent = paste(grparent, "_orig", sep = ""))

        dgeObj <- addItem(dgeObj,
                          item = gr,
                          itemName = "granges",
                          itemType = "granges",
                          funArgs = funArgs,
                          parent = grparent)
    }

    if (!missing(customAttr))
        dgeObj <- setAttributes(dgeObj, customAttr)

    return(dgeObj)
}


#' Instantiate a class DGEobjDef object.
#'
#' @param levels               A character string or vector providing names for new levels
#' @param primaryAssayNames    A character string or vector, must be the same length as levels
#'   This argument supplies the primaryAssayNames for the corresponding levels.
#' @param types                A named character vector of new types where the
#'   values indicate the basetype for each named type (optional)
#' @param uniqueTypes          A name or vector of names to add to the uniqueType list (optional)
#'
#' @return A class DGEobjDef object suitable for use with initDGEobj
#'
#' @examples
#'     # return the default DGEobj definition
#'     myDGEobjDef <- initDGEobjDef()
#'
#'     # Optionally add some new types and levels for metabolomics data
#'      myDGEobjDef <- initDGEobjDef(levels = "metabolomics",
#'                                   primaryAssayNames = "intensity",
#'                                   types <- c(normalizedIntensity = "assay"))
#'
#'     # When a new level is defined, the itemNames and types for the
#'     # rowData and colData are automatically established.  The
#'     # types argument is only needed to define downstream workflow objects.
#'
#' @importFrom assertthat assert_that
#'
#' @export
initDGEobjDef <- function(levels, primaryAssayNames, types, uniqueTypes){

    newDef <- .DGEobjDef

    if (!missing(levels)) {
        assertthat::assert_that("character" %in% class(levels),
                                msg = "levels must be a character string or vector")
        assertthat::assert_that(!any(levels %in% newDef$allowedLevels),
                                msg = "Abort. One or more levels already exists.")
        assertthat::assert_that(!missing(primaryAssayNames),
                                msg = "primaryAssayNames is required when specifying levels")
        assertthat::assert_that(length(primaryAssayNames) == length(levels),
                                msg = "A primaryAssayName must be specified for each level.")
        assertthat::assert_that("character" %in% class(primaryAssayNames),
                                msg = "primaryAssayNames must be a character string or vector")

        #add the new level(s)
        newDef$allowedLevels <- c(newDef$allowedLevels, levels)

        # Add associated types
        # primaryAssay type
        newTypes <- rep("assay", length(primaryAssayNames))
        names(newTypes) <- primaryAssayNames
        origTypes <- rep("meta", length(newTypes))
        names(origTypes) <- paste(names(newTypes), "_orig", sep = "")
        newDef$type <- c(newDef$type, newTypes, origTypes)
        newDef$uniqueType <- c(newDef$uniqueType, names(newTypes), names(origTypes))
        names(primaryAssayNames) <- levels
        newDef$primaryAssayNames <- c(newDef$primaryAssayNames, primaryAssayNames)

        # rowData type
        rowDataName <- paste(levels, "Data", sep = "")
        newTypes <- rep("row", length(rowDataName))
        names(newTypes) <- rowDataName
        origTypes <- rep("meta", length(newTypes))
        names(origTypes) <- paste(names(newTypes), "_orig", sep = "")
        newDef$type <- c(newDef$type, newTypes, origTypes)
        newDef$uniqueType <- c(newDef$uniqueType, names(newTypes))
    }

    if (!missing(primaryAssayNames)) {
        assertthat::assert_that(!missing(levels),
                                msg = "Must use levels arg with primaryAssayNames.")
    }

    if (!missing(types)) {
        assertthat::assert_that("character" %in% class(types),
                                msg = "types argument must be a named character vector.")
        assertthat::assert_that(!is.null(names(types)),
                                msg = "The types vector must have names.")
        #only new types allowed, reject if type name already exists
        assertthat::assert_that(!any(names(types) %in% names(newDef$type)),
                                msg = "At least one of the new types already exists.")

        #all type values must be a basetype
        assertthat::assert_that(all(types %in% newDef$basetype),
                                msg = paste("The type values must be one of:",
                                            paste(newDef$basetype, collapse = " ")))

        #add the new type(s)
        newDef$type <- c(newDef$type, types)
    }

    if (!missing(uniqueTypes)) {
        assertthat::assert_that("character" %in% class(uniqueTypes),
                                msg = "uniqueTypes must be a character string or vector")
        assertthat::assert_that(all(uniqueTypes %in% names(newDef$type)),
                                msg =  "Not a valid type.")

        #add them and remove dups
        newDef$uniqueType <- unique(c(newDef$uniqueType, uniqueTypes))
    }

    return(newDef)
}
