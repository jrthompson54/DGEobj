### Function resetDGEobj ###
#' Function  resetDGEobj
#'
#' During a workflow, a DGEobj typically gets filtered down to remove samples
#' that fail qc or non-expressed genes.  The resetDGEobj function produces a new DGEobj with
#' the original un-filtered data.
#'
#' We can restore the original data, because a copy of the original data was stored on the "meta"
#' layer and the meta layer is not subject to row or column filters.
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq; counts; low intensity
#'
#' @param DGEobj A DGEobj that we wish to extract original un-filtered data from.
#' @param platformType One of "RNA-Seq", "Affymetrix" or "Somalogic".  Only
#'   required if this attribute is missing from the DGEobj.
#'
#' @return Same class as input object with low intensity rows removed
#'
#' @examples
#'
#'   #get back the original data e.g. to start a different analysis
#'   unfilteredDGEobj <- resetDGEobj(MyHeavilyFilteredDGEobj)
#'
#' @importFrom assertthat assert_that
#' @importFrom stringr str_c
#'
#' @export
resetDGEobj <- function(dgeObj, platformType)
{
  platform.rnaseq <- c("rna-seq", "rnaseq")
  platform.somalogic <- c("somalogic", "soma", "adat")

  assertthat::assert_that(class(dgeObj)[[1]] == "DGEobj",
                          !is.null(attr(dgeObj, "level")))

  if(is.null(attr(dgeObj, "PlatformType")))
    stop ("Required attribute \"PlatformType\" is missing!  Must use platformType argument.")

  # The initXXXobj functions insert data in a particular order such that the
  # unfiltered *_orig components are indexes 1,3,5 In order, these correspond to
  # primary assay (counts, intensities, RMA), design table and
  # gene/isoform/protein annotation.

  metaList <- getBaseType(dgeObj, "meta")[1:3]

  #Use PlatformType attribute to decide which init function to use.
  platformType <- tolower(attr(dgeObj, "PlatformType"))

  #RNA-Seq
  counts <- getItem(dgeObj, "counts_orig")
  #Somalogic
  intensities <- getItem(dgeObj, "intensities_orig")

  design <- getItem(dgeObj, "design_orig")

  if ("geneData_orig" %in% names(dgeObj)) {
    rowData <- getItem(dgeObj, "geneData_orig")
  } else if ("isoformData_orig" %in% names(dgeObj)) {
    rowData <- getItem(dgeObj, "isoformData_orig")
  } else if ("exonData_orig" %in% names(dgeObj)) {
    rowData <- getItem(dgeObj, "exonData_orig")
  } else if ("proteinData_orig" %in% names(dgeObj)){
    rowData <- getItem(dgeObj, "proteinData_orig")
  } else {
    stop("gene/isoform/exon/protein data not found")
  }

  if (platformType %in% platform.rnaseq) {
    newObj <- initDGEobj(counts=dgeObj$counts_orig,
                         rowData = rowData,
                         colData = design,
                         level= attr(dgeObj, "level"),
                         DGEobjDef = attr(dgeObj, "objDef")
    )
  } else if (platformType %in% platform.somalogic) {
      newObj <- initSOMAobj(intensities=intensities,
                         rowData = rowData,
                         colData = design,
                         DGEobjDef = attr(dgeObj, "objDef")
    )
  } else {
    stop("The PlatformType attribute value was not recognized!")
  }

  #transfer effectiveLength_orig if present
  if ("effectiveLength_orig" %in% names(dgeObj)){
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

  # now transfer all but "names" and "class" attributes
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
  attributes.dgeObj <- getAttributes(dgeObj, excludeList=excludeList)
  for (at in names(attributes.dgeObj)) {
      attr(newObj, at) <- attributes.dgeObj[[at]]
  }

  return(newObj)
}



