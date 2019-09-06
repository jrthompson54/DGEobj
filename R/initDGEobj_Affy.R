### Function initDGEobj_Affy ###
#' Function initDGEobj_Affy
#'
#' Initializes DGEobj with base data (intensities, gene annotation and sample annotation).
#' This function is NOT typically used directly by end users.  See rXpress::Xpress2DGEO_Affy which
#' reads data from Xpress and calls this function to build a DGEobj.
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq
#'
#' @param AffyRMA An RMA intensity matrix in genes (rows) by samples (col) format
#'   with row and column names.
#' @param rowData  Protein level annotation. rownames must match
#'    rownames in intensities matrix.
#' @param colData A dataframe describing the experiment design.  Rownames much match
#'  colnames(intensities).
#' @param customAttr An optional (but highly recommended) named list of attributes
#'     to assign to the DGEobj.
#' @param DGEobjDef An object definition. Defaults to the global DGE object definition
#'     (.DGEobjDef) and you usually shouldn't change this unless you're customizing
#'     the object for new data types.
#'
#' @return A DGEobj object with RMA data
#'
#' @examples
#'
#'    myDgeObj <- initDGEobj_Affy(intensities = Myintensities,
#'                             rowData = MyGeneAnnotation,
#'                             colData = MyDesign,
#'                             level = "HG-U133_PLUS_2",
#'                             customAttr = list (PID = "20171025-0001",
#'                                                XpressID = "12345")
#'                            )
#'
#' @import magrittr
#' @importFrom assertthat assert_that
#' @export
# Initialize a DGEobj from AffyRMA data rather than RNAseq count data
initDGEobj_Affy <- function(AffyRMA, rowData, colData, #required
                            level,   #one of gene, isoform or exon
                            BrainArrayVer,  #the BrainArray version number for the alternate CDFs used
                            customAttr, #optional list of named Attr/Value pairs
                            DGEobjDef=.DGEobjDef
                            ) {
    assert_that(!missing(AffyRMA),
                !missing(colData),
                !missing(rowData),
                !missing(level),
                is.matrix(AffyRMA) | is.data.frame(AffyRMA),
                level %in% DGEobjDef$allowedLevels,
                !is.null(rownames(AffyRMA)),
                !is.null(colnames(AffyRMA)),
                !is.null(rownames(rowData)),
                !is.null(rownames(colData))
                )

    #some reality checking before we build the DGEobj
    #rownames annotation = rownames on AffyRMA
    #   if not and same length, sort to try and align the data
    # similarly rownames in colData must match colnames in AffyRMA
    assert_that(nrow(AffyRMA) == nrow(rowData),
                ncol(AffyRMA) == nrow(colData))

    #if we're here, lengths match up, now check names
    if (!all(rownames(AffyRMA) == rownames(rowData))){
        #sort both by rowname
        AffyRMA <- AffyRMA[order(rownames(AffyRMA)),]
        rowData <- rowData[order(rownames(rowData)),]
    }
    if (!all(colnames(AffyRMA) == rownames(colData))){
        #sort both as appropriate
        AffyRMA <- AffyRMA[,order(colnames(AffyRMA))]
        colData <- colData[order(rownames(colData)),]
    }
    #now everything should be sorted properly check one last time
    assert_that(
        all(rownames(AffyRMA) == rownames(rowData)),
        all(colnames(AffyRMA) == rownames(colData))
        )

    #all our data are properly aligned; build the DGEobj
    #
    funArgs <- match.call()

    result <- try(AffyRMA <- as.matrix(AffyRMA), silent=TRUE)
    if (class(result) == "try-error")
        stop("Couldn't coerce AffyRMA to a numeric matrix!")

    #initialize an empty DGEobj
    dgeObj <- list()
    class(dgeObj) <- "DGEobj"
    attr(dgeObj, "objDef") <- DGEobjDef

    #add empty attributes
    attr(dgeObj, "type") <- list()
    attr(dgeObj, "basetype") <- list()
    attr(dgeObj, "parent") <- list()
    attr(dgeObj, "funArgs") <- list()
    attr(dgeObj, "dateCreated") <- list()

    #load required items
    #
    #AffyRMA
    dgeObj <- newType(dgeObj,
                      itemType = "AffyRMA_orig",
                      baseType = "meta",
                      uniqueItem = TRUE
    )

    dgeObj <- addItem(dgeObj,
                      item = AffyRMA,
                      itemName = "AffyRMA_orig",
                      itemType = "AffyRMA_orig",
                      funArgs = funArgs,
                      parent = "",
                      init = TRUE
    )

    dgeObj <- addItem(dgeObj,
                      item = AffyRMA,
                      itemName = "AffyRMA",
                      itemType = "AffyRMA",
                      funArgs = funArgs,
                      parent = "AffyRMA_orig",
                      init = TRUE
    )

    #colData
    dgeObj <- addItem(dgeObj,
                      item=colData,
                      itemName="design_orig",
                      itemType="design_orig",
                      funArgs = funArgs,
                      parent="")

    dgeObj <- addItem(dgeObj,
                      item=colData,
                      itemName="design",
                      itemType="design",
                      funArgs = funArgs,
                      parent="design_orig")

    #rowData
    level <- tolower(level)
    switch(level,
           "gene" = itemName <- "geneData",
           "isoform" = itemName <- "isoformData",
           "exon" = itemName <- "exonData"
    )
    itemType <- itemName
    parent <- paste(itemName, "_orig", sep="")
    grparent <- itemName

    #Now add the gene/isoform/exon data
    #  _orig version in meta
    dgeObj <- addItem(dgeObj,
                      item=rowData,
                      itemName=parent,
                      itemType=parent,
                      funArgs=funArgs,
                      parent="")


    dgeObj <- addItem(dgeObj,
                      item=rowData,
                      itemName=itemName,
                      itemType=itemType,
                      funArgs=funArgs,
                      parent=parent)

    #annotate the level
    dgeObj %<>% setAttributes(list(level=level))

    # #Build GRanges if chr pos info is present
    # ### depends on chr pos data;  wrap in try
    # result <- try ({gr <- as(rowData, "GRanges")}, silent=TRUE)
    # if (class(result) == "try-error"){
    #     warning("Couldn't build a GRanges object!")
    # } else { #add the GRanges items
    #
    #     dgeObj <- addItem(dgeObj,
    #                       item=gr,
    #                       itemName="granges_orig",
    #                       itemType="granges_orig",
    #                       funArgs=funArgs,
    #                       parent=paste(grparent, "_orig", sep="")
    #                       )
    #
    #     dgeObj <- addItem(dgeObj,
    #                       item=gr,
    #                       itemName="granges",
    #                       itemType="granges",
    #                       funArgs=funArgs,
    #                       parent=grparent
    #                       )
    # }

    #add additional DGEobj level attributes
    if (!missing(customAttr))
      dgeObj <- setAttributes(dgeObj, customAttr)

    #add BrainArray Version number as a DGEobj level attribute
    if (!missing(BrainArrayVer))
      dgeObj <- setAttributes(dgeObj, list(BrainArrayVer = BrainArrayVer))

    return (dgeObj)
}
