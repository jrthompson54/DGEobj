### Function initSOMAobj ###
#' Function initSOMAobj
#'
#' Initializes DGEobj with base data (intensities, gene annotation and sample annotation).
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq
#'
#' @param intensities A intensity matrix in genes (rows) by samples (col) format
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
#' @return A DGEobj object with Somalogic data
#'
#' @examples
#'    # Load and convert Somalogic data
#'    library(readat)
#'    soma_adat <- readAdat(adatfilename)
#'    soma_se <- as.SummarizedExperiment(some_adat)
#'
#'    myDgeObj <- initSOMAobj(intensities = Myintensities,
#'                             rowData = MyGeneAnnotation,
#'                             colData = MyDesign,
#'                             level = "gene",
#'                             customAttr = list (PID = "20171025-0001",
#'                                                XpressID = "12345",
#'                                                Genome = "Mouse.B38",
#'                                                GeneModel = "Ensembl.R84")
#'                            )
#'
#' @import magrittr
#' @importFrom assertthat assert_that
#' @importFrom methods as
#' @export
### # Constructor function for the class
initSOMAobj <- function(intensities, rowData, colData, #required
                       level,   #one of gene, isoform or exon or protein
                       customAttr, #optional list of named Attr/Value pairs
                       DGEobjDef=.DGEobjDef
                       ) {
    level <- "protein"  #only option for SOMA data

    #need to add intensities to DGEobjDef
    assert_that(!missing(intensities),
                !missing(colData),
                !missing(rowData),
                # !missing(level),
                is.matrix(intensities) | is.data.frame(intensities),
                # level %in% DGEobjDef$allowedLevels,
                !is.null(rownames(intensities)),
                !is.null(colnames(intensities)),
                !is.null(rownames(rowData)),
                !is.null(rownames(colData))
                )

    #some reality checking before we build the DGEobj
    #rownames annotation = rownames on intensities
    #   if not and same length, sort to try and align the data
    # similarly rownames in colData must match colnames in intensities
    assert_that(nrow(intensities) == nrow(rowData),
                ncol(intensities) == nrow(colData))

    #if we're here, lengths match up, now check names
    if (!all(rownames(intensities) == rownames(rowData))){
        #sort both by rowname
        intensities <- intensities[order(rownames(intensities)),]
        rowData <- rowData[order(rownames(rowData)),]
    }
    if (!all(colnames(intensities) == rownames(colData))){
        #sort both as appropriate
        intensities <- intensities[,order(colnames(intensities))]
        colData <- colData[order(rownames(colData)),]
    }
    #now everything should be sorted properly check one last time
    assert_that(
        all(rownames(intensities) == rownames(rowData)),
        all(colnames(intensities) == rownames(colData))
        )

    #all our data are properly aligned; build the SOMAobj
    #
    funArgs <- match.call()

    level <- "protein"  #only option for SOMA data

    result <- try(intensities <- as.matrix(intensities), silent=TRUE)
    if (class(result) == "try-error")
        stop("Couldn't coerce intensities to a numeric matrix!")

    #initialize an empty SOMAobj
    somaObj <- list()
    class(somaObj) <- "DGEobj"
    attr(somaObj, "objDef") <- DGEobjDef

    #add empty attributes
    attr(somaObj, "type") <- list()
    attr(somaObj, "basetype") <- list()
    attr(somaObj, "parent") <- list()
    attr(somaObj, "funArgs") <- list()
    attr(somaObj, "dateCreated") <- list()

    #load required items
    #
    #intensities
    somaObj <- addItem(somaObj,
                      item=intensities,
                      itemName="intensities_orig",
                      itemType="intensities_orig",
                      funArgs = funArgs,
                      parent = "",
                      init=TRUE
    )

    somaObj <- addItem(somaObj,
                      item=intensities,
                      itemName="intensities",
                      itemType="intensities",
                      funArgs= funArgs,
                      parent = "intensities_orig",
                      init=TRUE
    )

    #colData
    somaObj <- addItem(somaObj,
                      item=colData,
                      itemName="design_orig",
                      itemType="design_orig",
                      funArgs = funArgs,
                      parent="")

    somaObj <- addItem(somaObj,
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
           "exon" = itemName <- "exonData",
           "protein" = itemName <- "proteinData"
    )
    itemType <- itemName
    parent <- paste(itemName, "_orig", sep="")
    grparent <- itemName

    #Now add the gene/isoform/exon data
    #  _orig version in meta
    somaObj <- addItem(somaObj,
                      item=rowData,
                      itemName=parent,
                      itemType=parent,
                      funArgs=funArgs,
                      parent="")


    somaObj <- addItem(somaObj,
                      item=rowData,
                      itemName=itemName,
                      itemType=itemType,
                      funArgs=funArgs,
                      parent=parent)

    #annotate the level
    somaObj %<>% setAttributes(list(level=level))

    # GRanges not appropriate for protein level data
    #
    # #Build GRanges if chr pos info is present
    # ### depends on chr pos data;  wrap in try
    # result <- try ({gr <- as(rowData, "GRanges")}, silent=TRUE)
    # if (class(result) == "try-error"){
    #     warning("Couldn't build a GRanges object!")
    # } else { #add the GRanges items
    #
    #     somaObj <- addItem(somaObj,
    #                       item=gr,
    #                       itemName="granges_orig",
    #                       itemType="granges_orig",
    #                       funArgs=funArgs,
    #                       parent=paste(grparent, "_orig", sep="")
    #                       )
    #
    #     somaObj <- addItem(somaObj,
    #                       item=gr,
    #                       itemName="granges",
    #                       itemType="granges",
    #                       funArgs=funArgs,
    #                       parent=grparent
    #                       )
    # }

    #add additional DGEobj level attributes
    if (!missing(customAttr))
        somaObj <- setAttributes(somaObj, customAttr)

    return (somaObj)
}




