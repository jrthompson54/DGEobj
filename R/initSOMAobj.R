### Function initSOMAobj ###
#' Function initSOMAobj
#'
#' Initializes DGEobj with base data (intensities, gene annotation and sample annotation).
#' This function is NOT typically used directly by end users.  See adatToDGEobj which
#' reads a Somalogic adat file and calls this function to build a DGEobj.
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq
#'
#' @param intensity A intensity matrix in genes (rows) by samples (col) format
#'   with row and column names.
#' @param rowData  Protein level annotation. rownames must match
#'    rownames in intensity matrix.
#' @param colData A dataframe describing the experiment design.  Rownames much match
#'  colnames(intensity).
#' @param customAttr An optional (but highly recommended) named list of attributes
#'     to assign to the DGEobj.
#' @param DGEobjDef An object definition. Defaults to the global DGE object definition
#'     (.DGEobjDef) and you usually shouldn't change this unless you're customizing
#'     the object for new data types.
#'
#' @return A DGEobj object with Somalogic data
#'
#' @examples
#'
#'    myDgeObj <- initSOMAobj(intensity = Myintensity,
#'                             rowData = MyGeneAnnotation,
#'                             colData = MyDesign,
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
initSOMAobj <- function(intensity, rowData, colData, #required
                       customAttr, #optional list of named Attr/Value pairs
                       DGEobjDef=.DGEobjDef
                       ) {
    level <- "protein"  #only option for SOMA data

    #need to add intensity to DGEobjDef
    assert_that(!missing(intensity),
                !missing(colData),
                !missing(rowData),
                # !missing(level),
                is.matrix(intensity) | is.data.frame(intensity),
                # level %in% DGEobjDef$allowedLevels,
                !is.null(rownames(intensity)),
                !is.null(colnames(intensity)),
                !is.null(rownames(rowData)),
                !is.null(rownames(colData))
                )

    #some reality checking before we build the DGEobj
    #rownames annotation = rownames on intensity
    #   if not and same length, sort to try and align the data
    # similarly rownames in colData must match colnames in intensity
    assert_that(nrow(intensity) == nrow(rowData),
                ncol(intensity) == nrow(colData))

    #if we're here, lengths match up, now check names
    if (!all(rownames(intensity) == rownames(rowData))){
        #sort both by rowname
        intensity <- intensity[order(rownames(intensity)),]
        rowData <- rowData[order(rownames(rowData)),]
    }
    if (!all(colnames(intensity) == rownames(colData))){
        #sort both as appropriate
        intensity <- intensity[,order(colnames(intensity))]
        colData <- colData[order(rownames(colData)),]
    }
    #now everything should be sorted properly check one last time
    assert_that(
        all(rownames(intensity) == rownames(rowData)),
        all(colnames(intensity) == rownames(colData))
        )

    #all our data are properly aligned; build the SOMAobj
    #
    funArgs <- match.call()

    result <- try(intensity <- as.matrix(intensity), silent=TRUE)
    if (class(result) == "try-error")
        stop("Couldn't coerce intensity to a numeric matrix!")

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
    #intensity
    somaObj <- addItem(somaObj,
                      item=intensity,
                      itemName="intensity_orig",
                      itemType="intensity_orig",
                      funArgs = funArgs,
                      parent = "",
                      init=TRUE
    )

    somaObj <- addItem(somaObj,
                      item=intensity,
                      itemName="intensity",
                      itemType="intensity",
                      funArgs= funArgs,
                      parent = "intensity_orig",
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
           "protein" = itemName <- "proteinAnnotation"
    )
    itemType <- itemName
    itemName_Parent <- paste(itemName, "_orig", sep="")
    # grparent <- itemName

    #Now add the gene/isoform/exon data
    #  _orig version in meta
    somaObj <- addItem(somaObj,
                      item=rowData,
                      itemName=itemName_Parent,
                      itemType=itemName_Parent,
                      funArgs=funArgs,
                      parent="")


    somaObj <- addItem(somaObj,
                      item=rowData,
                      itemName=itemName,
                      itemType=itemType,
                      funArgs=funArgs,
                      parent=itemName_Parent)

    #annotate the level
    somaObj %<>% setAttributes(list(level=level))

    #add additional DGEobj level attributes
    if (!missing(customAttr))
        somaObj <- setAttributes(somaObj, customAttr)

    return (somaObj)
}




