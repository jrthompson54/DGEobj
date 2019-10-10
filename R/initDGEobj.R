### Function initDGEobj ###
#' Function initDGEobj
#'
#' Initializes DGEobj with base data (counts, gene annotation and sample annotation).
#'
#' @author John Thompson, \email{john.thompson@@bms.com}
#' @keywords RNA-Seq
#'
#' @param counts A count matrix or dataframe with row and colnames.
#' @param rowData  Gene, isoform or exon level annotation. rownames must match
#'    rownames in count matrix.
#' @param colData A dataframe describing the experiment design.  Rownames much match
#'  colnames(counts).
#' @param level One of "gene", "isoform" or "exon"
#' @param customAttr An optional (but highly recommended) named list of attributes
#'     to assign to the DGEobj.
#' @param allowShortSampleIDs  Using sequential integers rownames (even if typed as character)
#'   is discouraged and by default will abort the DGEobj creation.  If you have a legitimate
#'   need to have short samplenames composed of numeric characters, you can set this argument to TRUE.
#'   (default = FALSE)
#' @param DGEobjDef An object definition. Defaults to the global DGE object definition
#'     (.DGEobjDef) and you usually shouldn't change this if you're running RNA-seq.
#' @return A DGEobj object
#'
#' @examples
#'    # initialize a DGEobj
#'    myDgeObj <- initDGEobj(counts = MyCounts,
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
initDGEobj <- function(counts, rowData, colData, #required
                       level,   #one of gene, isoform or exon
                       customAttr, #optional list of named Attr/Value pairs
                       allowShortSampleIDs = FALSE,
                       DGEobjDef=.DGEobjDef
) {
    ### # Constructor function for the class
    assert_that(!missing(counts),
                !missing(colData),
                !missing(rowData),
                !missing(level),
                is.matrix(counts) | is.data.frame(counts),
                level %in% DGEobjDef$allowedLevels,
                !is.null(rownames(counts)),
                !is.null(colnames(counts)),
                !is.null(rownames(rowData)),
                !is.null(rownames(colData))
    )

    #some reality checking before we build the DGEobj
    #rownames annotation = rownames on counts
    #   if not and same length, sort to try and align the data
    # similarly rownames in colData must match colnames in counts
    assert_that(nrow(counts) == nrow(rowData),
                ncol(counts) == nrow(colData))

    #if we're here, lengths match up, now check names
    if (!all(rownames(counts) == rownames(rowData))){
        #sort both by rowname
        counts <- counts[order(rownames(counts)),]
        rowData <- rowData[order(rownames(rowData)),]
    }
    if (!all(colnames(counts) == rownames(colData))){
        #sort both as appropriate
        counts <- counts[,order(colnames(counts))]
        colData <- colData[order(rownames(colData)),]
    }
    #now everything should be sorted properly check one last time
    assert_that(
        all(rownames(counts) == rownames(rowData)),
        all(colnames(counts) == rownames(colData))
    )

    #JRT oct2019: now trap for sequential numeric sample names (assay colnames and  design rownames)
    # disguised as type charater
    #
    # coerce the sample names to to numeric.  If succesfull, abort the process.
    # further qualify by character length; i.e. if <= 999 samples, force max(nchar) >3.  This will
    # allow long numeric sample IDs.

    #Do rownames convert all to numeric
    if (!allowShortSampleIDs == TRUE){ #override/skip the test
        suppressWarnings(
            test <- as.numeric(rownames(colData))
        )
        #If numeric conversion was successful on all rows, test further for length of labels
        if (all(is.na(test)) == FALSE) {

            sampleCount <- nrow(colData)
            minchar <- nchar(as.character(sampleCount))  #some samples must have nchar greater than this, or abort
            maxchar <- max(sapply(rownames(colData), nchar)) #max length of sampleID
            assert_that (maxchar > minchar,
                         msg = str_c("It looks like you have numeric sample IDs (design rownames).",
                                     "Please supply a more specific sample identifier. ",
                                     "Use allowShortSampleIDs = TRUE to explicitily override this restriction",
                                     sep="\n")
            )
        }
    }


    #all our data are properly aligned; build the DGEobj
    #
    funArgs <- match.call()

    result <- try(counts <- as.matrix(counts), silent=TRUE)
    if (class(result) == "try-error")
        stop("Couldn't coerce counts to a numeric matrix!")

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
    #Counts
    dgeObj <- addItem(dgeObj,
                      item=counts,
                      itemName="counts_orig",
                      itemType="counts_orig",
                      funArgs = funArgs,
                      parent = "",
                      init=TRUE
    )

    dgeObj <- addItem(dgeObj,
                      item=counts,
                      itemName="counts",
                      itemType="counts",
                      funArgs= funArgs,
                      parent = "counts_orig",
                      init=TRUE
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

    #Build GRanges if chr pos info is present
    ### depends on chr pos data;  wrap in try
    result <- try ({gr <- as(rowData, "GRanges")}, silent=TRUE)
    if (class(result) == "try-error"){
        warning("Couldn't build a GRanges object!")
    } else { #add the GRanges items

        dgeObj <- addItem(dgeObj,
                          item=gr,
                          itemName="granges_orig",
                          itemType="granges_orig",
                          funArgs=funArgs,
                          parent=paste(grparent, "_orig", sep="")
        )

        dgeObj <- addItem(dgeObj,
                          item=gr,
                          itemName="granges",
                          itemType="granges",
                          funArgs=funArgs,
                          parent=grparent
        )
    }

    #add additional DGEobj level attributes
    if (!missing(customAttr))
        dgeObj <- setAttributes(dgeObj, customAttr)


    return (dgeObj)
}
