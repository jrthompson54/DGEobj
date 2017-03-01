
### DGEobj object definition

### Define an extensible datatype
### There are 4 immutable base type: row, col, assay, meta
### Each type must be one of these four basetypes
### Extensibility: Additional types can be added to .DGEobj$type as long as they are
### assigned to one of the 4 basetypes.
###
### Data Definitions
#
# The .DGEobjDef describes the structure of the DGE object.
.DGEobjDef <- list(
    #there are 4 basetypes that fundamentally describe how to subset different items
    basetype = c(row="row",
                   col="col",
                   assay="assay",
                   meta="meta"),
    #the value of type is a basetype  All Types must be associated with one
    #of the four basetypes.
    type = c(row="row",
             col="col",
             assay ="assay",
             meta ="meta",

             geneData ="row",
             isoformData = "row",
             exonData = "row",
             granges = "row",

             fit = "row",
             contrast_fit = "row",
             topTable = "row",

             design ="col",
             designMatrix = "col",

             counts ="assay",
             effectiveLength = "assay",
             Log2CPM = "assay",
             TPM = "assay",
             FPKM = "assay",
             zFPKM = "assay",
             AffyRMA = "assay",
             DGEList = "assay",
             Elist = "assay",

             corFit = "meta",
             topTreat ="meta",
             geneList = "meta",  #intended for short gene lists
             pathway = "meta",   #should consider format standards for genelists and pathways
             URL = "meta",
             contrast_fit_treat = "meta",
             contrastMatrix = "meta",
             #types with _orig suffix are intended to store the initialized data
             #in its original state (i.e. before subsetting)
             geneData_orig ="meta",
             isoformData_orig = "meta",
             exonData_orig = "meta",
             granges_orig = "meta",
             counts_orig = "meta",
             design_orig ="meta",
             effectiveLength_orig = "meta",
             svobj = "meta"
             ),

# These Types can only have one instance in a DGEobj
    uniqueType = c("counts",
                "design",
                "geneData",
                "isoformData",
                "exonData",
                "effectiveLength",
                "DGEList",
                "granges"),

    allowedLevels = c("gene", "isoform", "exon"),

    #
    #Define allowed values for attributes of each data type to be attached to the data item.

    ##experimental:  here's the fine line between capturing accurate metadata and overburdening
    ##the end user to provide information.  If we only allow data entry in define fields,
    ##we'll likely not meet everyone's needs.  But if we don't enforce a vocabulary, caos reigns.  So
    ##Let's try to define the obvious attributes of different types and provide a mechanism
    ##to add new attributes to the definition.  Also make attribute names case insensitive.
    ##
    ##Define addAttributes to take a named list of attributes and add them with one line.
    typeAttributes = c(
         row = c("SeqId", "Source", "Version"), #name of geneid col, source e.g. Ensembl, version e.g. R84
         col = "SampIdCol",  #name of sampleID column
         assay = c("normalized", "LowIntFilter"),
         meta ="",

         geneData = c("SeqId", "Source", "Version"),
         isoformData = c("SeqId", "Source", "Version"),
         exonData = c("SeqId", "Source", "Version"),
         topTable = "",
         fit = "",
         DGEList = c("normalization", "LowIntFilter"),

         design ="SampIdCol",
         designMatrix = "SampIdCol",

         counts =c("normalization", "LowIntFilter"),

         Log2CPM = c("normalized", "LowIntFilter"),
         TPM = c("normalized", "LowIntFilter"),
         FPKM = c("normalized", "LowIntFilter"),
         zFPKM = c("normalized", "LowIntFilter"),
         AffyRMA = c("normalized", "LowIntFilter"),

         topTreat ="",
         geneList = "",
         pathway = "",
         URL = ""
         ),

    mainAttributes = c(
                Platform = c("RNA-Seq", "EdgeSeq", "DNA-Seq",
                             "Nanostring", "ddPCR", "TaqMan", "Affy"),
                Instrument = c("NextSeq", "HiSeq", "PacBio", "Nanopore", "IonTorrent"),
                Vendor = c("BMS", "Rutgers", "CHOP", "EA"),
                readType = c("PE", "SE"),
                readLength = "",
                strandSpecific = c(TRUE, FALSE),
                AffyChip = ""
                )
) #.DGEobjDef

# Uncomment this block when you need to update the ./data/DGEobj.rda file
# x = getwd()
# setwd ("~/R/lib/pkgsrc/DGEobj/")
# save(.DGEobjDef, file="./data/DGEobj.rda")
# setwd(x)


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
#' @import assertthat magrittr
#'
#' @export
### # Constructor function for the class
initDGEobj <- function(counts, rowData, colData, #required
                       level,   #one of gene, isoform or exon
                       customAttr, #optional list of named Attr/Value pairs
                       DGEobjDef=.DGEobjDef
                       ) {
    assert_that(!missing(counts),
                !missing(colData),
                !missing(rowData),
                !missing(level),
                is.matrix(counts) | is.data.frame(counts),
                level %in% DGEobjDef$allowedLevels,
                !is.null(rownames(counts)),
                !is.null(colnames(counts)),
                !is.null(rownames(rowData))
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
                      parent = "")

    dgeObj <- addItem(dgeObj,
                      item=counts,
                      itemName="counts",
                      itemType="counts",
                      funArgs= funArgs,
                      parent = "counts_orig"
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


##  To Do
##
##      as.RSE
##      as.ES
##
##

#Notcurrently used:  I'm using as(rowData, "GRanges") to cast rowData as a
#GRanges object.  This  works with Omicsoft data.  Haven't tested Xpress data yet.
#
### Function df2GR ###
#' @import magrittr IRanges GenomicRanges
df2GR <- function(df, seqnames=c("seqnames", "chr", "chromosome"),
                  start="start", end="end", strand="strand",
                  start.offset=1, end.offset=start.offset) {
    #Convert a Annotation DF to a genomic Ranges object
    #Optional parameters for seqnames, start, end, strand anticipate possible you might have for these
    #fields in your annotation file.  Only need to modify these if your annotation uses differenc colnames
    #for these fields
    #
    #These lines return the colnames used in your datafile.
    seqnames.col <- match(seqnames, tolower(colnames(df))) %>% na.omit %>% .[1]
    start.col <- match(start, tolower(colnames(df))) %>% na.omit %>% .[1]
    end.col <- match(end, tolower(colnames(df))) %>% na.omit %>% .[1]
    strand.col <- match(strand, tolower(colnames(df))) %>% na.omit %>% .[1]
    other.cols <- setdiff(seq_along(colnames(df)), c(seqnames.col, start.col, end.col, strand.col))

    #make sure start and end are numeric; if not, remove commas and convert to numeric
    if (is.character(df[[start.col]])) {
        df[[start.col]] = gsub(",", "", df[[start.col]]) %>% as.numeric
    }
    if (is.character(df[[end.col]])) {
        df[[end.col]] = gsub(",", "", df[[end.col]]) %>% as.numeric
    }

    MyRanges = IRanges::IRanges(start=df[[start.col]] - start.offset + 1,
                                end=df[[end.col]]) - end.offset + 1

    gr <- GenomicRanges::GRanges(seqnames=df[[seqnames.col]],
                                 ranges=MyRanges,
                                 strand=df[[strand.col]])

    GenomicRanges::mcols(gr) <- df[other.cols]
    names(gr) <- rownames(df)
    return(gr)
}

### Function Txt2DF ###
Txt2DF <- function(filename) {
    #configured to read Omicsoft .txt files correctly capturing GeneIDs as rownames
    if (file.exists(filename)) {
        df = read.table (filename, sep="\t", stringsAsFactors = FALSE,
                         header=TRUE, row.names = 1, comment.char="",
                         quote="", na.strings=c("NA", "."))
        return (df)
    } else {
        warning (paste ("Warning: File = ", filename, "not found."))
        return (-1)
    }
}


