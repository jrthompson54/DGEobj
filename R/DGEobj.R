
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
# .DGEobjDef <- list(
#     #there are 4 basetypes that fundamentally describe how to subset different items
#     # basetype = c(row="row",
#     #                col="col",
#     #                assay="assay",
#     #                meta="meta"),
#     #the value of type is a basetype  All Types must be associated with one
#     #of the four basetypes.
#     type = c(row="row",
#              col="col",
#              assay ="assay",
#              meta ="meta",
#
#              geneData ="row",
#              isoformData = "row",
#              exonData = "row",
#              proteinData = "row", #for Somalogic
#              granges = "row",
#
#              fit = "row",
#              contrast_fit = "row",
#              topTable = "row",
#
#              design ="col",
#              designMatrix = "col",
#
#              counts ="assay",
#              effectiveLength = "assay",
#              Log2CPM = "assay",
#              TPM = "assay",
#              FPKM = "assay",
#              zFPKM = "assay",
#              AffyRMA = "assay",
#              DGEList = "assay",
#              Elist = "assay",
#              isoformFrac = "assay",
#              corFit = "meta",
#              topTreat ="meta",
#              geneList = "meta",  #intended for short gene lists
#              pathway = "meta",   #should consider format standards for genelists and pathways
#              URL = "meta",
#              contrast_fit_treat = "meta",
#              contrastMatrix = "meta",
#              #types with _orig suffix are intended to store the initialized data
#              #in its original state (i.e. before subsetting)
#              geneData_orig ="meta",
#              isoformData_orig = "meta",
#              exonData_orig = "meta",
#              granges_orig = "meta",
#              counts_orig = "meta",
#              design_orig ="meta",
#              effectiveLength_orig = "meta",
#              svobj = "meta",
#              intensities = "assay", #for somalogic data
#              intensities_orig = "meta",
#              proteinData = "assay",
#              proteinData_orig = "meta",
#              AffyRMA_orig = "meta"
#     ),
#
# # These Types can only have one instance in a DGEobj
#     uniqueType = c("counts",
#                    "counts_orig",
#                    "design",
#                    "design_orig",
#                    "geneData",
#                    "geneData_orig",
#                    "isoformData",
#                    "isoformData_orig",
#                    "exonData",
#                    "exonData_orig",
#                    "effectiveLength",
#                    "effectiveLength_orig",
#                    # "DGEList",
#                    "granges",
#                    "granges_orig",
#                    "intensities",
#                    "intensities_orig",
#                    "AffyRMA",
#                    "AffyRMA_orig",
#                    "proteinData",
#                    "proteinData_orig"),
#
#     allowedLevels = c("gene", "isoform", "exon", "protein")
# ) #.DGEobjDef
# # Uncomment this block when you need to update the ./data/DGEobj.rda file
# x = getwd()
# setwd ("~/R/lib/pkgsrc/DGEobj/")
# save(.DGEobjDef, file="./data/DGEobj.rda")
# setwd(x)

    #
    #Define allowed values for attributes of each data type to be attached to the data item.

    ##experimental:  here's the fine line between capturing accurate metadata and overburdening
    ##the end user to provide information.  If we only allow data entry in define fields,
    ##we'll likely not meet everyone's needs.  But if we don't enforce a vocabulary, caos reigns.  So
    ##Let's try to define the obvious attributes of different types and provide a mechanism
    ##to add new attributes to the definition.  Also make attribute names case insensitive.
    ##
    ##Define addAttributes to take a named list of attributes and add them with one line.
    # typeAttributes = c(
    #      row = c("SeqId", "Source", "Version"), #name of geneid col, source e.g. Ensembl, version e.g. R84
    #      col = "SampIdCol",  #name of sampleID column
    #      assay = c("normalized", "LowIntFilter"),
    #      meta ="",
    #
    #      geneData = c("SeqId", "Source", "Version"),
    #      isoformData = c("SeqId", "Source", "Version"),
    #      exonData = c("SeqId", "Source", "Version"),
    #      topTable = "",
    #      fit = "",
    #      DGEList = c("normalization", "LowIntFilter"),
    #
    #      design ="SampIdCol",
    #      designMatrix = "SampIdCol",
    #
    #      counts =c("normalization", "LowIntFilter"),
    #
    #      Log2CPM = c("normalized", "LowIntFilter"),
    #      TPM = c("normalized", "LowIntFilter"),
    #      FPKM = c("normalized", "LowIntFilter"),
    #      zFPKM = c("normalized", "LowIntFilter"),
    #      AffyRMA = c("normalized", "LowIntFilter"),
    #
    #      topTreat ="",
    #      geneList = "",
    #      pathway = "",
    #      URL = ""
    #      ),
    #
    # mainAttributes = c(
    #             Platform = c("RNA-Seq", "EdgeSeq", "DNA-Seq",
    #                          "Nanostring", "ddPCR", "TaqMan", "Affy"),
    #             Instrument = c("NextSeq", "HiSeq", "PacBio", "Nanopore", "IonTorrent"),
    #             Vendor = c("BMS", "Rutgers", "CHOP", "EA"),
    #             readType = c("PE", "SE"),
    #             readLength = "",
    #             strandSpecific = c(TRUE, FALSE),
    #             AffyChip = ""
    #             )




##  To Do
##
##      as.RSE
##      as.ES
##
##

#Notcurrently used:  I'm using as(rowData, "GRanges") to cast rowData as a
#GRanges object.  This  works with Omicsoft data.  Haven't tested Xpress data yet.
#
# ### Function df2GR ###@import magrittr
# @importFrom stats na.omit
# @importFrom IRanges IRanges
# @importFrom GenomicRanges GRanges mcols
# df2GR <- function(df, seqnames=c("seqnames", "chr", "chromosome"),
#                   start="start", end="end", strand="strand",
#                   start.offset=1, end.offset=start.offset) {
#     #Convert a Annotation DF to a genomic Ranges object
#     #Optional parameters for seqnames, start, end, strand anticipate possible you might have for these
#     #fields in your annotation file.  Only need to modify these if your annotation uses differenc colnames
#     #for these fields
#     #
#     #These lines return the colnames used in your datafile.
#     seqnames.col <- base::match(seqnames, tolower(colnames(df))) %>% an.omit() %>% .[1]
#     start.col <- base::match(start, tolower(colnames(df))) %>% an.omit() %>% .[1]
#     end.col <-base:: match(end, tolower(colnames(df))) %>% an.omit() %>% .[1]
#     strand.col <- base::match(strand, tolower(colnames(df))) %>% an.omit() %>% .[1]
#     other.cols <- base::setdiff(seq_along(colnames(df)), c(seqnames.col, start.col, end.col, strand.col))
#
#     #make sure start and end are numeric; if not, remove commas and convert to numeric
#     if (is.character(df[[start.col]])) {
#         df[[start.col]] = gsub(",", "", df[[start.col]]) %>% as.numeric
#     }
#     if (is.character(df[[end.col]])) {
#         df[[end.col]] = gsub(",", "", df[[end.col]]) %>% as.numeric
#     }
#
#     MyRanges = IRanges::IRanges(start=df[[start.col]] - start.offset + 1,
#                                 end=df[[end.col]]) - end.offset + 1
#
#     gr <- GenomicRanges::GRanges(seqnames=df[[seqnames.col]],
#                                  ranges=MyRanges,
#                                  strand=df[[strand.col]])
#
#     GenomicRanges::mcols(gr) <- df[other.cols]
#     names(gr) <- rownames(df)
#     return(gr)
# }

### Function Txt2DF ###
#' @importFrom utils read.table
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

### Function tsmsg ###
# a timestamped message
tsmsg <- function(...) {
    # Works like message() but prepends a timestamp
    message(date(), ": ", ...)
}






