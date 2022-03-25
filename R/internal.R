.DGEobjDef <- list(
    # There are 4 immutable basetypes: row, col, assay, meta
    # Each type must be one of these four basetypes
    # Extensibility: Additional types can be added to .DGEobj$type as long as they are
    # assigned to one of the 4 basetypes.

    basetype = c(row   = "row",
                 col   = "col",
                 assay = "assay",
                 meta  = "meta"),

    # The value of type is a basetype.
    # All types must be associated with one of the four basetypes.
    type = c(row   = "row",
             col   = "col",
             assay = "assay",
             meta  = "meta",

             geneData    = "row",
             exonData    = "row",
             isoformData = "row",
             proteinData = "row",
             affyData    = "row",
             granges     = "row",

             fit          = "row",
             contrast_fit = "row",
             topTable     = "row",

             design       = "col",
             designMatrix = "col",
             alignQC      = "col",

             counts             = "assay",
             intensities        = "assay",
             effectiveLength    = "assay",
             AffyRMA            = "assay",
             DGEList            = "assay",
             Elist              = "assay",
             isoformFrac        = "assay",

             corFit             = "meta",
             topTreat           = "meta",
             geneList           = "meta",  #intended for short gene lists
             pathway            = "meta",  #should consider format standards for genelists and pathways
             URL                = "meta",
             contrast_fit_treat = "meta",
             contrastMatrix     = "meta",
             imputationMatrix   = "meta",

             #types with _orig suffix are intended to store the initialized data
             #in its original state (i.e. before subsetting, normalizing)
             counts_orig          = "meta",
             intensities_orig     = "meta",
             effectiveLength_orig = "meta",
             geneData_orig        = "meta",
             exonData_orig        = "meta",
             isoformData_orig     = "meta",
             proteinData_orig     = "meta",
             granges_orig         = "meta",
             design_orig          = "meta",
             AffyRMA_orig         = "meta",
             svobj                = "meta"
    ),

    # These can only have one instance in a DGEobj
    uniqueType = c("counts",
                   "counts_orig",
                   "design",
                   "design_orig",
                   "intensities",
                   "intensities_orig",
                   "geneData",
                   "geneData_orig",
                   "exonData",
                   "exonData_orig",
                   "isoformData",
                   "isoformData_orig",
                   "proteinDaa",
                   "proteinData_orig",
                   "effectiveLength",
                   "effectiveLength_orig",
                   "granges",
                   "granges_orig",
                   "AffyRMA",
                   "AffyRMA_orig"
    ),

    allowedLevels = c("gene", "exon", "isoform", "protein", "affy"),

    primaryAssayNames = c(gene    = "counts",
                          exon    = "counts",
                          isoform = "intensities",
                          protein = "intensities",
                          affy    = "AffyRMA")
)

class(.DGEobjDef) <- "DGEobjDef"
