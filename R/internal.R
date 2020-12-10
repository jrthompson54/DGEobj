.DGEobjDef <- list(
    # There are 4 immutable base type: row, col, assay, meta
    # Each type must be one of these four basetypes
    # Extensibility: Additional types can be added to .DGEobj$type as long as they are
    # assigned to one of the 4 basetypes.

    basetype = c(row = "row",
                 col = "col",
                 assay = "assay",
                 meta = "meta"),

    # The value of type is a basetype.
    # All types must be associated with one of the four basetypes.
    type = c(row   = "row",
             col   = "col",
             assay = "assay",
             meta  = "meta",

             geneData    = "row",
             isoformData = "row",
             exonData    = "row",
             granges     = "row",

             fit          = "row",
             contrast_fit = "row",
             topTable     = "row",

             design       = "col",
             designMatrix = "col",

             counts             = "assay",
             effectiveLength    = "assay",
             Log2CPM            = "assay",
             TPM                = "assay",
             FPKM               = "assay",
             zFPKM              = "assay",
             AffyRMA            = "assay",
             DGEList            = "assay",
             Elist              = "assay",
             isoformFrac        = "assay",
             corFit             = "meta",
             topTreat           = "meta",
             geneList           = "meta",  #intended for short gene lists
             pathway            = "meta",   #should consider format standards for genelists and pathways
             URL                = "meta",
             contrast_fit_treat = "meta",
             contrastMatrix     = "meta",

             #types with _orig suffix are intended to store the initialized data
             #in its original state (i.e. before subsetting)
             geneData_orig        = "meta",
             isoformData_orig     = "meta",
             exonData_orig        = "meta",
             granges_orig         = "meta",
             counts_orig          = "meta",
             design_orig          = "meta",
             effectiveLength_orig = "meta",

             svobj = "meta",

             intensities      = "assay",
             intensities_orig = "meta",
             intensity        = "assay",
             intensity_orig   = "meta",

             AffyRMA_orig = "meta",

             # Proteomics data types
             proteinAnnotation           = "row",
             peptideAnnotation           = "row",
             proteingroupAnnotation      = "row",
             proteinAnnotation_orig      = "meta",
             peptideAnnotation_orig      = "meta",
             proteingroupAnnotation_orig = "meta"
    ),

    # These can only have one instance in a DGEobj
    uniqueType = c("counts",
                   "counts_orig",
                   "design",
                   "design_orig",
                   "geneData",
                   "geneData_orig",
                   "isoformData",
                   "isoformData_orig",
                   "exonData",
                   "exonData_orig",
                   "effectiveLength",
                   "effectiveLength_orig",
                   "granges",
                   "granges_orig",
                   "intensities",
                   "intensities_orig",
                   "AffyRMA",
                   "AffyRMA_orig",
                   "proteinAnnotation",
                   "proteinAnnotation_orig",
                   "intensity",
                   "intentisy_orig",
                   "peptideAnnotation",
                   "peptideAnnotation_orig",
                   "proteingroupAnnotation",
                   "proteingroupAnnotation_orig",
                   "ptmAnnotation",
                   "ptmAnnotation_orig"),

    allowedLevels = c("gene", "isoform", "exon", "proteingroup", "peptide", "ptm", "protein")
)
