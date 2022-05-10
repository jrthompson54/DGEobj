context("init.R functions")
skip_if(setup_failed)

test_that('init.R: initDGEobj()', {

    # collect data from test object to initialize new DGEobj
    counts     <- getItem(t_obj, "counts_orig")
    rowData    <- getItem(t_obj, "geneData_orig")
    colData    <- getItem(t_obj, "design_orig")
    level      <- "gene"
    customAttr <- list(Genome    = "Mouse.B38",
                       GeneModel = "Ensembl.R84")
    # create data frame with different row names
    rowData1   <- rowData
    colData1   <- colData
    rownames(rowData1) <- paste0("new_", rownames(rowData))
    rownames(colData1) <- paste0("new_", rownames(colData))

    # checking mismatch rownames
    expect_error({initDGEobj(primaryAssayData = counts,
                             rowData          = rowData1,
                             colData          = colData,
                             level            = level,
                             customAttr       = customAttr)},
                 regexp = "The rownames for primaryAssayData must match the rownames of rowData. Similarly, the colnames of primaryAssayData must match the rownames of colData.")

    expect_error({initDGEobj(primaryAssayData = counts,
                             rowData          = rowData,
                             colData          = colData1,
                             level            = level,
                             customAttr       = customAttr)},
                 regexp = "The rownames for primaryAssayData must match the rownames of rowData. Similarly, the colnames of primaryAssayData must match the rownames of colData.")

    # checking for numeric sampleIds
    counts1 <- counts
    rownames(colData1) <- as.character(1:nrow(colData1))
    colnames(counts1)  <- as.character(1:nrow(colData1))

    expect_error({initDGEobj(primaryAssayData = counts1,
                             rowData          = rowData,
                             colData          = colData1,
                             level            = level,
                             customAttr       = customAttr)},
                 regexp = paste0("It looks like you have numeric sample IDs (design rownames).",
                                 "\nPlease supply a more specific sample identifier.",
                                 " \nUse allowShortSampleIDs = TRUE to explicitly override this restriction"),
                 fixed  = TRUE)

    # verifying class
    expect_s3_class(t_obj, "DGEobj")
    expect_type(attributes(t_obj), "list")

    # checking names and dimensions
    expect_setequal(names(t_obj), c("counts_orig", "counts", "design_orig", "design", "geneData_orig", "geneData", "granges_orig", "granges", "DGEList",
                                    "ReplicateGroupDesign", "ReplicateGroupDesign_Elist", "ReplicateGroupDesign_fit", "ReplicateGroupDesign_fit_cm", "ReplicateGroupDesign_fit_cf",
                                    "BDL_vs_Sham", "EXT1024_vs_BDL", "Nint_vs_BDL", "Sora_vs_BDL"))
    expect_equal(dim(t_obj), t_dim)

    # verifying missing value errors
    expect_error(initDGEobj(rowData =  rowData, colData =  colData, level =  level, customAttr = customAttr),
                 regexp = "Specify the primaryAssayData, colData, rowData, and level. All are required to initialize a DGEobj.")
    expect_error(initDGEobj(primaryAssayData = counts, colData =  colData, level =  level, customAttr = customAttr),
                 regexp = "Specify the primaryAssayData, colData, rowData, and level. All are required to initialize a DGEobj.")
    expect_error(initDGEobj(primaryAssayData = counts, rowData =  rowData, level =  level, customAttr = customAttr),
                 regexp = "Specify the primaryAssayData, colData, rowData, and level. All are required to initialize a DGEobj.")
    expect_error(initDGEobj(primaryAssayData = counts, rowData =  rowData, colData =  colData, customAttr = customAttr),
                 regexp = "Specify the primaryAssayData, colData, rowData, and level. All are required to initialize a DGEobj.")
})

test_that('init.R: levels - asserts', {

    # collect data from test object to initialize new DGEobj
    counts     <- getItem(t_obj, "counts_orig")
    rowData    <- getItem(t_obj, "geneData_orig")
    colData    <- getItem(t_obj, "design_orig")
    level      <- "not level"

    # checking not allowed level
    expect_error({initDGEobj(primaryAssayData = counts,
                             rowData          = rowData,
                             colData          = colData,
                             level            = level)},
                 regexp = "The specified level must be one of:  gene, exon, isoform, protein, affy")

    # checking level class
    level <- 123
    expect_error({initDGEobj(primaryAssayData = counts,
                             rowData          = rowData,
                             colData          = colData,
                             level            = level)},
                 regexp = "The specified level must be one of:  gene, exon, isoform, protein, affy")

    # checking NULL level
    level <- NULL
    expect_error({initDGEobj(primaryAssayData = counts,
                             rowData          = rowData,
                             colData          = colData,
                             level            = level)},
                 regexp = "Specify the primaryAssayData, colData, rowData, and level. All are required to initialize a DGEobj.")

    # checking NA level
    level <- NA
    expect_error({initDGEobj(primaryAssayData = counts,
                             rowData          = rowData,
                             colData          = colData,
                             level            = level)},
                 regexp = "The specified level must be one of:  gene, exon, isoform, protein, affy")
})

test_that('init.R: initDGEobjDef - happy case', {
    myDGEobjDef <- initDGEobjDef(levels = "metabolomics",
                                 primaryAssayNames = "intensity",
                                 types <- c(normalizedIntensity = "assay"))
    expect_equal(length(myDGEobjDef), 5)
    expect_equal(names(myDGEobjDef$basetype), c("row", "col", "assay", "meta"))
    expect_equal(length(myDGEobjDef$type), 47)
    expect_equal(myDGEobjDef$allowedLevels, c("gene", "exon", "isoform", "protein", "affy", "metabolomics"))
    expect_equal(myDGEobjDef$primaryAssayNames[["affy"]], "AffyRMA")
})
