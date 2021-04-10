context("init.R functions")


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
