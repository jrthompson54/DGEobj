context("reset.R functions")
skip_if(setup_failed)

# test gene level data
test_that('reset.R: gene level data', {

    test_t_obj <- t_obj
    test_t_reset <- resetDGEobj(test_t_obj)

    # object validation
    expect_s3_class(test_t_reset, "DGEobj")
    expect_equivalent(showMeta(test_t_reset), showMeta(test_t_obj))

    # validate level is gene before and after reset
    test_t_meta <- showMeta(test_t_obj)
    expect_equal("gene", test_t_meta$Value[3])
    expect_equal(attr(test_t_obj, "level"), "gene")

    test_t_meta_reset <- showMeta(test_t_reset)
    expect_equal("gene", test_t_meta_reset$Value[3])
    expect_equal(attr(test_t_reset, "level"), "gene")

    # reset after subsetting
    expect_equal(dim(test_t_obj), t_dim)
    test_t_obj <- test_t_obj[c(1:10), ]
    test_t_reset <- resetDGEobj(test_t_obj)
    expect_equal(dim(test_t_reset), c(1000, 48))

    # check names after reset
    expect_named(test_t_obj, c('counts_orig', 'counts', 'design_orig', 'design', 'geneData_orig',
                               'geneData', 'granges_orig', 'granges', 'DGEList', 'ReplicateGroupDesign',
                               'ReplicateGroupDesign_Elist', 'ReplicateGroupDesign_fit',
                               'ReplicateGroupDesign_fit_cm', 'ReplicateGroupDesign_fit_cf',
                               'BDL_vs_Sham', 'EXT1024_vs_BDL', 'Nint_vs_BDL', 'Sora_vs_BDL'))
    expect_named(test_t_reset, c("counts_orig", "counts", "design_orig", "design",
                                 "geneData_orig", "geneData", "granges_orig", "granges"))

    # testing t_obj with rm item
    test_t_obj <- rmItem(test_t_obj, "BDL_vs_Sham")
    expect_false("BDL_vs_Sham" %in% names(test_t_obj))
    test_t_reset <- resetDGEobj(test_t_obj)
    expect_false("BDL_vs_Sham" %in% names(test_t_reset)) # should not be restored

    # testing t_obj with add item
    test_t_obj <- addItem(test_t_obj,
                          item = 'Fred Flintstone',
                          itemName = 'Cartoon',
                          itemType = 'meta',
                          itemAttr = list('MyAttribute' = 'testObject'))

    test_t_attr <- getAttributes(test_t_obj)
    expect_equivalent(attributes(test_t_obj$Cartoon), 'testObject')
    expect_true("Cartoon" %in% names(test_t_obj))

    test_t_reset <- resetDGEobj(test_t_obj)
    expect_null(attributes(test_t_reset$Cartoon), 'testObject')
    expect_false("Cartoon" %in% names(test_t_reset)) # check if removed


    # testing t_obj after class change
    test_t_obj <- as.list.DGEobj(test_t_obj)
    expect_equal(class(test_t_obj), "list")
    # coerce back into DGEobj, expect reset to work
    class(test_t_obj) <- "DGEobj"
    expect_silent(resetDGEobj(test_t_obj))
})

# test isoform level data
test_that('reset.R: isoform level data', {

    test_t_obj <- t_isoform_obj
    test_t_reset <- resetDGEobj(test_t_obj)

    # object validation
    expect_s3_class(test_t_reset, "DGEobj")
    expect_equivalent(showMeta(test_t_reset), showMeta(test_t_obj))

    # validate level is isoform before and after reset
    test_t_meta <- showMeta(test_t_obj)
    expect_equal("isoform", test_t_meta$Value[3])
    expect_equal(attr(test_t_obj, "level"), "isoform")

    test_t_meta_reset <- showMeta(test_t_reset)
    expect_equal("isoform", test_t_meta_reset$Value[3])
    expect_equal(attr(test_t_reset, "level"), "isoform")

    # reset after subsetting
    test_t_dim <- dim(t_isoform_obj)
    expect_equal(dim(test_t_obj),test_t_dim)
    test_t_obj <- test_t_obj[c(1:10), ]
    test_t_reset <- resetDGEobj(test_t_obj)
    expect_equal(dim(test_t_reset), test_t_dim)

    # check names after reset
    expect_named(test_t_obj, c("intensities_orig", "intensities", "design_orig",
                               "design", "isoformData_orig", "isoformData"))
    expect_named(test_t_reset, c("intensities_orig", "intensities", "design_orig",
                                   "design", "isoformData_orig", "isoformData"))

    # testing test_t_obj with rm item
    test_t_obj <- rmItem(test_t_obj, "isoformData")
    expect_false("isoformData" %in% names(test_t_obj))
    test_t_reset <- resetDGEobj(test_t_obj)
    expect_true("isoformData" %in% names(test_t_reset)) # check if restored

    # testing test_t_obj with add item
    test_t_obj <- addItem(test_t_obj,
                          item = 'Fred Flintstone',
                          itemName = 'Cartoon',
                          itemType = 'meta',
                          itemAttr = list('MyAttribute' = 'testObject'))

    test_t_attr <- getAttributes(test_t_obj)
    expect_equivalent(attributes(test_t_obj$Cartoon), 'testObject')
    expect_true("Cartoon" %in% names(test_t_obj))

    test_t_reset <- resetDGEobj(test_t_obj)
    expect_null(attributes(test_t_reset$Cartoon), 'testObject')
    expect_false("Cartoon" %in% names(test_t_reset)) # check if removed

    # testing test_t_obj after class change
    test_t_obj <- as.list.DGEobj(test_t_obj)
    expect_equal(class(test_t_obj), "list")
    # coerce back into DGEobj, expect reset to work
    class(test_t_obj) <- "DGEobj"
    expect_silent(resetDGEobj(test_t_obj))
})

# test exon level data
test_that('reset.R: exon level data', {

    test_t_obj <- t_exon_obj
    test_t_reset <- resetDGEobj(test_t_obj)

    # object validation
    expect_s3_class(test_t_reset, "DGEobj")
    expect_equivalent(showMeta(test_t_reset), showMeta(test_t_obj))

    # validate level is exon before and after reset
    test_t_meta <- showMeta(test_t_obj)
    expect_equal("exon", test_t_meta$Value[3])
    expect_equal(attr(test_t_obj, "level"), "exon")

    test_t_meta_reset <- showMeta(test_t_reset)
    expect_equal("exon", test_t_meta_reset$Value[3])
    expect_equal(attr(test_t_reset, "level"), "exon")

    # reset after subsetting
    test_t_dim <- dim(t_exon_obj)
    expect_equal(dim(test_t_obj), test_t_dim)
    test_t_obj <- test_t_obj[c(1:10), ]
    test_t_reset <- resetDGEobj(test_t_obj)
    expect_equal(dim(test_t_reset), test_t_dim)

    # check names after reset
    expect_named(test_t_obj, c('counts_orig', 'counts', 'design_orig', 'design', 'exonData_orig',
                               'exonData', 'granges_orig', 'granges'))
    expect_named(test_t_reset, c("counts_orig", "counts", "design_orig", "design",
                                 "exonData_orig", "exonData", "granges_orig", "granges"))

    # testing test_t_obj with rm item
    test_t_obj <- rmItem(test_t_obj, "granges")
    expect_false("granges" %in% names(test_t_obj))
    test_t_reset <- resetDGEobj(test_t_obj)
    expect_true("granges" %in% names(test_t_reset)) # check if persists

    # testing test_t_obj with add item
    test_t_obj <- addItem(test_t_obj,
                          item = 'Fred Flintstone',
                          itemName = 'Cartoon',
                          itemType = 'meta',
                          itemAttr = list('MyAttribute' = 'testObject'))

    test_t_attr <- getAttributes(test_t_obj)
    expect_equivalent(attributes(test_t_obj$Cartoon), 'testObject')
    expect_true("Cartoon" %in% names(test_t_obj))

    test_t_reset <- resetDGEobj(test_t_obj)
    expect_null(attributes(test_t_reset$Cartoon), 'testObject')
    expect_false("Cartoon" %in% names(test_t_reset)) # check if removed

    # testing test_t_obj after class change
    test_t_obj <- as.list.DGEobj(test_t_obj)
    expect_equal(class(test_t_obj), "list")
    # coerce back into DGEobj, expect reset to work
    class(test_t_obj) <- "DGEobj"
    expect_silent(resetDGEobj(test_t_obj))
})

# test protein level data
test_that('reset.R: protein level data', {

    test_t_obj <- t_protein_obj
    test_t_reset <- resetDGEobj(test_t_obj)

    # object validation
    expect_s3_class(test_t_reset, "DGEobj")
    expect_equivalent(showMeta(test_t_reset), showMeta(test_t_obj))

    # validate level is protein before and after reset
    test_t_meta <- showMeta(test_t_obj)
    expect_equal("protein", test_t_meta$Value[3])
    expect_equal(attr(test_t_obj, "level"), "protein")

    test_t_meta_reset <- showMeta(test_t_reset)
    expect_equal("protein", test_t_meta_reset$Value[3])
    expect_equal(attr(test_t_reset, "level"), "protein")

    # reset after subsetting
    test_t_dim <- dim(t_protein_obj)
    expect_equal(dim(test_t_obj), test_t_dim)
    test_t_obj <- test_t_obj[c(1:10), ]
    test_t_reset <- resetDGEobj(test_t_obj)
    expect_equal(dim(test_t_reset), test_t_dim)

    # check names after reset
    expect_named(test_t_obj, c('intensities_orig', 'intensities', 'design_orig',
                               'design', 'proteinData_orig', 'proteinData'))
    expect_named(test_t_reset, c('intensities_orig', 'intensities', 'design_orig',
                                 'design', 'proteinData_orig', 'proteinData'))

    # testing test_t_obj with rm item
    test_t_obj <- rmItem(test_t_obj, "intensities")
    expect_false("intensities" %in% names(test_t_obj))
    test_t_reset <- resetDGEobj(test_t_obj)
    expect_true("intensities" %in% names(test_t_reset)) # check if persists

    # testing test_t_obj with add item
    test_t_obj <- addItem(test_t_obj,
                          item = 'Fred Flintstone',
                          itemName = 'Cartoon',
                          itemType = 'meta',
                          itemAttr = list('MyAttribute' = 'testObject'))

    test_t_attr <- getAttributes(test_t_obj)
    expect_equivalent(attributes(test_t_obj$Cartoon), 'testObject')
    expect_true("Cartoon" %in% names(test_t_obj))

    test_t_reset <- resetDGEobj(test_t_obj)
    expect_null(attributes(test_t_reset$Cartoon), 'testObject')
    expect_false("Cartoon" %in% names(test_t_reset)) # check if removed

    # testing test_t_obj after class change
    test_t_obj <- as.list.DGEobj(test_t_obj)
    expect_equal(class(test_t_obj), "list")
    # coerce back into DGEobj, expect reset to work
    class(test_t_obj) <- "DGEobj"
    expect_silent(resetDGEobj(test_t_obj))
})

# misc tests
test_that('reset.R: misc', {

    test_t_obj  <- t_obj
    test_t_reset <- resetDGEobj(t_obj)

    #test invalid level
    expect_error(test_t_obj <- initDGEobj(primaryAssayData = t_obj$counts,
                                          rowData = t_obj$geneData,
                                          colData = t_obj$design,
                                          level = "Fred Flinstone"),
                                          regexp = "The specified level must be one of:  gene, exon, isoform, protein, affy")

    # testing t_obj with new attributes
    new_attributes <- list("attribute1" = runif(100, min = 0, max = 2), "attribute2" = LETTERS)
    test_t_obj <- setAttributes(test_t_obj, new_attributes)
    test_t_attr <- getAttributes(test_t_obj)
    expect_true(exists('attribute1', where = getAttributes(test_t_obj)))
    expect_setequal(test_t_attr$attribute2, LETTERS)

    test_t_reset <- resetDGEobj(test_t_obj)
    test_t_reset_attr <- getAttributes(test_t_reset)
    expect_true(exists('attribute1', where = getAttributes(test_t_reset)))
    expect_setequal(test_t_reset_attr$attribute2, LETTERS)

    # testing t_obj without platformType (no longer required for reset)
    test_t_obj <- setAttributes(t_obj, list("PlatformType" = NULL))
    expect_s3_class(resetDGEobj(test_t_obj), "DGEobj")

    # testing rm item with _orig
    test_t_obj <- rmItem(test_t_obj, "counts_orig")
    expect_false("counts_orig" %in% names(test_t_obj))
    expect_error(resetDGEobj(test_t_obj),
                 regexp = 'The requested itemName should be in the DGEobj. Use names(dgeObj) to see the available items.',
                 fixed = TRUE)

})
