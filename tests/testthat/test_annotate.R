context("DGEobj - tests for annotate.R functions")
skip_if(setup_failed)

test_that('annotate.R: annotateDGEobj()', {
    ann.file <- tempfile("annotations_test", fileext = ".txt")
    writeLines(c("key1='value 1'", "key2=value 2"), con = ann.file)

    ann_DGEobj <- annotateDGEobj(t_obj, ann.file)
    expect_equal(attr(ann_DGEobj, 'key1'), "'value 1'")
    expect_equal(attr(ann_DGEobj, 'key2'), 'value 2')
    expect_null(attr(ann_DGEobj, 'key3'))

    ann_DGEobj <- annotateDGEobj(t_obj, ann.file, keys = list("key2"))
    expect_null(attr(ann_DGEobj, 'key1'))
    expect_equal(attr(ann_DGEobj, 'key2'), 'value 2')

    ann.list <- list("key1" = "'value 1'", "key2" = "value 2")
    ann_DGEobj <- annotateDGEobj(t_obj, ann.list)
    expect_equal(attr(ann_DGEobj, 'key1'), "'value 1'")
    expect_equal(attr(ann_DGEobj, 'key2'), 'value 2')
    expect_null(attr(ann_DGEobj, 'key3'))

    ann_DGEobj <- annotateDGEobj(t_obj, ann.list, keys = list("key2"))
    expect_null(attr(ann_DGEobj, 'key1'))
    expect_equal(attr(ann_DGEobj, 'key2'), 'value 2')
})

test_that('annotate.R: incorrect usage', {
    expect_error(annotateDGEobj(t_obj),
                 regexp = "argument \"annotations\" is missing, with no default")
    expect_error(annotateDGEobj(t_obj, NULL),
                 regexp = "When annotations is NULL, no attribute gets added to the dgeObj.")
    expect_error(annotateDGEobj(t_obj, "nonexistantfile.txt"),
                 regexp = "The file specified does not exist.")
})
