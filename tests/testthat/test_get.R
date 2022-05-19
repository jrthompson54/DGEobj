context("get.R functions")
skip_if(setup_failed)


test_that('get.R: getItem()', {
    getItem_DGEobj_test <- getItem(t_obj, 'design')

    expect_true(is.data.frame(getItem_DGEobj_test))
    expect_equal(nrow(getItem_DGEobj_test), 48)
    expect_equal(ncol(getItem_DGEobj_test), 10)

    expect_error(getItem(t_obj, 'fred'),
                 regexp = "The requested itemName should be in the DGEobj. Use names(dgeObj) to see the available items.",
                 fixed = TRUE)
})

test_that('get.R: getItems()', {
    getItems_DGEobj_one_test <- getItems(t_obj, 'counts')

    expect_true(is.matrix(getItems_DGEobj_one_test))
    expect_equal(nrow(getItems_DGEobj_one_test), t_dim[1]) &&
    expect_equal(ncol(getItems_DGEobj_one_test), t_dim[2])

    getItems_DGEobj_two_test <- getItems(t_obj, c('counts', 'design'))

    expect_type(getItems_DGEobj_two_test, 'list')
    expect_equal(length(getItems_DGEobj_two_test), 2)
    expect_setequal(names(getItems_DGEobj_two_test), c("counts", "design"))

    expect_warning(getItems(t_obj, c('fred', 'counts')),
                   regexp = "These item(s) not found: [fred]",
                   fixed  = TRUE)
    expect_warning(getItems(t_obj, c('fred', 'bob')),
                   regexp = "These item(s) not found: [fred]These item(s) not found: [bob]",
                   fixed  = TRUE)
})

test_that('get.R: getType()', {
    getType_DGEobj_test <- getType(t_obj, "design")

    expect_type(getType_DGEobj_test, 'list')
    expect_equal(length(getType_DGEobj_test), 1)
    expect_setequal(names(getType_DGEobj_test), c("design"))

    expect_warning(getType(t_obj, "fred"), "No items of specified type were found")
})

test_that('get.R: getBaseType()', {
    getBaseType_DGEobj_test <- getBaseType(t_obj, "col")

    expect_type(getBaseType_DGEobj_test, 'list')
    expect_equal(length(getBaseType_DGEobj_test), 2)
    expect_setequal(names(getBaseType_DGEobj_test), c("design", "ReplicateGroupDesign"))

    expect_error(getBaseType(t_obj, "counts"),
                 regexp = "baseType must be one of: row, col, assay, meta")
})
