context("types.R functions")


test_that('types.R: baseType()', {
    expect_equal(baseType(t_obj, "intensity"), "assay")
    expect_equal(baseType(t_obj, "design"), "col")
    expect_equal(baseType(t_obj, "intensity_orig"), "meta")

    expect_equivalent(baseType(t_obj, "counts"), "assay")

    expect_error(baseType(t_obj, "dog"),
                 regexp = "subscript out of bounds")
})

test_that('types.R: baseTypes()', {
    expect_setequal(baseTypes(), c("row", "col", "assay", "meta" ))
    expect_setequal(baseTypes(t_obj), c("row", "col", "assay", "meta"))
})

test_that('types.R: showTypes()', {
    showTypes_t_obj <- showTypes(t_obj, printed = FALSE)
    expect_s3_class(showTypes_t_obj, "data.frame")
    expect_equal(dim(showTypes_t_obj), c(49, 2))
})

test_that('types.R: newType()', {
    newType_t_obj <- newType(t_obj, "MyType", "meta")
    expect_true("MyType" %in% names(attr(newType_t_obj, "objDef")$type))
    expect_equal(attr(newType_t_obj, "objDef")$type[["MyType"]], "meta")
    expect_false("MyType" %in% attr(newType_t_obj, "objDef")$uniqueType)

    newType_t_obj <- newType(t_obj, "MyType", "assay", uniqueItem = TRUE)
    expect_true("MyType" %in% names(attr(newType_t_obj, "objDef")$type))
    expect_equal(attr(newType_t_obj, "objDef")$type[["MyType"]], "assay")
    expect_true("MyType" %in% attr(newType_t_obj, "objDef")$uniqueType)
})

test_that('types.R: incorrect usage', {
    expect_error(showTypes(),
                 regexp = "argument \"dgeObj\" is missing, with no default",
                 fixed  = TRUE)
    expect_error(newType(),
                 regexp = "Specify the DGEobj, itemType, and baseType. All three are required.",
                 fixed  = TRUE)
    expect_error(newType(t_obj),
                 regexp = "Specify the DGEobj, itemType, and baseType. All three are required.",
                 fixed  = TRUE)
    expect_error(newType(t_obj, "MyType", "badType"),
                 regexp = "The baseType must be one of the baseTypes available in the DGEobj. Use baseTypes(DGEobj) to see which are available.",
                 fixed  = TRUE)
})
