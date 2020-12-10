context("attributes.R functions")


test_that("attributes.R: showAttributes()", {
    output <- capture_output_lines(showAttributes(t_obj))

    expect_gt(length(output), 500)
    expect_setequal(capture_output_lines(showAttributes(NULL)), c("[1] \"dataName:\"", "[1] \"atnames: \"", "[1] \"dataName:\"", "[1] \"atnames: \""))
})

test_that("attributes.R: setAttributes()/getAttributes()", {
    new_attributes <- list("attribute1" = runif(100, min = 0, max = 2), "attribute2" = LETTERS)
    new_dgeobj     <- setAttributes(t_obj, new_attributes)

    output         <- getAttributes(new_dgeobj)

    expect_type(output, "list")
    expect_true(exists("attribute1", where = output))
    expect_true(exists("attribute2", where = output))
    expect_setequal(output$attribute2, LETTERS)
})

test_that("attributes.R: setAttribute()/getAttribute()", {
    new_dgeobj     <- setAttribute(t_obj, LETTERS, "new_attribute")
    output         <- getAttribute(new_dgeobj, "new_attribute")

    expect_type(output, "character")
    expect_setequal(output, LETTERS)
})

test_that("attributes.R: getAttributes() returns all", {
    output <- getAttributes(t_obj)

    expect_type(output, "list")
    expect_true(exists("objDef", where = output))
    expect_true(exists("type", where = output))
    expect_true(exists("basetype", where = output))
    expect_true(exists("parent", where = output))
    expect_true(exists("funArgs", where = output))

    output <- getAttributes(t_obj, excludeList = list("type", "basetype"))

    expect_false(exists("type", where = output))
    expect_false(exists("basetype", where = output))
})

test_that("attributes.R: showMeta()", {
    output <- showMeta(t_obj, printed = FALSE)

    expect_s3_class(output, "data.frame")
    expect_equal(output$Value[output$Attribute == "class"], "DGEobj")

    expect_null(showMeta(NULL))
})

test_that("attributes.R: incorrect usage", {
    expect_error(setAttributes(t_obj, attribs = NULL),
                 regexp = "attribs must be of class 'list'.")
    expect_error(setAttributes(t_obj, attribs = list()),
                 regexp = "The attribs list should be a named list, specifying the attribute/value pairs. It must have names specified.")
    expect_error(getAttribute(t_obj, NULL),
                 regexp = "'which' must be of mode character")

    expect_null(getAttributes("fred"))
    expect_null(getAttributes(NULL))
    expect_null(getAttributes(list()))
    expect_null(getAttribute("fred", "fred"))
})
