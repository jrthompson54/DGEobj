context("subset.R functions")
skip_if(setup_failed)

test_that('subset.R: subset()', {
    subsett_obj_1.0 <- subset(t_obj, row = c(1:5))
    subsett_obj_1.1 <- t_obj[c(1:5), ]
    subsett_obj_1.2 <- t_obj[row = c(1:5) ]

    expect_equal(subsett_obj_1.0, subsett_obj_1.1)
    expect_equal(subsett_obj_1.0, subsett_obj_1.2)
    expect_equal(dim(subsett_obj_1.0), c(5, t_dim[2]))

    subsett_obj_2.0 <- subset(t_obj, col = c(1:5))
    subsett_obj_2.1 <- t_obj[, c(1:5)]
    subsett_obj_2.2 <- t_obj[col = c(1:5)]

    expect_equal(subsett_obj_2.0, subsett_obj_2.1)
    expect_equal(subsett_obj_2.0, subsett_obj_2.2)
    expect_equal(dim(subsett_obj_2.0), c(t_dim[1], 5))

    subsett_obj_3.0 <- subset(t_obj, row = c(1:5), col = c(1:5))
    subsett_obj_3.1 <- t_obj[c(1:5), c(1:5)]
    subsett_obj_3.2 <- t_obj[row = c(1:5), col = c(1:5)]

    expect_equal(subsett_obj_3.0, subsett_obj_3.1)
    expect_equal(subsett_obj_3.0, subsett_obj_3.2)
    expect_equal(dim(subsett_obj_3.0), c(5, 5))

    subsett_obj_4 <- subset(t_obj,
                             row = rownames(t_obj$geneData)[1:2],
                             col = colnames(t_obj$counts)[1:2])
    expect_equal(dim(subsett_obj_4), c(2, 2))

    expect_equivalent(t_obj, t_obj[])
    expect_equivalent(t_obj, subset(t_obj))

    debug_messages <- capture_output(subset(t_obj, row = c(1:5), debug = TRUE))
    expect_match(debug_messages,
                 regexp = paste0("subsetting counts_orig meta .* dim: ", t_dim[1], ":", t_dim[2], ".*"))
})

test_that('subset.R: incorrect usage', {
    expect_error(subset(t_obj, row = c(50000:50005)),
                 regexp = "row coordinates out of range")
    expect_error(t_obj[c(50000:50005)],
                 regexp = "row coordinates out of range")
    expect_error(subset(t_obj, col = c(50000:50005)),
                 regexp = "col coordinates out of range")
    expect_error(t_obj[, c(50000:50005)],
                 regexp = "col coordinates out of range")
    expect_warning(subset(t_obj, row = LETTERS),
                   regexp = "26 items in row index not found in rownames(x)",
                   fixed = TRUE)
    expect_warning(t_obj[LETTERS],
                   regexp = "26 items in row index not found in rownames(x)",
                   fixed = TRUE)
    expect_warning(subset(t_obj, col = LETTERS),
                   regexp = "26 items in col index not found in colnames(x)",
                   fixed = TRUE)
    expect_warning(t_obj[, LETTERS],
                   regexp = "26 items in col index not found in colnames(x)",
                   fixed = TRUE)
})
