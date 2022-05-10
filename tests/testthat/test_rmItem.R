context("rmItem.R functions")
skip_if(setup_failed)

test_that('rmItem.R: rmItem()', {
    expect_length(t_obj, 18)
    rmItem_design_t_obj <- rmItem(t_obj, "design")
    expect_s3_class(rmItem_design_t_obj, "DGEobj")
    expect_length(rmItem_design_t_obj, 17)
    expect_true(!("design" %in% names(rmItem_design_t_obj)))
    expect_true("design_orig" %in% names(rmItem_design_t_obj))
})

test_that('rmItem.R: incorrect usage', {
    expect_error(rmItem(t_obj, c("design", "intensity")),
                 regexp = "Specify a singular itemName as a character string.")
    expect_error(rmItem(t_obj, "counts123"),
                 regexp = "counts123 does not exist within dgeObj")
})
