context("rmItem.R functions")


test_that('rmItem.R: rmItem()/rmItems()', {
    rmItem_design_t_obj <- rmItem(t_obj, "design")
    expect_s3_class(rmItem_design_t_obj, "DGEobj")
    expect_equal(length(rmItem_design_t_obj), 7)

    rmItems_t_obj <- rmItems(t_obj, list("design", "counts"))
    expect_s3_class(rmItems_t_obj, "DGEobj")
    expect_equal(length(rmItems_t_obj), 6)

    rmItems_byindex_t_obj <- rmItems(t_obj, c(1, 2, 3))
    expect_s3_class(rmItems_byindex_t_obj, "DGEobj")
    expect_equal(length(rmItems_byindex_t_obj), 5)
})

test_that('rmItem.R: incorrect usage', {
    expect_error(rmItem(t_obj, c("design", "intensity")),
                 regexp = "Specify a singular itemName as a character string.")
    expect_error(rmItem(t_obj, "counts123"),
                 regexp = "counts123 does not exist within dgeObj")
    expect_error(rmItems(t_obj, c("counts123", "genes")),
                 regexp = "counts123 does not exist within dgeObj")
    expect_error(rmItems(t_obj, c(70000)),
                 regexp = "A value in the numeric index is larger than the number of items in dgeObj")
})
