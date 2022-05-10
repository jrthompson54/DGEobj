context("utils.R functions")
skip_if(setup_failed)

test_that('utils.R: dim()/dimnames()', {
    expect_equal(dim(t_obj), t_dim)
    dimnames_t_obj <- dimnames(t_obj)

    expect_type(dimnames_t_obj, 'list')
    expect_equal(length(dimnames_t_obj), 2)
    expect_setequal(names(dimnames_t_obj), c("rownames", "colnames"))
    expect_equal(length(dimnames_t_obj[[1]]), t_dim[1])
    expect_equal(length(dimnames_t_obj[[2]]), t_dim[2])
})

test_that('utils.R: inventory()', {
    inventory_t_obj <- inventory(t_obj)

    expect_true(is.data.frame(inventory_t_obj))
    expect_equal(nrow(inventory_t_obj), 18)
    expect_equal(ncol(inventory_t_obj), 8)
    expect_setequal(names(inventory_t_obj), c("ItemName", "ItemType", "BaseType", "Parent", "Class", "Row", "Col", "DateCreated"))

    inventory_t_obj_verbose <- inventory(t_obj, verbose = TRUE)

    expect_true(is.data.frame(inventory_t_obj_verbose))
    expect_equal(nrow(inventory_t_obj_verbose), 18)
    expect_equal(ncol(inventory_t_obj_verbose), 9)
    expect_setequal(names(inventory_t_obj), c("ItemName", "ItemType", "BaseType", "Parent", "Class", "Row", "Col", "DateCreated"))
})

test_that('utils.R: print()', {
    expect_output(print(t_obj), "ItemName")
})

test_that('utils.R: as.list()', {
    list_DGEobj <- as.list(t_obj)
    expect_true(is.list(list_DGEobj))
})
