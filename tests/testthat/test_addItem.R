context("addItem.R functions")


test_that('addItem.R: addItem()', {
    #meta
    add <- addItem(t_obj,
                   item = 'Fred Flintstone',
                   itemName = 'Cartoon',
                   itemType = 'meta',
                   itemAttr = list('MyAttribute' = 'testObject'))
    expect_equivalent(add$Cartoon, "Fred Flintstone")
    expect_equivalent(attributes(add$Cartoon), 'testObject')

    #rows cols - matrix
    dims <- dim(t_obj)
    data.r <- matrix(rep("rowval", dims[1]), nrow = dims[1])
    rownames(data.r) <- dimnames(t_obj$counts)[[1]]

    data.c <- matrix(rep("colval", dims[2], nrow = dims[2]))
    rownames(data.c) <- dimnames(t_obj$counts)[[2]]

    add <- addItem(t_obj, item = data.r,
                   itemName = 'MyMatrixRow',
                   itemType = 'row')

    expect_true('MyMatrixRow' %in% names(add))
    expect_equal(dim(add$MyMatrixRow), c(dims[1], 1))
    expect_error(addItem(t_obj, item = data.r[-1, -1], itemName = "MismatchedSize", itemType = "row"),
                 regexp = "New row object does not match row dimension of DGEobj object")

    add <- addItem(t_obj, item = data.c,
                   itemName = 'MyMatrixCol',
                   itemType = 'col')

    expect_true('MyMatrixCol' %in% names(add))
    expect_equal(dim(add$MyMatrixCol), c(dims[2], 1))
    expect_error(addItem(t_obj, item = data.c[-1, -1], itemName = "MismatchedSize", itemType = "col"),
                 regexp = "Rows in new col object must match col dimension of DGEobj object")

    #assay
    dims <- dim(t_obj)
    assay <- matrix(runif(dims[1]*dims[2]), nrow = dims[1])
    rownames(assay) <- dimnames(t_obj$counts)[[1]]
    colnames(assay) <- dimnames(t_obj$counts)[[2]]

    add <- addItem(t_obj, item = assay,
                   itemName = 'MyAssay',
                   itemType = 'assay')

    expect_true('MyAssay' %in% names(add))
    expect_equal(dim(add$MyAssay), c(dims[1], dims[2]))
    expect_error(addItem(t_obj, item = assay[-1, -1], itemName = "MismatchedSize", itemType = "assay"),
                 regexp = "New assay object does not match row dimension of DGEobj object")
})

test_that('addItem.R: addItems()', {
    dims <- dim(t_obj)
    data.r <- matrix(rep("rowval", dims[1]), nrow = dims[1])
    rownames(data.r) <- dimnames(t_obj$counts)[[1]]

    data.c <- matrix(rep("colval", dims[2], nrow = dims[2]))
    rownames(data.c) <- dimnames(t_obj$counts)[[2]]

    assay <- matrix(runif(dims[1]*dims[2]), nrow = dims[1])
    rownames(assay) <- dimnames(t_obj$counts)[[1]]
    colnames(assay) <- dimnames(t_obj$counts)[[2]]

    add <- addItems(t_obj,
                    itemList = list("Cartoon" = "Fred Flintstone",
                                    "Historic" = "Abe Lincoln",
                                    "MyMatrixRow" = data.r,
                                    "MyMatrixCol" = data.c,
                                    "MyAssay" = assay),
                    itemTypes = list("meta", "meta", "row", "col", "assay"),
                    itemAttr  = list('MyAttribute1' = 'testObject1',
                                     'MyAttribute2' = 'testObject2'))

    expect_equal(add$Cartoon[1], "Fred Flintstone")
    expect_equal(add$Historic[1], "Abe Lincoln")
    expect_true('MyMatrixCol' %in% names(add))
    expect_equal(dim(add$MyMatrixCol), c(dims[2], 1))
    expect_true('MyMatrixRow' %in% names(add))
    expect_equal(dim(add$MyMatrixRow), c(dims[1], 1))
    expect_true('MyAssay' %in% names(add))
    expect_equal(dim(add$MyAssay), c(dims[1], dims[2]))
    expect_equal(attr(add$Cartoon,"MyAttribute1"), "testObject1")
    expect_equal(attr(add$Cartoon,"MyAttribute2"), "testObject2")
})

test_that('addItem.R: incorrect usage', {
    error_message_itemType <- "itemType must be one of: row, col, assay, meta, *"

    expect_error(addItem(matrix(rep(0, 5), nrow = 5)),
                 regexp = "Specify the DGEobj, item, itemName, and itemType")
    expect_error(addItem(NULL),
                 regexp = "Specify the DGEobj, item, itemName, and itemType")
    expect_error(addItem(t_obj),
                 regexp = "Specify the DGEobj, item, itemName, and itemType")
    expect_error(addItem(t_obj, item = 'mystring', itemName = 'teststring', itemType = 'badtype'),
                 regexp = error_message_itemType)
    expect_error(addItem(t_obj, item = 'mystring', itemName = 'teststring', itemType = 'row'),
                 regexp = "Row basetypes must have rownames")
    expect_error(addItem(t_obj, item = 'mystring', itemName = 'teststring', itemType = 'col'),
                 regexp = "Col basetypes must have rownames")
    expect_error(addItem(t_obj, item = 'mystring', itemName = 'teststring', itemType = 'assay'),
                 regexp = "Assay basetypes must have row and column names")
    expect_error(addItem(t_obj, item = matrix(rep(0, 25), nrow = 5), itemName = 'testmatrix', itemType = 'row'),
                 regexp = "Row basetypes must have rownames")
    expect_error(addItem(t_obj, item = matrix(rep(0, 25), nrow = 5), itemName = 'testmatrix', itemType = 'col'),
                 regexp = "Col basetypes must have rownames")
    expect_error(addItem(t_obj, item = matrix(rep(0, 25), nrow = 5), itemName = 'testmatrix', itemType = 'assay'),
                 regexp = "Assay basetypes must have row and column names")
    expect_error(addItem(t_obj, item = t_obj$counts_orig, itemName = 'counts_orig', itemType = 'meta'),
                 regexp = "itemName (counts_orig) already exists in DGEobj!",
                 fixed = TRUE)
    expect_error(addItems(NULL),
                 regexp = "Specify the DGEobj, itemList, and itemTypes. All are required.")
    expect_error(addItems(t_obj),
                 regexp = "Specify the DGEobj, itemList, and itemTypes. All are required.")
    expect_error(addItems(t_obj, itemList = list('teststring' = 'mystring'), itemTypes = list('badtype')),
                 regexp = error_message_itemType)
    expect_error(addItems(t_obj, itemList = list('teststring' = 'mystring'), itemTypes = list('row')),
                 regexp = "Row basetypes must have rownames")
    expect_error(addItems(t_obj, itemList = list('teststring' = 'mystring'), itemTypes = list('col')),
                 regexp = "Col basetypes must have rownames")
    expect_error(addItems(t_obj, itemList = list('teststring' = 'mystring'), itemTypes = list('assay')),
                 regexp = "Assay basetypes must have row and column names")
    expect_error(addItems(t_obj, itemList = list('testmatrix' = matrix(rep(0, 25), nrow = 5)), itemTypes = list('row')),
                 regexp = "Row basetypes must have rownames")
    expect_error(addItems(t_obj, itemList = list('testmatrix' = matrix(rep(0, 25), nrow = 5)), itemTypes = list('col')),
                 regexp = "Col basetypes must have rownames")
    expect_error(addItems(t_obj, itemList = list('testmatrix' = matrix(rep(0, 25), nrow = 5)), itemTypes = list('assay')),
                 regexp = "Assay basetypes must have row and column names")
    expect_error(addItems(t_obj,
                          itemList = list("Cartoon" = "Fred Flintstone", "Historic" = "Abe Lincoln"),
                          itemTypes = list("meta", "meta"),
                          parents = list("p1")),
                 regexp = "The parents list must be of class 'list' and of the same length as the itemList.")
})
