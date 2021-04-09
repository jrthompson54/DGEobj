context("reset.R functions")


test_that('reset.R: ', {
    # testing valid t_obj
    new <- resetDGEobj(t_obj)
    expect_s3_class(new, "DGEobj")
    expect_setequal(names(new), c("counts_orig", "counts", "design_orig", "design",
                                  "geneData_orig", "geneData", "granges_orig", "granges"))
    expect_equal(dim(new), c(1000, 48))

    # testing t_obj without platformType
    test_t_obj <- setAttributes(t_obj, list("PlatformType" = NULL))
    expect_error(resetDGEobj(test_t_obj),
                 regexp = "Required attribute \"PlatformType\" is missing.",
                 fixed  = TRUE)

    # testing t_obj with unavailable data
    test_t_obj <- setAttributes(t_obj, list("PlatformType" = "RNA-Seq"))
    names(test_t_obj) <- c("counts_orig", "counts", "design_orig", "design", "peptideAnnotation_orig")
    expect_error(resetDGEobj(test_t_obj),
                 regexp = "Gene/isoform/exon/protein data not found",
                 fixed  = TRUE)

    # testing t_obj with bad platformType
    test_t_obj1 <- setAttributes(t_obj, list("PlatformType" = "fred"))
    expect_error(resetDGEobj(test_t_obj1),
                 regexp = "The PlatformType attribute value was not recognized!")
})
