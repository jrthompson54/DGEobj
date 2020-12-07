require(testthat)
require(stringr)

require(DGEobj)
require(GenomicRanges)

t_obj <- readRDS(system.file("exampleObj.RDS", package = "DGEobj", mustWork = TRUE))
t_dim <- c(1000, 48)
