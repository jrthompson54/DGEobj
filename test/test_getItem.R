# test resetDGEobj
# RNA-Seq_Analysis_of_FL_IBD_Human_Biopsy_P-20170717-0001.RDS
library(magrittr)
library(tidyverse)
library(DGEobj)
library(JRTutil)

dgeObj <- getRDSobjFromStash("RNA-Seq_Analysis_of_FL_IBD_Human_Biopsy_P-20170717-0001.RDS")
dim(dgeObj)

inventory(dgeObj)

#getItem test
x <- getItem(dgeObj, "counts")
assertthat::assert_that("matrix" %in% class(x),
                        all(dim(x) == dim(dgeObj)))
#getItems test
x <- getItems(dgeObj, c("counts", "DGEList"))


x <- getItems(dgeObj, list("counts", "DGEList"))

x <- getItems(dgeObj, list("counts", "DGEList", "bogus" ))
