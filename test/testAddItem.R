library(DGEobj)
library(DGE.Tools2)
d <- readRDS("../DGEobj.RDS")

l2c <- convertCounts(d$counts, unit="cpm", log=TRUE, normalize="tmm")

d2 <- addItem(d, l2c, "Log2CPM", itemType = "assay")
