rm(list=ls())

library(magrittr)
library(inventorydExperiment)
library(DGEobj)
# library(assertthat)
library(knitr)

RSE <- readRDS("../RSE.RDS")

MyCounts <- assay(RSE, "Counts")
attr(MyCounts, "Algorithm") <- "RSEM"
MyGeneAnno <- mcols(RSE)  #without chr info
MyGeneAnno <- rowRanges(RSE) %>% as.data.frame #with chr info
rownames(MyGeneAnno) <- MyGeneAnno$ID
Design <- colData(RSE) %>% as.data.frame

d <- initDGEobj(counts=MyCounts, colData=Design, rowData=MyGeneAnno, "gene")

MyContrast <- MyGeneAnno
d %<>% addItem(MyContrast, "contrastTest", "topTable", overwrite = T)

kable(inventory(d, verbose=T))

dim(d)

# d %<>% rmItem("design")
kable(inventory(d))

#test trap for overwriting item
d %<>% addItem(Design, "design", "design")
kable(inventory(d))

#force overwrite
d %<>% addItem(Design, "design", "design", overwrite=T)
kable(inventory(d))

#test trap for adding 2nd instance of unique item
d %<>% addItem(MyCounts, "counts", "counts")

#try to remove a nonexistent item
d %<>% rmItem("xxx")

kable(inventory(d))

kable(inventory(d, verbose=T))

showTypes(d)

attributes(d)

length(d)

myAnnotation <- getItem(d, "design")
myCounts <- getItem(d, "counts")
kable(inventory(d))

dimnames(d)
colnames(d)
rownames(d)

d %<>% addItem(assay(RSE, "Counts"), "TPM", "TPM")

Items <- getType(d, "counts")
Items <- getType(d, c("x", "counts", "design"))
Items <- getBaseType(d, "assay")
Items <- getItem (d, "counts")

showTypes(d)
d <- newType(d, "LogTPM", "assay", uniqueItem = TRUE)
showTypes(d)

head(rownames(d))
head(colnames(d))
nrow(d)
ncol(d)






