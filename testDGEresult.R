rm(list=ls())

library(magrittr)
library(SummarizedExperiment)
library(DGEobj)
library(assertthat)

RSE <- readRDS("RSE.RDS")

MyCounts <- assay(RSE, "Counts")
attr(MyCounts, "Algorithm") <- "RSEM"
MyGeneAnno <- mcols(RSE)  #without chr info
MyGeneAnno <- rowRanges(RSE) %>% as.data.frame #with chr info
rownames(MyGeneAnno) <- MyGeneAnno$ID
Design <- colData(RSE) %>% as.data.frame

d <- initDGEobj(counts=MyCounts, colData=Design, rowData=MyGeneAnno, "gene")
MyContrast <- MyGeneAnno

d %<>% addItem(MyContrast, "contrastTest", "topTable", overwrite = T)

print(d, verbose=T)

dim(d)
print(d, verbose=T)

d %<>% rmItem("design")
print(d)

#test trap for overwriting item
d %<>% addItem(Design, "Design", "design")
print(d)

#test trap for adding 2nd instance of unique item
d %<>% addItem(MyCounts, "Morecounts", "counts")

#force overwrite
d %<>% addItem(Design, "Design", "design", overwrite=T)
print(d)

#try to remove a nonexistent item
d %<>% rmItem("xxx")

print(d)

print(d, verbose=T)

showTypes(d)

attributes(d)

length(d)

myAnnotation <- getItem(d, "Design")
myCounts <- getItem(d, "counts")
print(d)

dimnames(d)
colnames(d)
rownames(d)

d %<>% addItem(assay(RSE, "Counts"), "TPM", "TPM")

Items <- getType(d, "counts")
Items <- getType(d, c("assay", "counts", "design"))
Items <- getBaseType(d, "assay")
Items <- getItem (d, "counts")

showTypes(d)
d <- newType(d, "LogTPM", "assay", uniqueItem = TRUE)
showTypes(d)

head(rownames(d))
head(colnames(d))
nrow(d)
ncol(d)

# test renameing dimnames
shortcolnames <- Design$Barcode
lcgenenames <- tolower(rownames(counts))
dimnames(d) <- list(lcgenenames, shortcolnames) #dimnames assignment doesn't work

#Function List
# addItem.R
# newType.R
# colnames.R
# DGEresult.R
# dim.R
# dimensionMatch.R
# getItem.R
# getItems.R
# getType.R
# itemNames.R
# print.R
# rmItem.R
# rownames.R
# showTypes.R


