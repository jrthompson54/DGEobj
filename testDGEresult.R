rm(list=ls())
RSE <- readRDS("RSE.RDS")
library(magrittr)
library(SummarizedExperiment)
library(DGEobj)
library(assertthat)

MyCounts <- assay(RSE, "Counts")
MyGeneAnno <- mcols(RSE)
rownames(MyGeneAnno) <- MyGeneAnno$ID
Design <- colData(RSE)

d <- initDGEobj(MyCounts, Design, MyGeneAnno, "geneDat")

d %<>% addItem(MyGeneAnno, "contrastTest", "topTable")
print(d)
dim(d)

d %<>% rmItem("Design")
d %<>% rmItem("contrastTest")
print(d)

d %<>% addItem(colData(RSE), "Design", "design")
d %<>% addItem(MyGeneAnno, "contrastTest", "topTable")
print(d)
#test trap for overwriting item
d %<>% addItem(colData(RSE), "Design", "design")
print(d)

#test trap for adding 2nd instance of unique item
d %<>% addItem(assay(RSE, "Counts"), "Morecounts", "counts")

#force overwrite
d %<>% addItem(colData(RSE), "Design", "design", overwrite=T)
print(d)

dim(d)
#test rmItem
d %<>% rmItem("contrastTest")
print(d)
dim(d)
#try to remove a nonexistent item
d %<>% rmItem("xxx")
dim(d)
print(d)
d %<>% addItem(assay(RSE, "Counts"), "counts", "counts")
d %<>% addItem(colData(RSE), "Design", "col")
d %<>% addItem(MyGeneAnno, "geneAnnotation", "geneDat")
d %<>% addItem(MyGeneAnno, "contrastTest", "topTable")
print(d)

print(d, verbose=T)

showTypes(d)

myAnnotation <- getItem(d, "Design")
myCounts <- getItem(d, "counts")
print(d)

colnames(d)
rownames(d)


d %<>% addItem(assay(RSE, "Counts"), "TPM", "assay")

Items <- getType(d, "counts")
Items <- getType(d, c("assay", "counts", "design"))
Items <- getBaseType(d, "assay")
Items <- getItem (d, "counts")

d <- newType(d, "TPM", "assay", uniqueItem = TRUE)
showTypes(d)

d <- newType(d, "FPKM", "assay", uniqueItem = TRUE)

showTypes(d)

#can't get new types into the .DGEobjDef object definition

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


