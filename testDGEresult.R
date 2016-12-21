rm(list=ls())
RSE <- readRDS("RSE.RDS")
library(magrittr)
library(SummarizedExperiment)
library(DGEobj)
library(assertthat)

MyCounts <- assay(RSE, "Counts")
attr(MyCounts, "Algorithm") <- "RSEM"
MyGeneAnno <- mcols(RSE)
rownames(MyGeneAnno) <- MyGeneAnno$ID
Design <- colData(RSE)

d <- initDGEobj(MyCounts, Design, MyGeneAnno, "gene")
MyContrast <- MyGeneAnno
attr(MyContrast, "PValue") = 1
d %<>% addItem(MyGeneAnno, "contrastTest", "topTable", overwrite = T)

print(d)

dim(d)
print(d, verbose=T)

d %<>% rmItem("Design")
print(d)

d %<>% addItem(colData(RSE), "Design", "design")
print(d)

#test trap for overwriting item
d %<>% addItem(colData(RSE), "Design", "design")
print(d)

#test trap for adding 2nd instance of unique item
d %<>% addItem(assay(RSE, "Counts"), "Morecounts", "counts")

#force overwrite
d %<>% addItem(colData(RSE), "Design", "design", overwrite=T)
print(d)

#try to remove a nonexistent item
d %<>% rmItem("xxx")
dim(d)
print(d)
d %<>% addItem(assay(RSE, "Counts"), "counts", "counts")
d %<>% addItem(colData(RSE), "Design", "col")
d %<>% addItem(MyGeneAnno, "geneAnnotation", "geneData")
print(d)

print(d, verbose=T)

showTypes(d)

attributes(d)

length(d)

myAnnotation <- getItem(d, "Design")
myCounts <- getItem(d, "counts")
print(d)

colnames(d)
rownames(d)


d %<>% addItem(assay(RSE, "Counts"), "TPM", "TPM")

Items <- getType(d, "counts")
Items <- getType(d, c("assay", "counts", "design"))
Items <- getBaseType(d, "assay")
Items <- getItem (d, "counts")

d <- newType(d, "LogTPM", "assay", uniqueItem = TRUE)
showTypes(d)

head(rownames(d))
head(colnames(d))
nrow(d)
ncol(d)

# test renameing dimnames
shortcolnames <- Design$Barcode
lcgenenames <- tolower(rownames(counts))
dimnames(d) <- list(lcgenenames, shortcolnames)

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


