RSE <- readRDS("RSE.RDS")
library(magrittr)
library(SummarizedExperiment)
library(DGEresult)
d <- DGEresult()
d %<>% addItem(assay(RSE, "Counts"), "counts", "counts")
d %<>% addItem(mcols(RSE), "GeneInfo", "geneAnno")
d %<>% addItem(colData(RSE), "Design", "design")
d %<>% addItem(mcols(RSE), "contrastTest", "topTable")
print(d)

d %<>% rmItem("Design")
d %<>% rmItem("contrastTest")
print(d)

d %<>% addItem(colData(RSE), "Design", "design")
d %<>% addItem(mcols(RSE), "contrastTest", "topTable")
print(d)
#test trap for overwriting item
d %<>% addItem(colData(RSE), "Design", "col")
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
d %<>% addItem(assay(RSE, "Counts"), "counts", "assay")
d %<>% addItem(colData(RSE), "Design", "col")
d %<>% addItem(mcols(RSE), "geneAnnotation", "geneAnno")
print(d)

print(d, verbose=T)

showTypes()

myAnnotation <- getItem(d, "Design")
myCounts <- getItem(d, "counts")
print(d)

colnames(d) # colnames not working
rownames(d) #not working

d %<>% addItem(assay(RSE, "Counts"), "TPM", "assay")

Items <- getType(d, "assay")
Items <- getBaseType(d, "assay")  #not Working

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


