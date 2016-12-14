RSE <- readRDS("RSE.RDS")
library(magrittr)
library(SummarizedExperiment)
library(DGEresult)
d <- DGEresult()
d %<>% addItem(assay(RSE, "Counts"), "Counts", "assay")
d %<>% addItem(mcols(RSE), "GeneAnnotation", "row")
d %<>% addItem(colData(RSE), "SampAnnotation", "col")
d %<>% addItem(mcols(RSE), "contrastTest", "contrastTop")

#test trap for overwriting item
d %<>% addItem(colData(RSE), "SampAnnotation", "col")
#force overwrite
d %<>% addItem(colData(RSE), "SampAnnotation", "col", overwrite=T)

dim(d)
#test rmItem
d %<>% rmItem("Counts")
dim(d)

d %<>% rmItem("GeneAnnotation")
dim(d)

d %<>% addItem(assay(RSE, "Counts"), "Counts", "assay")
df <- print(d)
df <- print(d, verbose=T)
rownames(df)<-NULL
print(df)

