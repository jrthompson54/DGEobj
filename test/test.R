library(DGEobj)
library(DGE.Tools2)
library(JRTutil)
library(Xpress2R)

xid = 21192

d <- Xpress2DGEO(xid, level="GRCm38ERCC-ensembl91-genes")
d <- Xpress2DGEO(xid, level="GRCm38ERCC-ensembl91-genes",
                 doeVersion="2f4a0f0a7ca8d3b816e47e102e1f11cb")
View(d$geneData)

library(rXpress)

#retrieve counts (attribute 205)
xpressData <- rXpress::getXpressData(xid, attribute=205)
varSetNames <- rXpress::getVarSetNames(xid)
Level <- varSetNames[1]  #1 for gene, 2 for isoforms
MyGeneData <- xpressData$varSets[[Level]]$z


pname <- "FPR2_Macrophage-Fibroblast_Crosstalk_P-20190111-0006_22Mar2019"
dgeObj <- OmicsoftToDgeObj(pname)

pid <- "P-20190111-0006"

mountpoint <- "y:"
dgeObj <- buildOmicsoftDGEobj(projectName = pname, mountPoint=mountpoint)
unique(dgeObj$design$ReplicateGroup)
