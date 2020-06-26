#investigate problems calculating tpm from raw xpress count data

filepath <- "s:/data/nonclin/DGEobj_library/RNAseq_analysis_on_STING_agonists_induced_gene_signature_in_Human_PBMC.RDS"
library(magrittr)
library(tidyverse)
library(DGEobj)
library(DGE.Tools2)
library(JRTutil)
dgeObj <- getRDSobjFromStash(filepath)
dgeObj_orig <- resetDGEobj(dgeObj)

#Filter 0 length genes
idx <- dgeObj$geneData$ExonLength == 0
dgeobj_orig <- dgeObj_orig[idx,]

dgeObj_filt <- lowIntFilter(dgeobj_orig, fpkThreshold = 5, countThreshold = 10)
