library(magrittr)
library(tidyverse)
library(DGEobj)
library(DGE.Tools2)
library(JRTutil)
library(conflicted)

setwd("~/R/lib/pkgsrc/DGEobj")

datadir <- "C:/Users/thompj27/Documents/Proteomics"

ptxAnnotation <- readRDS(file.path(datadir, "protAnno4DGE.rds"))
ptxAssay <- readRDS(file.path(datadir,"protData4DGE.rds"))
Design <- readRDS(file.path(datadir,"protDOE4DGE.rds"))
ptmAnnotation <- readRDS(file.path(datadir,"ptmAnno4DGE.rds"))
ptmAssay <- readRDS(file.path(datadir,"ptmData4DGE.rds"))

# Fix row and colname;  this is scrambling the data so don't use this data for anything but testing.
ptxAnnotation %<>% mutate(rowname = str_replace_all(Protein.IDs, ";", "_")) %>%
  column_to_rownames()
rownames(ptxAssay) <- rownames(ptxAnnotation)

rownames(Design) <- Design$Samplelabel
colnames(ptxAssay) <- rownames(Design)

#now ptxAnnotation, Design, and ptxAssay should be ready to load.

dgeObj <- initDGEobj_ptx(intensity=ptxAssay,
                         rowData=ptxAnnotation,
                         colData=Design,
                         level="peptide",
                         debug=FALSE
                             )



inventory(dgeObj)
