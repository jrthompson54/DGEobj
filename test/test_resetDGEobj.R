# test resetDGEobj
# RNA-Seq_Analysis_of_FL_IBD_Human_Biopsy_P-20170717-0001.RDS
library(magrittr)
library(tidyverse)
library(DGEobj)
library(JRTutil)
library(DGE.Tools2)

dgeObj <- getRDSobjFromStash("RNA-Seq_Analysis_of_FL_IBD_Human_Biopsy_P-20170717-0001.RDS")
dim(dgeObj)

d <- resetDGEobj(dgeObj)
dim(d)
inventory(d)
