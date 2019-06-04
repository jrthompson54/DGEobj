# test attribute sparing subsetting
# (prevent subsetting from stripping attributes)
# Omit attributte sparing for granges
#

library(magrittr)
library(tidyverse)
library(DGEobj)
library(DGE.Tools2)
library(JRTutil)

stashPath <- getStashPath()
dgeobjPath <- "data/nonclin/DGEobj_library"
dgeObjName <- "UCSD_Lung_Fibroblasts_P-20171107-0002_8Feb2018.RDS"
dgeObj <- readRDS(file.path(stashPath, dgeobjPath, dgeObjName))

#capture the attributes
userAttrBeforeSubsetting <- getAttributes(dgeObj$Treatment_Disease)  ###



