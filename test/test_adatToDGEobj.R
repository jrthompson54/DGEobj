#test adatToDGEobj
setwd("Z:/SomaDat/DS-316/derived/TOPCAT_V4-18-079.2018-12-05.adats")
library(magrittr)
library(tidyverse)
library(DGEobj)
library(DGE.Tools2)
library(JRTutil)
library(SummarizedExperiment)
inputPath <- "./input"
outputPath <- "./output"
adatFile <- "V4-18-079.hybNorm.medNormInt.plateScale.calibrate.medNormRef.qcCheck.medNormRefSMP.adat"

#Stash folder
#/stash/data/clin/external-collaboration/DS-316/sd
SomaObj <- adatToDGEobj(adatFile)
