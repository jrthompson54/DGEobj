#test adatToDGEobj
setwd("Z:/SomaDat")
library(readat)
library(magrittr)
library(tidyverse)
library(DGEobj)
library(DGE.Tools2)
library(JRTutil)
library(SummarizedExperiment)
inputPath <- "./input"
outputPath <- "./output"
adatFile <- "V4-18-079.hybNorm.medNormInt.plateScale.calibrate.medNormRef.qcCheck.medNormRefSMP.adat"

