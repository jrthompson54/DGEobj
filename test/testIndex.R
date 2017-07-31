
library(magrittr)
library(assertthat)
library(DGEobj)
source('C:/Users/thompj27/Documents/R/lib/pkgsrc/DGEobj/R/index.R')

d <- readRDS("CedarSinai_TGFSignature_dgeObj.RDS")

x <- indexDGEobj(d)

y <- .list_to_dfrow(x)
