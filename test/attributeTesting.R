# test attribute sparing subsetting
# (prevent subsetting from stripping attributes)
# Omit attributte sparing for granges
#
library(tidyverse)
library(magrittr)
library(DGEobj)
library(JRTutil)
stashPath <- getStashPath()
dgeobjPath <- "data/nonclin/DGEobj_library"
dgeObjName <- "UCSD_Lung_Fibroblasts_P-20171107-0002_8Feb2018.RDS"
dgeObj <- readRDS(file.path(stashPath, dgeobjPath, dgeObjName))

#capture the attributes
userAttrBeforeSubsetting <- getAttributes(dgeObj$Treatment_Disease)  ###

#subset the DGEobj
dim(dgeObj)
dsubset <- dgeObj[1:1000, 1:20, debug=FALSE]

# names(attributes(dgeObj$Treatment_Disease))
# names(attributes(dsubset$Treatment_Disease))
# #Now shows that the fixed attributes handling is working.
#
#
#
# # newAttrAfterSubsetting <- getAttributes(dsubset$Treatment_Disease)
# # mergedttributes <- do.call(c, list(newAttrAfterSubsetting, userAttrBeforeSubsetting))
#
# #grab the design matrix (see inventory(dgeObj))
# dm <- dsubset$Treatment_Disease
# dm <- setAttributes(dm, userAttrBeforeSubsetting)   ###
#
# names(attributes(dgeObj$Treatment_Disease))
# names(attributes(dm))
#
# #can I set attributes directly in a list subelement?
# dsubset$Treatment_Disease <- setAttributes(dsubset$Treatment_Disease, userAttrBeforeSubsetting)
# names(attributes(dsubset$Treatment_Disease) )
# #that works
#
# #now check all attributes before/after subsetting
#


anames <- list()
for (i in 1:length(dgeObj)){
    asubi <- names(attributes(dgeObj[[i]]))
    for (j in 1:length(asubi)){
        # x <- c(names(dgeObj)[i], names(attributes(dgeObj[[i]]))[j])
        anames[[str_c(names(dgeObj)[i],j, sep="_")]] <- names(attributes(dgeObj[[i]]))[j]
    }
}
anames <- bind_cols(anames) %>% t() %>%
    as.data.frame() %>%
    rownames_to_column(var="Item") %>%
    set_colnames(c("Item", "AttributeName")) %>%
    arrange(Item, AttributeName)
openxlsx::write.xlsx(anames, "anames.xlsx", rowNames=FALSE)

bnames <- list()
for (i in 1:length(dsubset)){
    asubi <- names(attributes(dsubset[[i]]))
    for (j in 1:length(asubi)){
        # x <- c(names(dgeObj)[i], names(attributes(dgeObj[[i]]))[j])
        bnames[[str_c(names(dsubset)[i],j, sep="_")]] <- names(attributes(dsubset[[i]]))[j]
    }
}
bnames <- bind_cols(bnames) %>% t() %>%
    as.data.frame() %>%
    rownames_to_column(var="Item") %>%
    set_colnames(c("Item", "AttributeName")) %>%
    arrange(Item, AttributeName)
openxlsx::write.xlsx(bnames, "bnames.xlsx", rowNames=FALSE)

#all attribute names are present after subsetting now

