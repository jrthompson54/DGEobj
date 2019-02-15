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

names(attributes(dgeObj$Treatment_Disease))
names(attributes(dsubset$Treatment_Disease))
#Now shows that the fixed attributes handling is working.



# newAttrAfterSubsetting <- getAttributes(dsubset$Treatment_Disease)
# mergedttributes <- do.call(c, list(newAttrAfterSubsetting, userAttrBeforeSubsetting))

dm <- dsubset$Treatment_Disease
dm <- setAttributes(dm, userAttrBeforeSubsetting)   ###

names(attributes(dgeObj$Treatment_Disease))
names(attributes(dm))

#can I set attributes directly in a list subelement?
dsubset$Treatment_Disease <- setAttributes(dsubset$Treatment_Disease, userAttrBeforeSubsetting)
names(attributes(dsubset$Treatment_Disease) )
#that works

#replace attribs with merged attributes
attributes(newdm) <- mergedttributes
names(attributes(newdm))

#try setAttibutes
#

#assigning attributes strips existing attributes.
#
#protocol
#getAttributes before subsetting
#setAttrributes after subsetting
