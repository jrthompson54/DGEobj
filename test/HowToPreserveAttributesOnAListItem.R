# example to test if attributes on items inside a list survive
a = 1
#assign an attribute
attr(a,"test") <- "avalue"

#retrieve the attribute
attr(a, "test")  #attribute is there

#put a into list
t1 <- list()
t1$a <- a
#check for the attribute on var a within the list
attr(t1$a, "test") #Attribute value returned
attr(t1[[1]], "test") #Attribute value returned

class(t1)
length(t1)
t1
# Test1 works as expected. The var a element retains in attribute when embedded in a list.


#### test2
attr(a, "test")

t2 <- list()
t2["a"] <- a

attr(t2$a, "test")
attr(t2[[1]], "test")
#test attribute is NULL!
names(t2)
class(t2)
length(t2)
t2
# Names, class and length match but attribue is gone

#### test3
attr(a, "test")

t3 <- list()
t3[["a"]] <- a

attr(t3$a, "test") #Attribute value returned
attr(t3[[1]], "test") #Attribute value returned
#test attribute is NULL!
names(t3)
class(t3)
length(t3)
t3



names(t1)
class(t1)
length(t1)
t1

names(t2)
class(t2)
length(t2)
t2

names(t3)
class(t3)
length(t3)
t3

#Both test1 and test3 work.  Test3 is used in addItem

# Hypothesis: attributes may be stripped when subsetting
# Check and see if an unfiltered DGEobj still has the formula
#
stashPath <- getStashPath()
dgeobjPath <- "data/nonclin/DGEobj_library"
dgeObjName <- "UCSD_Lung_Fibroblasts_P-20171107-0002_8Feb2018.RDS"
dgeObj <- readRDS(file.path(stashPath, dgeobjPath, dgeObjName))

names(attributes(dgeObj$Treatment_Disease))
#formula is there
attr(dgeObj$Treatment_Disease, "formula")
#formula value retrieved

#subset the DGEobj and check again
d_filtered <- dgeObj[1:1000,]
names(attributes(d_filtered$Treatment_Disease))
#formula is now missing
attr(d_filtered$Treatment_Disease, "formula")
#so returns NULL



#### Check attributes in all items


anames <- list()
for (i in 1:length(dgeObj)){
  anames[[names(dgeObj)[i]]] <- names(attributes(dgeObj[[i]]))
}

bnames <- list()
for (i in 1:length(d_filtered)){
  bnames[[names(d_filtered)[i]]] <- names(attributes(d_filtered[[i]]))
}


anames <- list()
for (i in 1:length(dgeObj)){
  asubi <- names(attributes(dgeObj[[i]]))
  for (j in 1:length(asubi)){
    # x <- c(names(dgeObj)[i], names(attributes(dgeObj[[i]]))[j])
    anames[[str_c(names(dgeObj)[i],j, sep="_")]] <- names(attributes(dgeObj[[i]]))[j]
  }
}
anames <- bind_cols(anames) %>% t()
openxlsx::write.xlsx(anames, "anames.xlsx", rowNames=TRUE)

bnames <- list()
for (i in 1:length(d_filtered)){
  asubi <- names(attributes(d_filtered[[i]]))
  for (j in 1:length(asubi)){
    # x <- c(names(dgeObj)[i], names(attributes(dgeObj[[i]]))[j])
    bnames[[str_c(names(d_filtered)[i],j, sep="_")]] <- names(attributes(d_filtered[[i]]))[j]
  }
}
bnames <- bind_cols(bnames) %>% t()
openxlsx::write.xlsx(bnames, "bnames.xlsx", rowNames=TRUE)
