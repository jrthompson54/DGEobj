#test annotateDGEobj
#
library(DGEobj)
dgeObj <- readRDS("../DGEobj.RDS")
names(attributes(dgeObj))

d <- annotateDGEobj(dgeObj, "./test/regfileTest.txt")
names(attributes(d))

#Looks like it's erading the preexisting attributes'
