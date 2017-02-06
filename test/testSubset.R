library(DGEobj)
d <- readRDS("../DGEobj.RDS")

dd <- subset(d, 1:1000, 1:10)

ddd <- as.list(dd)
n <- names(ddd)
for (i in 1:length(ddd)){
    i
    print(n[[i]])
    print(dim(ddd[[i]]))
}
