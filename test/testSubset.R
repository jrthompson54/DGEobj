library(DGEobj)
d <- readRDS("../DGEobj.RDS")

dd <- subset(d, 1:1000, 1:10)

ddd <- as.list(dd)
n <- names(ddd)
for (i in 1:length(ddd)){
    s <- paste(n[[i]],
            nrow(ddd[[i]]),
            ncol(ddd[[i]]),
            sep=" ")
    print(s)
    }
print(ddd)
