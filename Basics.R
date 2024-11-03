## load data
library("fBasics")
data("DowJones30")
x = DowJones30[,2:31]
## Compute correlation matrix
R = cor(x)
r <- eigen(R)
V <- r$vectors
lam <- r$values ##R = V lam V^-1
