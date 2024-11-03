## load data
library("fBasics")
data("DowJones30")
x = DowJones30[,2:31]
## Compute correlation matrix
R = cor(x)
r <- eigen(R)
Gam <- r$vectors
Del <- r$values ##R = Gam Del Gam^-1, Note: Del is row vector of eigenvalues
## Create new correlation matrix
eps <- 0.1
Del[1] = Del[1]+eps
Del[1]
Rstar = Gams