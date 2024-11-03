## load data
library("fBasics")
data("DowJones30")
x <- DowJones30[, 2:31]
## Compute correlation matrix
c <- cor(x)
r <- eigen(c)
gam <- r$vectors
del <- r$values ##c = gam del gam^-1, Note: del is row vector of eigenvalues
## Create new correlation matrix
eps <- 0.1
del[1] <- del[1] + eps
del[1]
cstar <- gam