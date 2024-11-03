## load data
library("fBasics")
data("DowJones30")
x <- DowJones30[1:600, 2:31]
# Compute correlation matrix
c <- cor(x)
r <- eigen(c)
gam <- r$vectors
del <- r$values #c = gam del gam^-1, Note: del is row vector of eigenvalues
# Create new correlation matrix
eps <- -50
del[1] <- del[1] + eps
del[1]
# matrix multiplication is optimized given form of del
cstar <- t(sweep(t(gam), 1, del, "*"))
cstar <- cstar %*% t(gam)
# Check that rstar eigenvalues are indeed given by del
rstar <- eigen(cstar)
gamstar <- rstar$vectors
delstar <- rstar$values
all.equal(delstar, del)
# Construct correlation matrix from rstar
d <- diag(cstar)
d <- 1 / d
cc <- sweep(cstar, 1, d, "*")
cc <- t(sweep(t(cstar), 1, d, "*"))
# Comparing cc and c
# average correlation increases (decreases) if eps>0 (eps<0)...
mean(cc) > mean(c)
#but not each pairwise correlation increases (decreases)
comp <- cc > c
sum(comp)
# The larger the absolute change in lambda_1, the smaller (larger) is sum(comp)
rm(list = ls())
shell("cls")
