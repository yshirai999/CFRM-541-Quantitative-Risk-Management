#Restart session
rm(list = ls())
shell("cls")

## H2Q2 Stressing a correlation matrix

## Load libraries and data
library("fBasics")
data("DowJones30")
x <- DowJones30[1:600, 2:31]
# (a) Compute spectral decomposition of the correlation matrix
c <- cor(x)
r <- eigen(c)
gam <- r$vectors
del <- r$values #c = gam del gam^-1, Note: del is row vector of eigenvalues

## (b) Perturb first eigenvalue
eps <- 10
del[1] <- del[1] + eps
del[1]

## (c) Define matrix with perturbed eigenvalue
# matrix multiplication is optimized given form of del
cstar <- t(sweep(t(gam), 1, del, "*"))
cstar <- cstar %*% t(gam)
# Check that eigenvalues of matrix cstar are indeed given by del
rstar <- eigen(cstar)
gamstar <- rstar$vectors
delstar <- rstar$values
all.equal(delstar, del)

## (d) Construct new correlation matrix cc from rstar
d <- diag(cstar)
d <- 1 / d
cc <- sweep(cstar, 1, d, "*")
cc <- t(sweep(t(cstar), 1, d, "*"))

## Comparing cc and c
# Average correlation increases (decreases) if eps>0 (eps<0)...
mean(cc) > mean(c)
# ...but not each pairwise correlation increases (decreases)
comp <- cc > c
sum(comp)
# In fact (try it) the larger |eps|, the smaller (larger) is sum(comp)
