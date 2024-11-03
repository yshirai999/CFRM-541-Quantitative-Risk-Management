#Restart session
rm(list = ls())
shell("cls")

## H2Q2 Stressing a correlation matrix

## Load libraries and data
library(data.table) # recall to run install.packages("data.table")
data <- fread("USRiskFreeRatesWeekly.csv", header = TRUE)
dates <- as.Date(as.matrix(data[, 1]))
t0 <- as.Date("2007-01-01")
t1 <- as.Date("2007-12-31")
t0 <- which(abs(dates - t0) == min(abs(dates - t0)))
t1 <- which(abs(dates - t1) == min(abs(dates - t1)))
r <- as.matrix(data[t0:t1, 2:ncol(data)])
n <- nrow(r)

## PCA
delta_r <- r[2:n, ] - r[1:n - 1, ]
set.seed(1)
res <- princomp(delta_r, scale = FALSE)
del <- res$sdev^2
gam <- res$loadings
# Check
c <- cov(delta_r)
gam_c <- eigen(c)$vector
del_c <- eigen(c)$values
# Explained variability
varexp <- cumsum(del) / sum(del)
varexp
