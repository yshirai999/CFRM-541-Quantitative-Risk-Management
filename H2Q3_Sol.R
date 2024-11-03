#Restart session
rm(list = ls())
shell("cls")

## H2Q2 Stressing a correlation matrix

## Load libraries and data
library(data.table) # recall to run install.packages("data.table")
data <- fread("USRiskFreeRatesWeekly.csv", header = TRUE)
r <- as.matrix(data[, 2:ncol(data)])
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
