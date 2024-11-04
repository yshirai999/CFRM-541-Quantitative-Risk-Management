#Restart session
rm(list = ls())
shell("cls")

## H2Q2 Stressing a correlation matrix

## Load libraries and data
library(data.table) # recall to run install.packages("data.table")
data <- fread("USRiskFreeRatesWeekly.csv", header = TRUE)
dates <- as.Date(as.matrix(data[, 1]))

## Question 1
t <- as.Date("2010-01-29")
t <- which(abs(dates - t) == min(abs(dates - t)))
r <- as.matrix(data[t, 2:ncol(data)])
r1 <- r[, "WGS1YR"]
r10 <- r[, "WGS10YR"]
r20 <- r[, "WGS20YR"]
r_pi <- c(1 - r1 / 100, 1 - r10 / 100, 1 - r20 / 100)
f <- 1000000
pi <- f * (exp(-r1 * 1 / 100) + exp(-r1 * 10 / 100) + exp(-r1 * 20 / 100))
pi_approx <- f * (r_pi %*% c(1, 1, 1))
pi
pi_approx

## Question 2
# PCA
library("matrixStats")
t0 <- as.Date("2009-01-01") #initial date
t1 <- as.Date("2009-12-31") #final date
t0 <- which(abs(dates - t0) == min(abs(dates - t0))) # find row
t1 <- which(abs(dates - t1) == min(abs(dates - t1)))
r <- as.matrix(data[t0:t1, 2:ncol(data)])
n <- nrow(r)
dr <- r[2:n, ] - r[1:n - 1, ] #PCA performed on changes
mu_dr <- colMeans(dr)
sig_dr <- sqrt(colVars(dr))
y <- (dr - mu_dr) / sig_dr #normalized vectors of changes
set.seed(1) #set seed for reproducibility
res <- princomp(y, scale = FALSE)
del <- res$sdev
gam <- res$loadings
del3 <- del[1:3]
gam3 <- gam[, 1:3]
gam3 <- gam3[c("WGS1YR","WGS10YR","WGS20YR"),]
# VaR
library(qrmtools)
mu_dr_pi <- c(mu_dr["WGS1YR"], mu_dr["WGS10YR"], mu_dr["WGS20YR"])
mu_pi <- -f * sum(mu_dr_pi)
sig_dr_pi <- diag(c(sig_dr["WGS1YR"], sig_dr["WGS10YR"], sig_dr["WGS20YR"]))
sig_pc <- (del[1:3])^2 # standard deviation of principal components 
sig_pi_1 <- sweep(t(gam3), 1, sig_dr_pi, "*")
sig_pi <- t(sweep(t(sig_pi_1), 1, sig_pc, "*"))
sig_pi <- sqrt(f^2 * sum(sig_pi %*% t(sig_pi_1)))
alpha <- 1-2^seq(log(1-0.001, base = 2), -10, length.out = 256)
dg3 <- t(sweep(t(gam3), 1, del3, "*"))
dgd3 <- dg3 %*% t(gam3)
var_n <- mu_pi + VaR_t(alpha, scale = sig_pi, df = Inf)
es_n  <- mu_pi + ES_t(alpha, scale = sig_pi, df = Inf)
#Plot
plot(1 - alpha, es_n, type = "l", ylim = range(var_n, es_n), log = "x",
     col = "maroon3", xlab = expression(1 - alpha), ylab = "")
lines(1 - alpha, var_n, col = "black")
legend("topright", bty = "n", lty = rep(1, 2), col = c("maroon3", "black"),
       legend = c(expression(ES[alpha] ~ ~ "for normal model"),
                  expression(VaR[alpha] ~ ~ "for normal model")))

## Question 3
