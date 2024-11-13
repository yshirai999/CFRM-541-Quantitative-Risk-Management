#Restart session
rm(list = ls())
shell("cls")

## Load libraries and data
library("Ecdat") # recall to run install.packages("Ecdat")
data("SP500")
x <- SP500
pi0 <- 1000000
pi <- cumprod(1 + x / 100)
loss <- -pi[2:2780] + pi[1:2779]

## VaR by Historical Simulation
loss_sorted <- sort(loss)
loss_cdf <- seq_along(loss_sorted) / length(loss_sorted)
alpha <- 0.95
var_hs <- pi0 * loss_sorted[which(loss_cdf >= alpha)[1]]
var_hs

## VaR by fitting t distribution
library("MASS")
library(qrmtools)
start <- list(m = mean(loss), s = sd(loss), df = 5) #Parameters guess
lower <- c(mean(loss), sd(loss), 3) #Lower bound to the parameters
dt <- fitdistr(loss, "t", start = start, lower = lower)
estimate <- c(dt$estimate)
var_t <- estimate[1] * pi0 +
  pi0 * VaR_t(alpha, scale = estimate[2], df = estimate[3])
var_t

## Note that the VaR for the fitted t distribution is higher
## This is because the empirical distribution is capped by the
## maximum value observed.