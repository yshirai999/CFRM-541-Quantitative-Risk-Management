#Restart session
rm(list = ls())
shell("cls")
## H2Q3 Comparing VaR for normal model with historically simulated

## Load libraries and data
library(data.table) # recall to run install.packages("data.table")
path <- "C://Users//yoshi//OneDrive//Desktop
//Teaching//CFRM541 QRM//QRM_Git//QRM//H2//USRiskFreeRatesWeekly.csv"
data <- fread(path, header = TRUE)
dates <- as.Date(as.matrix(data[, 1]))

## Question 1
t <- as.Date("2010-01-29")
t <- which(abs(dates - t) == min(abs(dates - t)))
r <- as.matrix(data[t, 2:ncol(data)])
r1 <- r[, "WGS1YR"]
r10 <- r[, "WGS10YR"]
r20 <- r[, "WGS20YR"]
r_pi <- c(1 - r1 / 100, 1 - r10 * 10 / 100, 1 - r20 * 20 / 100)
f <- 1000000
pi <- f * (exp(-r1 / 100) + exp(-r10 * 10 / 100) + exp(-r20 * 20 / 100))
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
gam3 <- gam3[c("WGS1YR", "WGS10YR", "WGS20YR"), ]
# VaR

mu_dr_pi <- c(mu_dr["WGS1YR"] / 100,
              mu_dr["WGS10YR"] * 10 / 100, mu_dr["WGS20YR"] * 20 / 100)
mu_pi <- -f * sum(mu_dr_pi)
sig_dr_pi <- diag(c(sig_dr["WGS1YR"], sig_dr["WGS10YR"], sig_dr["WGS20YR"]))
sig_pc <- (del[1:3])^2 # standard deviation of principal components
sig_pi_1 <- sweep(t(gam3), 1, sig_dr_pi, "*")
sig_pi <- t(sweep(t(sig_pi_1), 1, sig_pc, "*"))
sig_pi <- sqrt(f^2 * sum(sig_pi %*% t(sig_pi_1)))
alpha <- 1 - 2^seq(log(1 - 0.001, base = 2), -10, length.out = 256)
dg3 <- t(sweep(t(gam3), 1, del3, "*"))
dgd3 <- dg3 %*% t(gam3)
var_n <- mu_pi + VaR_t(alpha, scale = sig_pi, df = Inf)
es_n  <- mu_pi + ES_t(alpha, scale = sig_pi, df = Inf)

## Question 3
# Historical Simulation
r <- as.matrix(data[t0:t1, 2:ncol(data)])
r <- r[, c("WGS1YR", "WGS10YR", "WGS20YR")]
dr <- (r[2:n, ] - r[1:n - 1, ])
loss <- -f * rowSums(dr * c(1 / 100, 10 / 100, 20 / 100))
loss_sorted <- sort(loss)
loss_cdf <- seq_along(loss_sorted) / length(loss_sorted)
# Plot
# For multiple plots in vscode, activate httpgd in Files > Pref > Settings > R
plot(loss_sorted, loss_cdf, type = "s")
abline(h = 0.95, lty = 3)
var_hs <- loss_sorted[which(loss_cdf >= alpha[1])[1]]
for (a in alpha[2:length(alpha)]) {
  var_hs <- c(var_hs, loss_sorted[which(loss_cdf >= a)[1]])
}

#Plot
plot(1 - alpha, es_n, type = "l", ylim = range(var_n, es_n, var_hs),
     log = "x", col = "maroon3", xlab = expression(1 - alpha), ylab = "")
lines(1 - alpha, var_n, col = "black")
lines(1 - alpha, var_hs, col = "yellow")
legend("topright", bty = "n", lty = rep(1, 3),
       col = c("maroon3", "black", "yellow"),
       legend = c(expression(ES[alpha] ~ ~ "for normal model"),
                  expression(VaR[alpha] ~ ~ "for normal model"),
                  expression(VaR[alpha] ~ ~ "for historical simulation")))
