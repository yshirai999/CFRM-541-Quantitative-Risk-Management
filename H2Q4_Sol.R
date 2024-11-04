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
r
pi <- 1000000 / (1 + r[, "WGS1YR"] / 100)
pi <- pi + 1000000 / (1 + r[, "WGS10YR"] / 100)
pi <- pi + 1000000 / (1 + r[, "WGS20YR"] / 100)
pi

## Question 2

