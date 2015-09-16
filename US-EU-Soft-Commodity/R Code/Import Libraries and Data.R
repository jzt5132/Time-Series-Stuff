library(tseries)
library(timeSeries)
library(stats)
library(forecast)
library(MSBVAR)
library(vars)
library(urca)
library(strucchange)
library(pastecs)
library(deseasonalize)
library(fGarch)
library(tsDyn)
library(apt)
library(doSNOW)

###Import Data###

data <- read.csv(file="data1.csv", header=T)
data <- ts(data[,(-1)])
name <- colnames(data)
data <- log(data)

