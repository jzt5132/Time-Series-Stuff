#Call libraries
library(tseries)
library(timeSeries)
library(stats)
library(forecast)
library(MSBVAR)
library(vars)

#Import Data
data=read.csv(file="CPI goods prices 4.csv")
data=ts(data,start=1990,frequency=12)
name=colnames(data)
length(name)

#Change the column name
a=matrix(data=NA,nrow=260,ncol=25)
for (i in 1:25)
{a[,i]=data[,(i+1)]}
colnames(a)=c(1:25)
a=ts(a,start=1990,frequency=12)

#Plot(Univariate)

#Stationarity Test

#Cointegration Test(Second Diff)

#Cbind Causality (Two Vectors)(Second Diff)



#Causality (Whole Vectors)(Second Diff)

#Structure VAR

