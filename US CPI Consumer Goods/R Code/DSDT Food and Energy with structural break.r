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
###########Import Data##################################################
data=read.csv(file="CPI goods prices 4.csv")
data=ts(data,start=c(1990,1),end=c(2011,8),frequency=12)
name=colnames(data)
length(name)
data=log(data)

###########Change the column name########################################
a=matrix(data=NA,nrow=260,ncol=25)
for (i in 1:25)
{a[,i]=data[,(i+1)]}
colnames(a)=c(1:25)
a=ts(a,start=c(1990,1),end=c(2011,8),frequency=12)

##############Energy and Food#############################
a.fe=a[,c(2,4,8,10,14,19,21,22,23,24,25)]

##############Cointegration with trend/seasonal###############################
ci=matrix(data=NA, nrow=22, ncol=44)
for (i in 1:10) { for (j in (i+1):11){
c=ca.jo(a.fe[,c(i,j)], type = "trace", ecdet = "trend",spec = "longrun",season=12)

ci[(2*i-1),(4*j-3)]=summary(c)@teststat[1]
ci[2*i,4*j-3]=summary(c)@teststat[2]
ci[2*i-1,4*j-2]=summary(c)@cval[1]
ci[2*i,4*j-2]=summary(c)@cval[2]
ci[2*i-1,4*j-1]=summary(c)@cval[3]
ci[2*i,4*j-1]=summary(c)@cval[4]
ci[2*i-1,4*j]=summary(c)@cval[5]
ci[2*i,4*j]=summary(c)@cval[6]

}}

for (j in 1:10) { for (i in (j+1):11){
c=ca.jo(a.fe[,c(i,j)], type = "trace", ecdet = "trend",spec = "longrun",season=12)

ci[(2*i-1),(4*j-3)]=summary(c)@teststat[1]
ci[2*i,4*j-3]=summary(c)@teststat[2]
ci[2*i-1,4*j-2]=summary(c)@cval[1]
ci[2*i,4*j-2]=summary(c)@cval[2]
ci[2*i-1,4*j-1]=summary(c)@cval[3]
ci[2*i,4*j-1]=summary(c)@cval[4]
ci[2*i-1,4*j]=summary(c)@cval[5]
ci[2*i,4*j]=summary(c)@cval[6]

}}

