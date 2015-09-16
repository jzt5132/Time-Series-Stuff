library(vars)
library(urca)


#read data
data=read.csv(file="CPI goods prices 4.csv")
data=ts(data,start=1990,frequency=12)
name=colnames(data)
#Change the column name
a=matrix(data=NA,nrow=260,ncol=25)
for (i in 1:25)
{a[,i]=data[,(i+1)]}
colnames(a)=c(1:25)
a=ts(a,start=1990,frequency=12)

#plot data
plot(a[,1:10],nc=2,xlab="")
plot(a[,11:20],nc=2,xlab="")
plot(a[,21:25],nc=2,xlab="")
ts.plot(a)

#determine I(.)
adf=matrix(data=NA,nrow=25,ncol=8)
for (i in 1:25) {
adf1 <- summary(ur.df(a[,i], type = "trend", lags =trunc((length(a[,i])-1)^(1/3))))
adf2 <- summary(ur.df(diff(a[, i]), type = "drift", lags = trunc((length(a[,i])-1)^(1/3))-1-1))
adf[i,1]=adf1@teststat[1]
adf[i,2:4]=adf1@cval[1,]
adf[i,5]=adf2@teststat[1]
adf[i,6:8]=adf2@cval[1,]}
adf



a.I1=a[,-c(10,15,18)]
VARselect(a.I1, lag.max =5, type = "both")
p1ct <- VAR(a.I1, p = 5, type = "both")
summary(p1ct, equation = "X1")
plot(p1ct, names = "X1")

