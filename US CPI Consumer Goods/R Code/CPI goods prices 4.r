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
a1=matrix(data=NA,nrow=260,ncol=25)
for (i in 1:25)
{a1[,i]=data[,(i+1)]}
colnames(a1)=c(1:25)
a=a1

#SAMPLE
data(Canada)
var1=VAR(Canada, p = 2, type = "const")
var2=VAR(Canada[,c(1:2,4)],p=2,type = "const")
SSR1=summary(var1)[2]$varresult$U[6]$sigma
SSR2=summary(var2)[2]$varresult$U[6]$sigma
f=((SSR2-SSR1)/2)/(SSR1/73)
p.value <- 1-pf(f,73,2)



#Vector AR

sink("myfile.txt", append=FALSE,split=TRUE)
{
b=matrix(data=NA,nrow=126,ncol=25)
var1=VARselect(diff(a),lag.max = 5)
v=VAR(diff(a),p=var1$selection[1])
b[,1]=summary(v$varresult$X1)$coef[,4]
b[,2]=summary(v$varresult$X2)$coef[,4]
b[,3]=summary(v$varresult$X3)$coef[,4]
b[,4]=summary(v$varresult$X4)$coef[,4]
b[,5]=summary(v$varresult$X5)$coef[,4]
b[,6]=summary(v$varresult$X6)$coef[,4]
b[,7]=summary(v$varresult$X7)$coef[,4]
b[,8]=summary(v$varresult$X8)$coef[,4]
b[,9]=summary(v$varresult$X9)$coef[,4]
b[,10]=summary(v$varresult$X10)$coef[,4]
b[,11]=summary(v$varresult$X11)$coef[,4]
b[,12]=summary(v$varresult$X12)$coef[,4]
b[,13]=summary(v$varresult$X13)$coef[,4]
b[,14]=summary(v$varresult$X14)$coef[,4]
b[,15]=summary(v$varresult$X15)$coef[,4]
b[,16]=summary(v$varresult$X16)$coef[,4]
b[,17]=summary(v$varresult$X17)$coef[,4]
b[,18]=summary(v$varresult$X18)$coef[,4]
b[,19]=summary(v$varresult$X19)$coef[,4]
b[,20]=summary(v$varresult$X20)$coef[,4]
b[,21]=summary(v$varresult$X21)$coef[,4]
b[,22]=summary(v$varresult$X22)$coef[,4]
b[,23]=summary(v$varresult$X23)$coef[,4]
b[,24]=summary(v$varresult$X24)$coef[,4]
b[,25]=summary(v$varresult$X25)$coef[,4]
b
}
#VAR2
c=matrix(data=NA,ncol=21,nrow=4)

for (j in 22:25) {
v1=VAR(diff(a), p = 1, type = "none")
v2=VAR(diff(a[,-j]), p = 1, type = "none")
SSR1=((summary(v1)[2]$varresult$X8[6]$sigma)^2)*233
SSR2=((summary(v2)[2]$varresult$X8[6]$sigma)^2)*234
f=(233*(SSR2-SSR1))/SSR1
p=pf(f,233,1)
c[j-21,8]=p
a=a1
}

for (j in 22:25) {
v1=VAR(diff(a), p = 1, type = "none")
v2=VAR(diff(a[,-j]), p = 1, type = "none")
SSR1=((summary(v1)[2]$varresult$X8[6]$sigma)^2)*233
SSR2=((summary(v2)[2]$varresult$X8[6]$sigma)^2)*234
f=(233*(SSR2-SSR1))/SSR1
p=pf(f,233,1)
c[j-21,8]=p
a=a1
}

for (j in 22:25) {
v1=VAR(diff(a), p = 1, type = "none")
v2=VAR(diff(a[,-j]), p = 1, type = "none")
SSR1=((summary(v1)[2]$varresult$X9[6]$sigma)^2)*233
SSR2=((summary(v2)[2]$varresult$X9[6]$sigma)^2)*234
f=(233*(SSR2-SSR1))/SSR1
p=pf(f,233,1)
c[j-21,9]=p
a=a1
}

for (j in 22:25) {
v1=VAR(diff(a), p = 1, type = "none")
v2=VAR(diff(a[,-j]), p = 1, type = "none")
SSR1=((summary(v1)[2]$varresult$X10[6]$sigma)^2)*233
SSR2=((summary(v2)[2]$varresult$X10[6]$sigma)^2)*234
f=(233*(SSR2-SSR1))/SSR1
p=pf(f,233,1)
c[j-21,10]=p
a=a1
}

for (j in 22:25) {
v1=VAR(diff(a), p = 1, type = "none")
v2=VAR(diff(a[,-j]), p = 1, type = "none")
SSR1=((summary(v1)[2]$varresult$X11[6]$sigma)^2)*233
SSR2=((summary(v2)[2]$varresult$X11[6]$sigma)^2)*234
f=(233*(SSR2-SSR1))/SSR1
p=pf(f,233,1)
c[j-21,11]=p
a=a1
}

for (j in 22:25) {
v1=VAR(diff(a), p = 1, type = "none")
v2=VAR(diff(a[,-j]), p = 1, type = "none")
SSR1=((summary(v1)[2]$varresult$X12[6]$sigma)^2)*233
SSR2=((summary(v2)[2]$varresult$X12[6]$sigma)^2)*234
f=(233*(SSR2-SSR1))/SSR1
p=pf(f,233,1)
c[j-21,12]=p
a=a1
}

for (j in 22:25) {
v1=VAR(diff(a), p = 1, type = "none")
v2=VAR(diff(a[,-j]), p = 1, type = "none")
SSR1=((summary(v1)[2]$varresult$X13[6]$sigma)^2)*233
SSR2=((summary(v2)[2]$varresult$X13[6]$sigma)^2)*234
f=(233*(SSR2-SSR1))/SSR1
p=pf(f,233,1)
c[j-21,13]=p
a=a1
}

for (j in 22:25) {
v1=VAR(diff(a), p = 1, type = "none")
v2=VAR(diff(a[,-j]), p = 1, type = "none")
SSR1=((summary(v1)[2]$varresult$X14[6]$sigma)^2)*233
SSR2=((summary(v2)[2]$varresult$X14[6]$sigma)^2)*234
f=(233*(SSR2-SSR1))/SSR1
p=pf(f,233,1)
c[j-21,14]=p
a=a1
}

for (j in 22:25) {
v1=VAR(diff(a), p = 1, type = "none")
v2=VAR(diff(a[,-j]), p = 1, type = "none")
SSR1=((summary(v1)[2]$varresult$X15[6]$sigma)^2)*233
SSR2=((summary(v2)[2]$varresult$X15[6]$sigma)^2)*234
f=(233*(SSR2-SSR1))/SSR1
p=pf(f,233,1)
c[j-21,15]=p
a=a1
}

for (j in 22:25) {
v1=VAR(diff(a), p = 1, type = "none")
v2=VAR(diff(a[,-j]), p = 1, type = "none")
SSR1=((summary(v1)[2]$varresult$X16[6]$sigma)^2)*233
SSR2=((summary(v2)[2]$varresult$X16[6]$sigma)^2)*234
f=(233*(SSR2-SSR1))/SSR1
p=pf(f,233,1)
c[j-21,16]=p
a=a1
}

for (j in 22:25) {
v1=VAR(diff(a), p = 1, type = "none")
v2=VAR(diff(a[,-j]), p = 1, type = "none")
SSR1=((summary(v1)[2]$varresult$X17[6]$sigma)^2)*233
SSR2=((summary(v2)[2]$varresult$X17[6]$sigma)^2)*234
f=(233*(SSR2-SSR1))/SSR1
p=pf(f,233,1)
c[j-21,17]=p
a=a1
}

for (j in 22:25) {
v1=VAR(diff(a), p = 1, type = "none")
v2=VAR(diff(a[,-j]), p = 1, type = "none")
SSR1=((summary(v1)[2]$varresult$X18[6]$sigma)^2)*233
SSR2=((summary(v2)[2]$varresult$X18[6]$sigma)^2)*234
f=(233*(SSR2-SSR1))/SSR1
p=pf(f,233,1)
c[j-21,18]=p
a=a1
}

for (j in 22:25) {
v1=VAR(diff(a), p = 1, type = "none")
v2=VAR(diff(a[,-j]), p = 1, type = "none")
SSR1=((summary(v1)[2]$varresult$X19[6]$sigma)^2)*233
SSR2=((summary(v2)[2]$varresult$X19[6]$sigma)^2)*234
f=(233*(SSR2-SSR1))/SSR1
p=pf(f,233,1)
c[j-21,19]=p
a=a1
}

for (j in 22:25) {
v1=VAR(diff(a), p = 1, type = "none")
v2=VAR(diff(a[,-j]), p = 1, type = "none")
SSR1=((summary(v1)[2]$varresult$X20[6]$sigma)^2)*233
SSR2=((summary(v2)[2]$varresult$X20[6]$sigma)^2)*234
f=(233*(SSR2-SSR1))/SSR1
p=pf(f,233,1)
c[j-21,20]=p
a=a1
}

for (j in 22:25) {
v1=VAR(diff(a), p = 1, type = "none")
v2=VAR(diff(a[,-j]), p = 1, type = "none")
SSR1=((summary(v1)[2]$varresult$X21[6]$sigma)^2)*233
SSR2=((summary(v2)[2]$varresult$X21[6]$sigma)^2)*234
f=(233*(SSR2-SSR1))/SSR1
p=pf(f,233,1)
c[j-21,21]=p
a=a1
}