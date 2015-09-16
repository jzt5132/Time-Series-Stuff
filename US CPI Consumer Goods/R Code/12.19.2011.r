library(tseries)
library(timeSeries)
library(stats)
library(forecast)
library(MSBVAR)
library(vars)
library(urca)
library(strucchange)
library(pastecs)

#Import Data
data=read.csv(file="CPI goods prices 4.csv")
data=ts(data,start=1990,frequency=12)
name=colnames(data)
length(name)
data[,8]=data[,8]/4
data=log(data)
#Change the column name
a=matrix(data=NA,nrow=260,ncol=25)
for (i in 1:25)
{a[,i]=data[,(i+1)]}
colnames(a)=c(1:25)
a=ts(a,start=1990,frequency=12)


##Break Point
bp=matrix(NA,ncol=6,nrow=25)
for (i in 1:25){for (j in 1:6){
bp.a=breakpoints(a[,i] ~ 1)
bp[i,j]=breakdates(bp.a)[j]
}}

###Test trend
tp1=matrix(NA,ncol=1,nrow=25)
for (i in 1:25) {
t=trend.test(a[,i],R=999)
tp1[i,1]=t$p.value}

tp2=matrix(NA,ncol=1,nrow=25)
for (i in 1:25) {
tr=decompose(a[,i])$trend
tr=matrix(tr)
tr=na.omit(tr)
t=c(1:248)
lr=lm(tr~1+t)
tp2[i,1]=summary(lr)$coefficients[8]}



###determine I(.)
adf=matrix(data=NA,nrow=25,ncol=8)
for (i in 1:25) {
adf1 <- summary(ur.df(a[,i], type = "trend", lags =trunc((length(a[,i])-1)^(1/3))))
adf2 <- summary(ur.df(diff(a[, i]), type = "drift", lags = trunc((length(a[,i])-1)^(1/3))-1-1))
adf[i,1]=adf1@teststat[1]
adf[i,2:4]=adf1@cval[1,]
adf[i,5]=adf2@teststat[1]
adf[i,6:8]=adf2@cval[1,]}
adf

c2.1=matrix(data=NA,nrow=25,ncol=75)

for (i in 1:25)
{ for (j in 1:25)
{
lm1=lm(a[,i]~a[,j])
s1.t=adf.test(summary(lm1)$residuals,alternative="stationary")
s2.t=kpss.test(summary(lm1)$residuals)
s3.t=pp.test(summary(lm1)$residuals,lshort=TRUE)
c2.1[j,i]=s1.t$p.value
c2.1[j,i+25]=s2.t$p.value
c2.1[j,i+50]=s3.t$p.value
}}
c2.1

#Cointegration
a.I1=a[,-c(10,14,15,16,17,18)]
VARselect(a.I1, lag.max =5, type = "both")
p1ct <- VAR(a.I1, p = 5, type = "both")
summary(p1ct, equation = "X1")
plot(p1ct, names = "X3")


a=summary(ca.jo(a.I1[,1:10], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=12))
b=summary(ca.jo(a.I1[,11:20], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=12))
c=summary(ca.jo(a.I1[,c(1:7,17:20)], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=12))
d=summary(ca.jo(a.I1[,10:20], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=12))
a@teststat
a@cval
b@teststat
b@cval
t(t(c@teststat))
c@cval
t(t(d@teststat))
d@cval


c2.2=matrix(data=NA,nrow=40,ncol=80)
for (i in 1:19){for (j in (i+1):20)
{c=summary(ca.jo(a.I1[,c(i,j)], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=12))
c2.2[(2*i-1):(2*i),(4*j-3)]=t(t(c@teststat))
c2.2[(2*i-1):(2*i),(4*j-2):(4*j)]=c@cval
}}
for (j in 1:19){for (i in (j+1):20)
{c=summary(ca.jo(a.I1[,c(i,j)], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=12))
c2.2[(2*i-1):(2*i),(4*j-3)]=t(t(c@teststat))
c2.2[(2*i-1):(2*i),(4*j-2):(4*j)]=c@cval
}}


c2.3=matrix(data=NA,nrow=40,ncol=20)
for (i in 1:19){for (j in (i+1):20)
{c=summary(ca.jo(a.I1[,c(i,j)], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=12))
c2.3[(2*i-1):(2*i),j]=t(t(c@teststat))
}}
for (j in 1:19){for (i in (j+1):20)
{c=summary(ca.jo(a.I1[,c(i,j)], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=12))
c2.3[(2*i-1):(2*i),j]=t(t(c@teststat))
}}


c2.4=matrix(data=NA,nrow=20,ncol=20)
for (i in 1:19){for (j in (i+1):20)
{p1=VARselect(diff(a.I1[,c(i,j)]),lag.max = 5)
c=summary(ca.jo(a.I1[,c(i,j)], type = "trace", ecdet = "trend", K =max(2,p1$selection[1]),spec = "transitory",season=12))
c2.4[i,j]=c@teststat[2]
}}
for (j in 1:19){for (i in (j+1):20)
{p1=VARselect(diff(a.I1[,c(i,j)]),lag.max = 5)
c=summary(ca.jo(a.I1[,c(i,j)], type = "trace", ecdet = "trend", K =max(2,p1$selection[1]),spec = "transitory",season=12))
c2.4[i,j]=c@teststat[2]
}}


vecm1=ca.jo(a.I1[,11:20], type = "trace",ecdet = "trend", K = 2, spec = "transitory",season=12)
jo.results=summary(vecm1)
vecm.r2=cajorls(vecm1,r=6)
class(jo.results)
alpha=coef(vecm.r2$rlm)[1,]
beta=vecm.r2$beta
resids=resid(vecm.r2$rlm)
N=nrow(resids)
sigma=crossprod(resids)/N
beta[-1]


v1=vec2var(vecm1, r =6)
SSR1=sum((v1$resi[,1])^2)
v2=vec2var(vecm2, r =5)
SSR2=sum((v2$resi[,1])^2)
f=((SSR2-SSR1)*238)/(SSR1*2)
p=pf(f,238,2)




#Causality
#On Bivariate VAR of the first difference
c2.5=matrix(data=NA,nrow=20,ncol=20)
for (i in 1:19){for (j in (i+1):20){
p1=VARselect(diff(a.I1[,c(i,j)]),lag.max = 5)
v=VAR(diff(a.I1[,c(i,j)]),p=p1$selection[1])
c2.5[i,j]=causality(v)$Granger$p.value
}}
for (j in 1:19){for (i in (j+1):20){
p1=VARselect(diff(a.I1[,c(i,j)]),lag.max = 5)
v=VAR(diff(a.I1[,c(i,j)]),p=p1$selection[1])
c2.5[i,j]=causality(v)$Granger$p.value
}}


c2.6=matrix(data=NA,nrow=20,ncol=20)
for (i in 1:19){for (j in (i+1):20){
v=VAR(diff(a.I1[,c(i,j)]),p=2)
c2.6[i,j]=causality(v)$Granger$p.value
}}
for (j in 1:19){for (i in (j+1):20){
v=VAR(diff(a.I1[,c(i,j)]),p=2)
c2.6[i,j]=causality(v)$Granger$p.value
}}

#On Bivariate Vecm
c2.7=matrix(data=NA,nrow=20,ncol=20)
c2.8=matrix(data=NA,nrow=20,ncol=20)
for (i in 1:19){ for (j in (i+1):20){
lr.reg=lm(a.I1[,j]~a.I1[,i])
error=residuals(lr.reg)
error.lagged=error[-c(259,260)]
dy1=diff(a.I1[,i])
dy2=diff(a.I1[,j])
diff.dat=data.frame(embed(cbind(dy1,dy2),2))
colnames(diff.dat)=c("dy1","dy2","dy1.1","dy2.1")
ecm.reg=lm(dy2~error.lagged+dy1.1+dy2.1,data=diff.dat)
c2.7[i,j]=summary(ecm.reg)$coefficients[14]
c2.8[i,j]=summary(ecm.reg)$coefficients[15]}}


for (j in 1:19){ for (i in (j+1):20){
lr.reg=lm(a.I1[,j]~a.I1[,i])
error=residuals(lr.reg)
error.lagged=error[-c(259,260)]
dy1=diff(a.I1[,i])
dy2=diff(a.I1[,j])
diff.dat=data.frame(embed(cbind(dy1,dy2),2))
colnames(diff.dat)=c("dy1","dy2","dy1.1","dy2.1")
ecm.reg=lm(dy2~error.lagged+dy1.1+dy2.1,data=diff.dat)
c2.7[i,j]=summary(ecm.reg)$coefficients[14]
c2.8[i,j]=summary(ecm.reg)$coefficients[15]}}

#############TRIAL
lr.reg=lm(a.I1[,1]~a.I1[,2])
error=residuals(lr.reg)
error.lagged=error[-c(259,260)]
dy1=diff(a.I1[,1])
dy2=diff(a.I1[,2])
diff.dat=data.frame(embed(cbind(dy1,dy2),3))
colnames(diff.dat)=c("dy1","dy2","dy1.1","dy2.1","dy3.1","dy3.2")
ecm.reg=lm(dy2~error.lagged+diff.dat[,1]+diff.dat[,2]+diff.dat[,3]+diff.dat[,4])
summary(ecm.reg)

