library(tseries)
library(timeSeries)
library(stats)
library(forecast)
library(MSBVAR)
library(vars)
library(urca)
library(strucchange)
library(pastecs)


###########Import Data##################################################
data=read.csv(file="CPI goods prices 4.csv")
data=ts(data,start=c(1990,1),end=c(2011,8),frequency=12)
name=colnames(data)
length(name)
data=log(data)


############univariate plot data########################################
pdf(file="c:\\Univariate Plot.pdf")
for (i in 2:26)
{
plot(data[,i],xlab="Date",ylab=name[i],main=name[i],
font.main=2,xaxs="i",cex.sub=.6,font.sub=4,
sub="Source:BLS All US Cities CPI Average Price http://data.bls.gov/pdq/querytool.jsp?survey=ap Monthly Average")
}
dev.off()

###########Change the column name########################################
a=matrix(data=NA,nrow=260,ncol=25)
for (i in 1:25)
{a[,i]=data[,(i+1)]}
colnames(a)=c(1:25)
a=ts(a,start=c(1990,1),end=c(2011,8),frequency=12)

###########Break Point###################################################
bp=matrix(NA,ncol=6,nrow=25)
for (i in 1:25){for (j in 1:6){
bp.a=breakpoints(a[,i] ~ 1)
bp[i,j]=breakdates(bp.a)[j]
}}

##########Test trend######################################################
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


##########determine I(.)#################################################
adf=matrix(data=NA,nrow=25,ncol=8)
for (i in 1:25) {
adf1 <- summary(ur.df(a[,i], type = "trend", lags =trunc((length(a[,i])-1)^(1/3))))
adf2 <- summary(ur.df(diff(a[, i],lag=12), type = "trend", lags = trunc((length(a[,i])-1)^(1/3))-1-1))
adf[i,1]=adf1@teststat[1]
adf[i,2:4]=adf1@cval[1,]
adf[i,5]=adf2@teststat[1]
adf[i,6:8]=adf2@cval[1,]}
adf


#########Cointegration###################################################
a.I1=a[,-c(10,14,15,16,17,18)]
c2.1=matrix(data=NA,nrow=19,ncol=57)
for (i in 1:19)
{ for (j in 1:19)
{
lm1=lm(a.I1[,i]~a[,j])
s1.t=adf.test(summary(lm1)$residuals,alternative="stationary")
s2.t=kpss.test(summary(lm1)$residuals)
s3.t=pp.test(summary(lm1)$residuals,lshort=TRUE)
c2.1[j,i]=s1.t$p.value
c2.1[j,i+19]=s2.t$p.value
c2.1[j,i+38]=s3.t$p.value
}}
c2.1

c2.2=matrix(data=NA,nrow=38,ncol=76)
for (i in 1:18){for (j in (i+1):19)
{c=summary(ca.jo(a.I1[,c(i,j)], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=12))
c2.2[(2*i-1):(2*i),(4*j-3)]=t(t(c@teststat))
c2.2[(2*i-1):(2*i),(4*j-2):(4*j)]=c@cval
}}
for (j in 1:18){for (i in (j+1):19)
{c=summary(ca.jo(a.I1[,c(i,j)], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=12))
c2.2[(2*i-1):(2*i),(4*j-3)]=t(t(c@teststat))
c2.2[(2*i-1):(2*i),(4*j-2):(4*j)]=c@cval
}}

c2.3=matrix(data=NA,nrow=38,ncol=19)
for (i in 1:18){for (j in (i+1):19)
{c=summary(ca.jo(a.I1[,c(i,j)], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=12))
c2.3[(2*i-1):(2*i),j]=t(t(c@teststat))
}}
for (j in 1:18){for (i in (j+1):19)
{c=summary(ca.jo(a.I1[,c(i,j)], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=12))
c2.3[(2*i-1):(2*i),j]=t(t(c@teststat))
}}



c2.4=matrix(data=NA,nrow=38,ncol=76)
for (i in 1:18){for (j in (i+1):19)
{p1=VARselect(diff(a.I1[,c(i,j)]),lag.max = 5)
c=summary(ca.jo(a.I1[,c(i,j)], type = "trace", ecdet = "trend", K =max(2,p1$selection[1]),spec = "transitory",season=12))
c2.4[(2*i-1):(2*i),(4*j-3)]=t(t(c@teststat))
c2.4[(2*i-1):(2*i),(4*j-2):(4*j)]=c@cval
}}
for (j in 1:18){for (i in (j+1):19)
{p1=VARselect(diff(a.I1[,c(i,j)]),lag.max = 5)
c=summary(ca.jo(a.I1[,c(i,j)], type = "trace", ecdet = "trend", K =max(2,p1$selection[1]),spec = "transitory",season=12))
c2.4[(2*i-1):(2*i),(4*j-3)]=t(t(c@teststat))
c2.4[(2*i-1):(2*i),(4*j-2):(4*j)]=c@cval}}
}}


#########################On Bivariate Vecm###################################
c2.7=matrix(data=NA,nrow=19,ncol=19)
c2.8=matrix(data=NA,nrow=19,ncol=19)
for (i in 1:18){ for (j in (i+1):19){
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


for (j in 1:18){ for (i in (j+1):19){
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


############First Difference VAR########################################
c2.6=matrix(data=NA,nrow=25,ncol=25)
for (i in 1:24){for (j in (i+1):25){
v=VAR(diff(a[,c(i,j)]),p=2)
c2.6[i,j]=causality(v)$Granger$p.value
}}
for (j in 1:24){for (i in (j+1):25){
v=VAR(diff(a[,c(i,j)]),p=2)
c2.6[i,j]=causality(v)$Granger$p.value
}}


###############12th Difference VAR#######################################
c2.7=matrix(data=NA,nrow=25,ncol=25)
for (i in 1:24){for (j in (i+1):25){
a.12i=diff(a[,i],lag=12)
a.12j=diff(a[,j],lag=12)
v=VAR(cbind(a.12i,a.12j),p=2)
c2.7[i,j]=causality(v)$Granger$p.value
}}
for (j in 1:24){for (i in (j+1):25){
a.12i=diff(a[,i],lag=12)
a.12j=diff(a[,j],lag=12)
v=VAR(cbind(a.12i,a.12j),p=2)
c2.7[i,j]=causality(v)$Granger$p.value
}}

################Structural Break in cointegration########################

c2.8=matrix(data=NA,nrow=19,ncol=1)
for (i in 1:19){
fs=Fstats(a.I1[,i]~a.I1[,-i])
sctest=sctest(fs,type="aveF")
c2.8[i,1]=sctest$p.value}

c2.m=matrix(data=NA,nrow=3,ncol=1)
am=a.I1[,3:5]
for (i in 1:3){
fs=Fstats(am[,i]~am[,-i])
sctest=sctest(fs,type="aveF")
c2.m[i,1]=sctest$p.value}

c2.d=matrix(data=NA,nrow=2,ncol=1)
ad=a.I1[,8:9]
for (i in 1:2){
fs=Fstats(ad[,i]~ad[,-i])
sctest=sctest(fs,type="aveF")
c2.d[i,1]=sctest$p.value}

c2.e=matrix(data=NA,nrow=4,ncol=1)
ae=a.I1[,16:19]
for (i in 1:4){
fs=Fstats(ae[,i]~ae[,-i])
sctest=sctest(fs,type="aveF")
c2.e[i,1]=sctest$p.value}


###############Break Point 2#############################################
bp1=matrix(data=NA,nrow=25,ncol=25)
for (i in 1:24)
{for (j in (i+1):25){
bp.data1=breakpoints(a[,i]~a[,j])
bp1[i,j]=removeNA(summary(bp.data1)$breakdates[1,])[1]
}
}
for (j in 1:24)
{for (i in (j+1):25){
bp.data1=breakpoints(a[,i]~a[,j])
bp1[i,j]=removeNA(summary(bp.data1)$breakdates[1,])[1]
}
}

bp1


##############Break Point3 (Energy and Food)#############################
a.fe=a[,c(4,8,10,14,23)]
bp2=matrix(data=NA,nrow=25,ncol=25)
for (i in 1:24)
{for (j in (i+1):25){

lr.reg=lm(a[,1]~a[,2])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=1990,freq=12),k=-1)
a2=cbind(a[,c(1,2)],diff(a[,c(1,2)]),error.lagged)
a2=window(a2,start=c(1990,2),end=c(2011.7))
colnames(a2)=c("y1","y2","dy1","dy2",error.lagged)
ecm.reg=lm(dy2~error.lagged+dy1)

ocus= efp(ecm.reg, type="OLS-CUSUM", data=a)
me=efp(ecm.reg, type="ME",data=a)
bound.ocus <- boundary(ocus, alpha=0.05)
plot(ocus)

