#Cointegration test (Second difference)
#####Test Cointegration on Level
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



#####Test Cointegration on First Diff.
c1=matrix(data=NA,nrow=4,ncol=21)

for (i in 1:21)
{ for (j in 22:25)
{
lm1=lm(diff(diff(a[,i]))~diff(diff(a[,j])))
f.stat=summary(lm1)$fstatistic
p.value=1-pf(f.stat["value"],f.stat["numdf"],f.stat["dendf"])
c1[j-21,i]=p.value
}}
c1

c2=matrix(data=NA,nrow=25,ncol=25)

for (i in 1:25)
{ for (j in 1:25)
{
lm1=lm(diff(diff(a[,i]))~diff(diff(a[,j])))
f.stat=summary(lm1)$fstatistic
p.value=1-pf(f.stat["value"],f.stat["numdf"],f.stat["dendf"])
c2[j,i]=p.value
}}
c2

