#Stationarity test (First difference,Second difference& Random Component)

#####
adf=matrix(data=NA,nrow=25,ncol=8)
for (i in 1:25) {
adf1 <- summary(ur.df(a[,i], type = "trend", lags =trunc((length(a[,i])-1)^(1/3))))
adf2 <- summary(ur.df(a[, i], type = "drift", lags = trunc((length(a[,i])-1)^(1/3))-1-1))
adf[i,1]=adf1@teststat[1]
adf[i,2:4]=adf1@cval[1,]
adf[i,5]=adf2@teststat[1]
adf[i,6:8]=adf2@cval[1,]}
adf


#####Level (log)

s1=matrix(data=NA,nrow=25,ncol=1)
s2=matrix(data=NA,nrow=25,ncol=1)
s3=matrix(data=NA,nrow=25,ncol=1)
for (i in 1:25)
{
s1.t=adf.test(a[,i],alternative="stationary")
s2.t=kpss.test(a[,i])
s3.t=pp.test(a[,i],lshort=TRUE)
s1[i]=s1.t$p.value
s2[i]=s2.t$p.value
s3[i]=s3.t$p.value
}
stationary.table3=cbind(s1,s2,s3)
t(stationary.table3)



#####First Diff

s1=matrix(data=NA,nrow=25,ncol=1)
s2=matrix(data=NA,nrow=25,ncol=1)
s3=matrix(data=NA,nrow=25,ncol=1)
for (i in 1:25)
{
s1.t=adf.test(diff(a[,i]),alternative="stationary")
s2.t=kpss.test(diff(a[,i]))
s3.t=pp.test(diff(a[,i]))
s1[i]=s1.t$p.value
s2[i]=s2.t$p.value
s3[i]=s3.t$p.value
}
stationary.table1=cbind(s1,s2,s3)
stationary.table1



#####Second Diff

s1=matrix(data=NA,nrow=25,ncol=1)
s2=matrix(data=NA,nrow=25,ncol=1)
s3=matrix(data=NA,nrow=25,ncol=1)
for (i in 1:25)
{
s1.t=adf.test(diff(diff(a[,i])),alternative="stationary")
s2.t=kpss.test(diff(diff(a[,i])))
s3.t=pp.test(diff(diff(a[,i])))
s1[i]=s1.t$p.value
s2[i]=s2.t$p.value
s3[i]=s3.t$p.value
}
stationary.table2=cbind(s1,s2,s3)
t(stationary.table2)



#####Random Component

s1=matrix(data=NA,nrow=25,ncol=1)
s2=matrix(data=NA,nrow=25,ncol=1)
s3=matrix(data=NA,nrow=25,ncol=1)
for (i in 1:25)
{
s1.t=adf.test(removeNA(decompose(a[,i])$random),alternative="stationary")
s2.t=kpss.test(removeNA(decompose(a[,i])$random))
s3.t=pp.test(removeNA(decompose(a[,i])$random))
s1[i]=s1.t$p.value
s2[i]=s2.t$p.value
s3[i]=s3.t$p.value
}
stationary.table3=cbind(s1,s2,s3)
t(stationary.table3)
