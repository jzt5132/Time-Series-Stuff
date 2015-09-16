

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