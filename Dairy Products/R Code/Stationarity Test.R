##########determine uni. I(.)#################################################
adf=matrix(data=NA,nrow=8,ncol=8)
for (i in 1:8) {
adf1 <- summary(ur.df(a[,i], type = "trend", lags =trunc((length(a[,i])-1)^(1/3))))
adf2 <- summary(ur.df(diff(a[, i]), type = "trend", lags = trunc((length(a[,i])-1)^(1/3))-1-1))
adf[i,1]=adf1@teststat[1]
adf[i,2:4]=adf1@cval[1,]
adf[i,5]=adf2@teststat[1]
adf[i,6:8]=adf2@cval[1,]}
adf