cs1.5=matrix(data=NA,nrow=4,ncol=21)
for (i in 1:21)
{ for (j in 22:25)
{
v=VAR(cbind(diff(diff(a[,j])),diff(diff(a[,i]))),p=5)
cs1.5[j-21,i]=causality(v)$Granger$p.value
}}
cs1.5