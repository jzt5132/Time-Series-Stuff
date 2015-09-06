
###First Differenced VAR###

#This program determines the bivariate first difference VAR of all time serieses
#and conduct Granger Causlity Test on it. The output is returned in csl matrix.


csl <- matrix(data = NA,nrow = 7,ncol = 7)
for (i in 1:6)
{ for (j in (i+1):7)
{
v <- VAR(cbind(diff(a[,i]),diff(a[,j])),p = 2)
csl[i,j] <- causality(v)$Granger$p.value
}}
for (j in 1:6)
{ for (i in (j+1):7)
{
v <- VAR(cbind(diff(a[,i]),diff(a[,j])),p = 2)
csl[i,j] <- causality(v)$Granger$p.value
}}
csl