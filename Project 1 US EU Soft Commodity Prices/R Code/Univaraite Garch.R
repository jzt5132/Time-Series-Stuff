
###GARCH Coefficient###

#This program determines the GARCH cointegration of all time serieses (univariately)
# and return the result in the ge matrix.

ge <- matrix(data = NA,nrow = 7,ncol = 1)
for (i in 1:7)
{
fit <- garchFit(~garch(1,1),data = (a[,i]))
ge[i,1] <- fit@fit$par[3]
}
ge