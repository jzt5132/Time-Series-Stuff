
###MTAR and Asymmetry###

#This code inverstigate asymmetry by using MTAR
#(Mulitvariate Threshold VAR)

Asym<-matrix(NA,ncol = 7,nrow = 7)
for (i in 1:6){
		for (j in (i+1):7){
				ciTarlag(a[,j],a[,i],model=C('mtar'),maxlag=12,thresh=0,adjust=TURE)
				}
				