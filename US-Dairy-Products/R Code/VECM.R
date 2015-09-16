
##############Bivariate Cointegration###########################
Crank0=matrix(data=0, nrow=8, ncol=8)
for (i in 1:7){
		for (j in (i+1):8){
				H=ca.jo(a[,c(i,j)], type = "trace")
				if (summary(H)@teststat[2]>=summary(H)@cval[4]){Crank0[i,j]<-1}
				if (summary(H)@teststat[1]>=summary(H)@cval[3]){Crank0[i,j]<-2}
				}				
		}

alpha_beta=matrix(data=NA, nrow=16, ncol=16)
for (i in 1:7){
  for (j in (i+1):8){
        if (Crank0[i,j]>=1){
          H=ca.jo(a[,c(i,j)], type = "trace")
          alpha_beta[2*i-1,2*j-1]=H@W[1,1]
          alpha_beta[2*i-1,2*j]=H@W[2,1]
          alpha_beta[2*i,2*j-1]=H@V[1,1]
          alpha_beta[2*i,2*j]=H@V[2,1]
          
          print(H@V[,1]);print(H@W[,1]);
          print(summary(cajorls(H, r = 1)$rlm))
        }
  }
}


H@W%*%t(H@V)
H@W[1,]%*%t(H@V[1,])
H@W[2,]%*%t(H@V[2,])


H@PI%*%solve(t(H@V))


z=lm(a[,7]~a[,8]-1)