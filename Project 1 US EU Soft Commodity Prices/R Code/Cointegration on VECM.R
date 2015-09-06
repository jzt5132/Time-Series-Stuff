
###Cointegration with trend/seasonal###

#This program determines the bivariate cointegration of all time serieses using VECM
#and return the result in the ci matrix

ci <- matrix(data = NA, nrow = 14, ncol = 28)
for (i in 1:6) { for (j in (i+1):7){
c <- ca.jo(a[,c(i,j)], type = "trace", ecdet = "trend",spec = "longrun",season = 52)

ci[(2*i-1),(4*j-3)] <- summary(c)@teststat[1]
ci[2*i,4*j-3] <- summary(c)@teststat[2]
ci[2*i-1,4*j-2] <- summary(c)@cval[1]
ci[2*i,4*j-2] <- summary(c)@cval[2]
ci[2*i-1,4*j-1] <- summary(c)@cval[3]
ci[2*i,4*j-1] <- summary(c)@cval[4]
ci[2*i-1,4*j] <- summary(c)@cval[5]
ci[2*i,4*j] <- summary(c)@cval[6]

}}

for (j in 1:6) { for (i in (j+1):7){
c <- ca.jo(a[,c(i,j)], type = "trace", ecdet = "trend",spec = "longrun",season = 52)

ci[(2*i-1),(4*j-3)] <- summary(c)@teststat[1]
ci[2*i,4*j-3] <- summary(c)@teststat[2]
ci[2*i-1,4*j-2] <- summary(c)@cval[1]
ci[2*i,4*j-2] <- summary(c)@cval[2]
ci[2*i-1,4*j-1] <- summary(c)@cval[3]
ci[2*i,4*j-1] <- summary(c)@cval[4]
ci[2*i-1,4*j] <- summary(c)@cval[5]
ci[2*i,4*j] <- summary(c)@cval[6]

}}


###Cointegration Rank###

#The code determines bivariate cointegration rank and return the output in Crank0
# matrix.


Crank0 <- matrix(data = 0, nrow = 7, ncol = 7)
for (i in 1:6){
  for (j in (i+1):7){
    H <- ca.jo(a[,c(i,j)], type = "trace")
    if (summary(H)@teststat[2] >= summary(H)@cval[4]){Crank0[i,j] <- 1}
    if (summary(H)@teststat[1] >= summary(H)@cval[3]){Crank0[i,j] <- 2}
  }				
}

alpha_beta <- matrix(data = NA, nrow = 14, ncol = 14)
for (i in 1:6){
  for (j in (i+1):7){
    if (Crank0[i,j] >= 1){
      H <- ca.jo(a[,c(i,j)], type = "trace")
      alpha_beta[2*i-1,2*j-1] <- H@W[1,1]
      alpha_beta[2*i-1,2*j] <- H@W[2,1]
      alpha_beta[2*i,2*j-1] <- H@V[1,1]
      alpha_beta[2*i,2*j] <- H@V[2,1]
      print(H@V[,1]);print(H@W[,1]);
      print(summary(cajorls(H, r = 1)$rlm))
    }
  }
}
