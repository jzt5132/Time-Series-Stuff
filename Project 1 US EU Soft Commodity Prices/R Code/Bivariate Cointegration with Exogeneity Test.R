

###Bivariate Cointegration with exogeneity test (w/ M2 10year & Exchange rate)###


options(digits = 4)
Crank <- matrix(data = NA, nrow = 6, ncol = 6)
for (i in 1:6){
		for (j in (i+1):7){
				H <- ca.jo(a[,c(i,j,8,10,11)], type = "trace",ecdet = "trend",season = 52)
				if (summary(H)@teststat[5] >= summary(H)@cval[5]){Crank[i,j] <- 1}
				else if (summary(H)@teststat[4] >= summary(H)@cval[4]){Crank[i,j] <- 2}
				else if (summary(H)@teststat[3] >= summary(H)@cval[3]){Crank[i,j] <- 3}	
				else if (summary(H)@teststat[2] >= summary(H)@cval[2]){Crank[i,j] <- 4}	
				else if (summary(H)@teststat[1] >= summary(H)@cval[1]){Crank[i,j] <- 5}
				}
		}
for (j in 1:6){
		for (i in (j+1):7){
				H <- ca.jo(a[,c(i,j,8,10,11)], type = "trace",ecdet = "trend",season = 52)
				if (summary(H)@teststat[5] >= summary(H)@cval[5]){Crank[i,j] <- 1}
				else if (summary(H)@teststat[4] >= summary(H)@cval[4]){Crank[i,j] <- 2}
				else if (summary(H)@teststat[3] >= summary(H)@cval[3]){Crank[i,j] <- 3}	
				else if (summary(H)@teststat[2] >= summary(H)@cval[2]){Crank[i,j] <- 4}	
				else if (summary(H)@teststat[1] >= summary(H)@cval[1]){Crank[i,j] <- 5}
				}
		}
CW1 <- matrix(data = NA,nrow = 35,ncol = 14)
I <- diag(5)
for (i in 1:6){
		for (j in (i+1):7){
				H <- ca.jo(a[,c(i,j,8,10,11)], type = "trace",ecdet = "trend",season = 52)
				for (h in 1:5){
						A1 <- I[,-h]
						CW1[5*(i-1)+h,2*j-1] <- summary(alrtest(z = H,A = A1,r = Crank[i,j]))@teststat
						CW1[5*(i-1)+h,2*j] <- summary(alrtest(z = H,A = A1,r = Crank[i,j]))@pval[1]
						}
					}
		}

CW <- matrix(data = NA, nrow = 5, ncol = 2)
for (h in 1:5) {
	A1 <- I[,-h]
	CW[h,1] <- summary(alrtest(z = H,A = A1,r = Crank[i,j]))@teststat
	CW[h,2] <- summary(alrtest(z = H,A = A1,r = Crank[i,j]))@pval[1]
		}

###Bivariate Cointegration with exogeneity test (w/ 3 months& Exchange rate###

options(digits = 4)
Crank <- matrix(data = 0, nrow = 7, ncol = 7)
for (i in 1:6){
		for (j in (i+1):7){
				H <- ca.jo(a[,c(i,j,9,11)], type = "trace",ecdet = "trend",season = 52)
				if (summary(H)@teststat[4] >= summary(H)@cval[8]){Crank[i,j] <- 1}
				else if (summary(H)@teststat[3] >= summary(H)@cval[7]){Crank[i,j] <- 2}	
				else if (summary(H)@teststat[2] >= summary(H)@cval[6]){Crank[i,j] <- 3}	
				else if (summary(H)@teststat[1] >= summary(H)@cval[5]){Crank[i,j] <- 4}
				}
		}
for (j in 1:6){
		for (i in (j+1):7){
				H <- ca.jo(a[,c(i,j,9,11)], type = "trace",ecdet = "trend",season = 52)
				if (summary(H)@teststat[4] >= summary(H)@cval[8]){Crank[i,j] <- 1}
				else if (summary(H)@teststat[3] >= summary(H)@cval[7]){Crank[i,j] <- 2}	
				else if (summary(H)@teststat[2] >= summary(H)@cval[6]){Crank[i,j] <- 3}	
				else if (summary(H)@teststat[1] >= summary(H)@cval[5]){Crank[i,j] <- 4}
				}
		}
Crank0 <- matrix(data = 0, nrow = 7, ncol = 7)
for (i in 1:6){
		for (j in (i+1):7){
				H <- ca.jo(a[,c(i,j)], type = "trace",ecdet = "trend",season = 52)
				if (summary(H)@teststat[2] >= summary(H)@cval[4]){Crank0[i,j] <- 1}
				else if (summary(H)@teststat[1] >= summary(H)@cval[3]){Crank0[i,j] <- 2}	
				}
		}
for (j in 1:6){
		for (i in (j+1):7){
				H <- ca.jo(a[,c(i,j)], type = "trace",ecdet = "trend",season = 52)			
				if (summary(H)@teststat[2] >= summary(H)@cval[4]){Crank0[i,j] <- 1}
				else if (summary(H)@teststat[1] >= summary(H)@cval[3]){Crank0[i,j] <- 2}	
				}
		}
CW1 <- matrix(data = NA,nrow = 28,ncol = 14)
I <- diag(4)
for (i in 1:6){
		for (j in (i+1):7){
				if(Crank[i,j] > 0){
						H=ca.jo(a[,c(i,j,9,11)], type = "trace",ecdet = "trend",season=52)
						for (h in 1:4){
							A1 <- I[,-h]
							CW1[4*(i-1)+h,2*j-1] <- summary(alrtest(z = H,A = A1,r = Crank[i,j]))@teststat
							CW1[4*(i-1)+h,2*j] <- summary(alrtest(z = H,A = A1,r = Crank[i,j]))@pval[1]
								}
						}
					}
		}
