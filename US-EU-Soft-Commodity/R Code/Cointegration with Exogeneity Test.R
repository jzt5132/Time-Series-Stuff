###Cointegration of whole data with Exogeneity Test###

#This program determines the cointegration of all time serieses with 
#Exogeneity test and return the result in the H1 matrix.


options(digits = 4)
H1 <- ca.jo(a, type = "trace",ecdet = "trend",season = 52)
summary(H1)
I <- diag(12)
CW <- matrix(data = NA, nrow = 12, ncol = 2)
for (i in 1:12) {
	A1 <- I[,-i]
	CW[i,1] <- summary(alrtest(z = H1,A = A1,r = 2))@teststat
	CW[i,2] <- summary(alrtest(z = H1,A = A1,r = 2))@pval[1]
		}
H1