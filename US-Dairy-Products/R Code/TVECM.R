
#########Threshold Cointegration#####################

options(digits=4)
ThresCI=matrix(data=NA, nrow=8, ncol=8)
for (i in 1:7){
		for (j in (i+1):8){
				b=TVECM.HStest(a[,c(i,j)], lag=1, ngridTh=50, trim=0.05,
				nboot=200, fixed.beta=NULL, intercept=TRUE)
				ThresCI[i,j]=b$PvalBoot
				}
		}
for (j in 1:7){
		for (i in (j+1):8){
				b=TVECM.HStest(a[,c(i,j)], lag=1, ngridTh=50, trim=0.05,
				nboot=200, fixed.beta=NULL, intercept=TRUE)
				ThresCI[i,j]=b$PvalBoot
				}
		}

summary(TVECM.1.7)