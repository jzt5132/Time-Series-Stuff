chooseCRANmirror()
install.packages("tseries")
install.packages("timeSeries")
install.packages("stats")
install.packages("forecast")
install.packages("MSBVAR")
install.packages("vars")
install.packages("urca")
install.packages("strucchange")
install.packages("pastecs")
install.packages("deseasonalize")
install.packages("fGarch")
install.packages("tsDyn")
install.packages("apt")
install.packages("doSNOW")

library(tseries)
library(timeSeries)
library(stats)
library(forecast)
library(MSBVAR)
library(vars)
library(urca)
library(strucchange)
library(pastecs)
library(deseasonalize)
library(fGarch)
library(tsDyn)
library(apt)
library(doSNOW)
###########Import Data##################################################
data=read.csv(file="data.csv", header=T)
data=ts(data[,(-1)])
name=colnames(data)
a=data=log(data)

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

###########Asymmetry##################################################
Asym<-matrix(NA,ncol=8,nrow=8)
for (i in 1:7){
  for (j in (i+1):8){
    aem <- ecmAsyFit(y=a[,j], x=a[,i],lag=1, model="linear", split=FALSE)
    tes <- ecmAsyTest(aem)$out[1,6]
    Asym[i,j]<-tes
  }
}

for (j in 1:7){
  for (i in (j+1):8){
    aem <- ecmAsyFit(y=a[,j], x=a[,i],lag=1, model="linear", split=FALSE)
    tes <- ecmAsyTest(aem)$out[1,6]
    Asym[i,j]<-tes
  }
}


Asym1<-matrix(NA,ncol=8,nrow=8)
for (i in 1:7){
  for (j in (i+1):8){
    aem <- ecmAsyFit(y=a[,j], x=a[,i],lag=1, model="mtar", split=TRUE)
    tes <- ecmAsyTest(aem)$out[1,6]
    Asym1[i,j]<-tes
  }
}

for (j in 1:7){
  for (i in (j+1):8){
    aem <- ecmAsyFit(y=a[,j], x=a[,i],lag=1, model="mtar", split=TRUE)
    tes <- ecmAsyTest(aem)$out[1,6]
    Asym1[i,j]<-tes
  }
}

###Print details######
for (i in 1:7){
  for (j in (i+1):8){
    if (Asym[i,j]<=0.05){
      aem <- ecmAsyFit(y=a[,j], x=a[,i],lag=1, model="linear", split=FALSE)
      print(aem);
    }
  }
}

for (j in 1:7){
  for (i in (j+1):8){
    if (Asym[i,j]<=0.05){
      aem <- ecmAsyFit(y=a[,j], x=a[,i],lag=1, model="linear", split=FALSE)
      print(aem);
    }
  }
}
